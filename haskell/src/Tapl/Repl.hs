{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Tapl.Repl
  ( ReplFuncs(..)
  , repl
  ) where

import Prelude hiding (lex)

import Control.Monad (mapM_)
import Control.Monad.Catch (MonadMask, MonadCatch)
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State.Lazy as S
import Control.Monad.Trans.State.Lazy (modify, gets)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import System.Console.Haskeline
import System.Console.Haskeline.Completion
import Text.Parsec hiding (State)
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Token

import Semantics.STS

data EvalMode
  = EMStep
  | EMList
  | EMFull
  deriving (Show, Eq, Ord)

data ReplFuncs e t s = ReplFuncs
  { rfSystemName :: String
  , rfBind :: Maybe (String -> State s -> e -> e)
  , rfClearEnv :: Maybe (e -> e)
  , rfNames :: e -> [String]
  , rfPrepare :: e -> t -> Either String (State s)
  , rfContext :: e -> Context s
  , rfParser :: Parsec String () t
  , rfRender :: e -> State s -> String
  }

data ReplState e t s = ReplState
  { rsFuncs :: ReplFuncs e t s
  , rsEnv :: e
  , rsPrevRes :: Maybe (State s)
  , rsMode :: EvalMode
  , rsDebug :: Bool
  }

rsAskEnv :: (e -> a) -> ReplState e t s -> a
rsAskEnv ask = ask . rsEnv

rsModEnv :: (e -> e) -> ReplState e t s -> ReplState e t s
rsModEnv mod st = st { rsEnv = mod (rsEnv st) }

rsSetMode :: EvalMode -> ReplState e t s -> ReplState e t s
rsSetMode mode st = st { rsMode = mode }

rsSetPrevRes :: Maybe (State s) -> ReplState e t s -> ReplState e t s
rsSetPrevRes res st = st { rsPrevRes = res }

rsSetDebug :: Bool -> ReplState e t s -> ReplState e t s
rsSetDebug dbg st = st { rsDebug = dbg }

rsFunc :: (ReplFuncs e t s -> a) -> ReplState e t s -> a
rsFunc proj = proj . rsFuncs

data Command t
  = CEval t
  | CBind String t
  | CDebug
  | CSetMode EvalMode
  | CLoad String
  | CClearEnv
  | CQuit

fileLangDef :: LanguageDef u
fileLangDef = emptyDef
  { commentLine = "--"
  , identStart = letter <|> oneOf "_'"
  , identLetter = alphaNum <|> oneOf "_'"
  }

TokenParser
  { identifier = ident
  , whiteSpace = whitespace
  , lexeme = lex
  } = makeTokenParser fileLangDef

commandName :: String -> Parsec String u ()
commandName cmd = do
    char ':'
    lex $ string cmd
    return ()

evalParser :: Parsec String u t -> Parsec String u t
evalParser stateParser = do
    whitespace; t <- stateParser
    whitespace; eof
    return t

bindParser :: Parsec String u t -> Parsec String u (String, t)
bindParser stateParser = do
    whitespace
    id <- ident
    lex $ char '='
    st <- lex stateParser
    eof
    return (id, st)

toggleDebugParser :: Parsec String u ()
toggleDebugParser = commandName "debug" >> eof

setModeParser :: Parsec String u EvalMode
setModeParser = do
    commandName "mode"
    em <- (string "step" >> return EMStep)
      <|> (string "list" >> return EMList)
      <|> (string "full" >> return EMFull)
    spaces; eof
    return em

loadParser :: Parsec String u String
loadParser = do
    commandName "load"
    filename <- many anyChar
    eof
    return (trim filename)
  where
    trim s = reverse (dropWhile isSpace (reverse s))

clearEnvParser :: Parsec String u ()
clearEnvParser = commandName "clearenv" >> eof

fileParser :: Parsec String u t -> Parsec String u [(String, t)]
fileParser stateParser = do
    whitespace
    bindings <- endBy1 binding semi
    eof
    return bindings
  where
    binding = do
        id <- ident
        lex $ char '='
        st <- lex stateParser
        return (id, st)
    semi = lex $ char ';'

quitParser :: Parsec String u ()
quitParser = do { commandName "quit"; spaces; eof }

commandParser :: Parsec String u t -> Parsec String u (Command t)
commandParser stateParser = do
    try (CEval <$> evalParser stateParser)
    <|> try (uncurry CBind <$> bindParser stateParser)
    <|> try (toggleDebugParser >> return CDebug)
    <|> try (CSetMode <$> setModeParser)
    <|> try (CLoad <$> loadParser)
    <|> try (clearEnvParser >> return CClearEnv)
    <|> (quitParser >> return CQuit)

type ReplBase e t s = S.StateT (ReplState e t s)
type Repl e t s m = InputT (ReplBase e t s m)

replFunc :: (Monad m) => (ReplFuncs e t s -> a) -> Repl e t s m a
replFunc proj = lift $ gets (proj . rsFuncs)

replEnv :: (Monad m) => Repl e t s m e
replEnv = lift (gets rsEnv)

replModEnv :: (Monad m) => (e -> e) -> Repl e t s m ()
replModEnv = lift . modify . rsModEnv

replMode :: (Monad m) => Repl e t s m EvalMode
replMode = lift (gets rsMode)

replPrevRes :: (Monad m) => Repl e t s m (Maybe (State s))
replPrevRes = lift (gets rsPrevRes)

replDoDebug :: (Monad m) => Repl e t s m Bool
replDoDebug = lift (gets rsDebug)

replSetDebug :: (Monad m) => Bool -> Repl e t s m ()
replSetDebug = lift . modify . rsSetDebug

replSetPrevRes :: (Monad m) => Maybe (State s) -> Repl e t s m ()
replSetPrevRes = lift . modify . rsSetPrevRes

replContext :: (Monad m) => Repl e t s m (Context s)
replContext = do
    env <- replEnv
    mkContext <- replFunc rfContext
    return (mkContext env)

replPrepare :: (Monad m) => t -> Repl e t s m (Either String (State s))
replPrepare init = do
    env <- replEnv
    prep <- replFunc rfPrepare
    return (prep env init)

replError :: (MonadIO m, MonadMask m) => String -> Repl e t s m ()
replError msg = outputStr $ unlines $ map ("  [!!] " ++) (lines msg)

replEvalInit :: (MonadIO m, MonadMask m) => String -> Repl e t s m ()
replEvalInit res = outputStrLn $ "     " ++ res

replEvalResult :: (MonadIO m, MonadMask m) => String -> Repl e t s m ()
replEvalResult res = outputStrLn $ "  -> " ++ res

replNotice :: (MonadIO m, MonadMask m) => String -> Repl e t s m ()
replNotice msg = outputStrLn $ "   > " ++ msg

replDebug :: (MonadIO m, MonadMask m, Show d) => d -> Repl e t s m ()
replDebug msg = do
    doDbg <- replDoDebug
    if doDbg
      then outputStrLn $ "    [DEBUG] " ++ show msg
      else return ()

replEval :: (STS s, MonadIO m, MonadMask m, Show (State s)) => State s -> Repl e t s m ()
replEval init = do
    render <- replFunc rfRender
    env <- replEnv
    ctx <- replContext
    mode <- replMode

    handleInterrupt (replError "Interrupted")
        (withInterrupt $ case mode of
            EMStep -> case evalStep ctx init of
                Left err -> do
                    replError (show err)
                    replSetPrevRes Nothing
                Right res -> do
                    replDebug res
                    replEvalResult (render env res)
                    replSetPrevRes (Just res)
            EMList -> do
                let ((first : rest), maybeFailure) = evalList ctx init
                replEvalInit (render env first)
                mapM_ (\res -> replDebug res >> replEvalResult (render env res)) rest
                case maybeFailure of
                    Nothing -> return ()
                    Just failure -> replError (show failure)
            EMFull -> case eval ctx init of
                Left err -> replError (show err)
                Right res -> do
                    replDebug res
                    replEvalResult $ render env res)

replBind :: (STS s, MonadIO m, MonadMask m) => String -> t -> Repl e t s m ()
replBind name term = do
    replSetPrevRes Nothing
    bind <- replFunc rfBind
    case bind of
        Nothing -> replError "This system does not support name binding"
        Just bind -> do
            term' <- replPrepare term
            case term' of
                Left err -> replError (show err)
                Right term' -> replModEnv (bind name term')

replToggleDebug :: (MonadIO m, MonadMask m) => Repl e t s m ()
replToggleDebug = do
    dbg <- replDoDebug
    let dbg' = not dbg
    replSetDebug dbg'
    replNotice $ "Debug output " ++ if dbg' then "on" else "off"

replSetMode :: (Monad m) => EvalMode -> Repl e t s m ()
replSetMode = lift . modify . rsSetMode

replReadFile :: (MonadCatch m, MonadIO m) => String -> Repl e t s m (Either IOError String)
replReadFile = try' . readFile
  where
    try' = lift . lift . liftIO . Catch.try

replLoad :: (STS s, MonadIO m, MonadMask m) => String -> Repl e t s m ()
replLoad filename = do
    stateParser <- replFunc rfParser
    readRes <- replReadFile filename
    case readRes of
        Left err -> replError (show err)
        Right contents -> case parse (fileParser stateParser) filename contents of
            Left err -> replError (show err)
            Right bindings -> do
                mapM_ (uncurry replBind) bindings
                replNotice $ filename ++ " loaded"

replClearEnv :: (MonadIO m, MonadMask m) => Repl e t s m ()
replClearEnv = do
    clear <- replFunc rfClearEnv
    case clear of
        Nothing -> replError "This system does not support clearing the environment"
        Just clear -> do
            replSetPrevRes Nothing
            replModEnv clear
            replNotice "Environment cleared"

replCommand :: (STS s, MonadIO m, MonadMask m, Show t, Show (State s)) => Command t -> Repl e t s m Bool
replCommand = \case
    CEval init -> do
        replDebug $ "eval: " ++ show init
        init' <- replPrepare init
        replDebug $ "prepared form: " ++ show init'
        case init' of
            Left err -> replError err
            Right init' -> replEval init'
        return False
    CBind name term -> do
        replDebug $ "bind: " ++ name ++ " = " ++ show term
        replBind name term
        return False
    CSetMode mode -> do
        replDebug $ "mode: " ++ show mode
        replSetMode mode
        return False
    CDebug -> replToggleDebug >> return False
    CLoad filename -> replLoad filename >> return False
    CClearEnv -> replClearEnv >> return False
    CQuit -> pure True

replMain :: (STS s, MonadIO m, MonadMask m, Show t, Show (State s)) => Repl e t s m ()
replMain = do
    parser <- replFunc rfParser
    name <- replFunc rfSystemName
    line <- getInputLine (name ++ "# ")
    case line of
        Nothing -> return ()
        Just "" -> do
            mode <- replMode
            prevRes <- replPrevRes
            case (mode, prevRes) of
                (EMStep, Just res) -> replEval res
                _ -> pure ()
            replMain
        Just line -> do
            quit <- case parse (commandParser parser) "input" line of
                Left err -> replError (show err) >> pure False
                Right cmd -> replCommand cmd
            if quit then return () else replMain

completeNames :: (Monad m) => CompletionFunc (ReplBase e t s m)
completeNames = completeWord Nothing " \t\n()\\." namesStartingWith
  where
    namesStartingWith prefix = do
        env <- gets rsEnv
        allNames <- gets (rfNames . rsFuncs)
        let names = filter (prefix `isPrefixOf`) (allNames env)
        return $ map simpleCompletion names

completeModes :: (Monad m) => CompletionFunc (ReplBase e t s m)
completeModes = completeWord Nothing " \t\n" modesStartingWith
  where
    modesStartingWith prefix =
      let modes = filter (prefix `isPrefixOf`) ["full", "list", "step"]
      in return $ map simpleCompletion modes

completeCommands :: (Monad m) => CompletionFunc (ReplBase e t s m)
completeCommands = completeWord Nothing ":" commandsStartingWith
  where
    commandsStartingWith prefix =
      let cmds = filter (prefix `isPrefixOf`) ["mode", "load", "clearenv", "debug"]
      in return $ map simpleCompletion cmds

replCompletion :: (MonadIO m) => CompletionFunc (ReplBase e t s m)
replCompletion (init, tail) = do
    let init' = reverse init
        initWords = words init'
    case initWords of
        ":mode" : _ -> completeModes (init, tail)
        ":load" : _ -> completeFilename (init, tail)
        [ ':' : _ ] -> completeCommands (init, tail)
        (':' : _) : _ -> return ("", [])
        _ -> completeNames (init, tail)

repl :: (STS s, Show t, Show (State s)) => ReplFuncs e t s -> e -> IO ()
repl funcs initEnv = S.evalStateT (runInputT (setComplete replCompletion defaultSettings) replMain) initState
  where
    initState = ReplState
      { rsFuncs = funcs
      , rsEnv = initEnv
      , rsMode = EMFull
      , rsPrevRes = Nothing
      , rsDebug = False
      }
