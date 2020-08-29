{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Tapl.Repl
  (
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.Proxy
import System.Console.Haskeline
import Text.Parsec hiding (State)
import Text.Parsec.Char

import Semantics.STS

class (STS s) => ReplLang s where
    type Environment s :: *

    emptyEnv :: Environment s

    parser :: Proxy s -> Parsec String u (State s)

    context :: Proxy s -> Environment s -> Context s

    update :: Environment s -> String -> State s -> Either String (Environment s)

    prepare :: Proxy s -> Environment s -> State s -> Either String (State s)

    render :: State s -> String

    names :: Environment s -> [String]

    constants :: [String]

data EvalMode
  = Step
  | Full
  | List
  deriving (Show, Eq)

data Action s
  = Eval (State s)
  | Bind String (State s)
  | Load String
  | SetMode EvalMode
  | Quit

actionParser :: (ReplLang s) => Proxy s -> Parsec String u (Action s)
actionParser proxy =
        (Eval <$> parser proxy)
    <|> (uncurry Bind <$> bindParser proxy)
    <|> (Load <$> loadParser)
    <|> (SetMode <$> setModeParser)
    <|> (quitParser >> pure Quit)

bindParser :: (ReplLang s) => Proxy s -> Parsec String u (String, State s)
bindParser proxy = do
    spaces
    id <- ident
    spaces
    t <- parser proxy
    spaces
    eof
    return (id, t)

loadParser :: Parsec String u String
loadParser = do
    command "load"
    spaces
    many anyChar

setModeParser :: Parsec String u EvalMode
setModeParser = do
    command "evalmode"
    spaces
    mode <- modeParser
    spaces
    eof
    return mode

quitParser :: Parsec String u ()
quitParser = do
    command "quit"
    spaces
    eof

modeParser :: Parsec String u EvalMode
modeParser = do
    mode <- (string "step" >> pure Step)
        <|> (string "full" >> pure Full)
        <|> (string "list" >> pure List)
    notFollowedBy identContinue
    return mode

identStart :: Parsec String u Char
identStart = letter <|> oneOf "_'"

identContinue :: Parsec String u Char
identContinue = alphaNum <|> oneOf "_'"

ident :: Parsec String u String
ident = (pure <$> identStart) <> many identContinue

command :: String -> Parsec String u String
command cmd = do
    char ':'
    c <- string cmd
    lookAhead space
    return c

data Repl s = Repl {
    replEnv :: Environment s,
    mode :: EvalMode,
    shouldQuit :: Bool
  }

replErr :: (MonadMask m, MonadIO m) => String -> InputT m ()
replErr msg = outputStrLn $ "  [!!] " ++ msg

replEvalResult :: (MonadMask m, MonadIO m) => String -> InputT m ()
replEvalResult res = outputStrLn $ "  -> " ++ res

replAction
  :: (ReplLang s, MonadIO m, MonadMask m)
  => Proxy s
  -> Repl s
  -> Action s
  -> InputT m (Repl s)
replAction proxy replState action =
    case action of

        Eval st -> do
            let env = replEnv replState
                ctx = context proxy env
            case prepare proxy env st of
                Left err -> do
                    replErr err
                    return replState
                Right st -> undefined

replInput
  :: (ReplLang s, MonadIO m, MonadMask m)
  => Proxy s
  -> Repl s
  -> InputT m ()
replInput proxy replState = do
    maybeLine <- getInputLine "% "
    case maybeLine of
        Nothing -> return ()
        Just line -> case parse (actionParser proxy) "input" line of
            Left err -> do
                replErr (show err)
                replInput proxy replState
            Right action -> do
                replState' <- replAction proxy replState action
                if shouldQuit replState'
                  then return ()
                  else replInput proxy replState'

repl :: (ReplLang s) => Proxy s -> IO ()
repl = undefined
