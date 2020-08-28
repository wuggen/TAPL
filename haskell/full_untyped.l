-- Fundamentals
id = \x. x;
fix = \f. (\x. f (\y. x x y)) (\x. f (\y. x x y));

-- Basic higher-order helpers
app = \f. \x. f x; -- equiv id
const = \c. \x. c; -- equiv tru
flip = \f. \y. \x. f x y;

-- Pairs
pair = \f. \s. \b. b f s;
fst = flip app tru;
snd = flip app fls;

-- Church booleans
tru = \t. \f. t;
fls = \t. \f. f;

cand = \b. \c. b c fls;
cor = \b. \c. b tru c;
cxor = \b. \c. b (c fls tru) (c tru fls);
cnot = \b. b fls tru;

test = \b. \m. \n. b m n; -- equiv id

-- Church naturals
c0 = \s.\z. z; -- equiv fls
c1 = \s.\z. s z;
c2 = \s.\z. s (s z);
scc = \a. \s.\z. s (a s z);
prd = \n. fst (
    n
    (\p. pair (snd p) (plus c1 (snd p)))
    (pair c0 c0)
  );

iszro = \n. n (const fls) tru;

cplus = \a. \b. \s.\z. b s (a s z);
ctimes = \a. \b. \s.\z. b (a s) z;
cpow = \n. \m. m (ctimes n) (scc c0);
cminus = \n. \m. m prd n;

cequal = \n. \m. cand (iszro (sub n m)) (iszro (sub m n));

-- Conversions between Church encodings and primitives
rbool = \b. b true false;
cbool = \b. if b then tru else fls;

rnat = \n. n (\x. succ x) 0;
cnat = fix (\cn.
    \n. if iszero n
        then c0
        else scc (cn (pred n))
  );

-- Lists
nil = \c. \n. n; -- equiv fls
cons = \x. \l. \c.\n. c x (l c n);

isnil = \l. l (cand fls) tru;
hd = \l. l const nil;
tl = \l. fst (
    l
    (\x.\p. pair (snd p) (cons x (snd p)))
    (pair nil nil)
  );

-- Non-primitive functions on primitives
not = \b. if b then false else true;
and = \b.\c. if b then c else false;
or = \b.\c. if b then true else c;
xor = \b.\c. if b then (not c) else c;

plus = fix (\plus.
    \n.\m.
        if iszero m
        then n
        else succ (plus n (pred m))
  );
times = fix (\times.
    \n.\m.
        if iszero m
        then 0
        else plus n (times n (pred m))
  );
pow = fix (\pow.
    \n.\m.
        if iszero m
        then 1
        else times n (pow n (pred m))
  );
minus = fix (\minus.
    \n.\m.
        if iszero n
        then 0
        else if iszero m
        then n
        else minus (pred n) (pred m)
  );
