module Zilly where

import Data.Functor
import Control.Applicative
import Control.Monad
import Data.Functor.Identity
import Text.Parsec hiding ((<|>),many)
import Test.HUnit
import Data.Function 
import Data.Maybe (fromMaybe)
type Z = Integer

type Symbol = String



-- reserved words: Z, lazy, if, lt, minus, show

----------------------
-- Types and Data    |
----------------------

-- Done
data T =
      Z                 -- integer type: Z
    | Fun T T           -- function type: <type> -> <type>
    | Lazy T            -- lazy type: lazy <type>
    deriving (Eq, Show)


data E =
      -- done
      Val Z             -- integer values: 0, 1, .., 10, 11, .. 42 ..
      -- done
    | Sym Symbol        -- var names, i.e. user defined symbols: a, b, .. x, y, .. zarzuela, ..
    -- testing pending 
    | Lambda T E E      -- <type> <symbol> -> <exp>: (Z -> Z) x -> minus(x)(minus(0)(1))
    -- testing pending 
    | Apply E E         -- function application <exp>(<exp>): ((Z -> Z) x -> minus(x)(minus(0)(1)))(41)
    -- testing pending
    | If (E, E, E)      -- conditional expression: if(<exp>,<exp>,<exp>): if(lt(x)(y), x, y)
    -- testing pending
    | Defer E           -- deferred expression '<exp>': 'uniform(0)(1)'
    -- testing pending
    | Less E E          -- less than (lt predefined function): lt(x)(y) =<>=> x < y
    -- testing pending
    | Minus E E         -- subtraction (minus predefined function): minus(x)(y) =<>=> x - y
    deriving (Eq,Show)

-- testing pending
data A = 
      Define T E E      -- <type> <symbol> := <expression>;
    | Assign E E        -- <symbol> := <expression>;
    | Magic M
    deriving (Eq,Show)

-- testing pending
data M =
      Show E            -- magic form: .show(<string>, <exp>) prints <string> ==> rvalue(<exp>)
    | ShowEnv           -- magic form: .env 
    deriving (Eq,Show)
type Prog = [A]

type Parser m a = ParsecT String () m a


----------------------
-- Type Parser       |
----------------------

pType :: Monad m => Parser m T
pType = p2
  where
    base = parens pType <|> Z    <$  lexeme' "Z" 
    p1   = prefix (lexeme' "lazy" $> Lazy) base
    p2   = chainr1 p1 (Fun <$ lexeme' "->")

----------------------
-- Expression Parser  |
----------------------


pVal :: (Monad m) => Parser m E
pVal = Val <$> lexeme pInteger

pSym :: (Monad m) => Parser m E
pSym = Sym <$> pVarName

pLambda :: Monad m => Parser m E -> Parser m E
pLambda  = prefix (Lambda <$> pType <*> pSym <* lexeme' "->")

pApply :: Monad m => Parser m E -> Parser m E
pApply p = postfix (flip Apply <$> parens p) p

pIf :: Monad m => Parser m E -> Parser m E
pIf expr = lexeme' "if" >> If <$> parens ((,,) <$> expr <*> (char' ',' *> expr) <*> (char' ',' *> expr))

pDefer :: Monad m => Parser m a -> Parser m a
pDefer = between pQuote pQuote 
  where
  pQuote = char' '\''

pLess :: Monad m => Parser m E -> Parser m E
pLess p = lexeme' "lt" *> (Less <$> parens p <*> parens p)

pMinus :: Monad m => Parser m E -> Parser m E
pMinus p = lexeme' "minus" >> Minus <$> parens p <*> parens p

pExpression :: Monad m => Parser m E
pExpression =  p3
  where 
    base = parens pExpression
      <|> pVal
      <|> pSym
    p1 = asum $ ($ base) <$> [pIf,pDefer,pLess,pMinus, id]
    p2 = pApply p1
    p3 = pLambda p2 

----------------------
-- Action Parser     |
----------------------

pDefine :: Monad m => Parser m A
pDefine = Define <$> pType <*> pSym <*> (lexeme' ":=" *> pExpression)

pAssign :: Monad m => Parser m A
pAssign = Assign <$> pSym <*> (lexeme' ":=" *> pExpression)

pMagicA :: Monad m => Parser m A
pMagicA = Magic <$> pMagic

pAction :: Monad m => Parser m A
pAction = pMagicA <|> pDefine <|> pAssign 


----------------------
-- Magic Parser      |
----------------------

pShow :: Monad m => Parser m M
pShow = lexeme' ".show" *> parens (Show <$> pExpression)

pShowEnv :: Monad m => Parser m M
pShowEnv = ShowEnv <$ lexeme' ".env"

pMagic :: Monad m => Parser m M
pMagic = pShow <|> pShowEnv


----------------------
-- Parser utilities  |
----------------------

prefix :: Monad m => Parser m (a -> a) -> Parser m a -> Parser m a
prefix op p = op <*> prefix  op p <|> p


postfix :: Monad m => Parser m (a -> a) -> Parser m a -> Parser m a
postfix  op p =  p <**> pipeline
  where 
    pipeline = pure (flip (.) ) <*> op <*> pipeline <|> pure id 


reservedWords :: [String]
reservedWords = ["Z","lazy","if","lt","minus","show"]

lexeme :: Monad m => Parser m a -> Parser m a
lexeme p = p <* spaces

lexeme' :: Monad m => String -> Parser m String
lexeme' p = string' p <* spaces

char' :: Monad m => Char -> Parser m Char
char' c = char c <* spaces

parens :: Monad m => Parser m a -> Parser m a
parens = between (char' '(') (char' ')')

pReserved :: (Monad m) => Parser m String
pReserved = asum (string' <$> reservedWords) 

failReserved :: (Monad m) => (String -> String) -> Parser m ()
failReserved errMsgHandler = notFollowedBy pReserved


pInteger :: (Read a, Monad m) => Parser m a
pInteger = read <$> many1 digit

pVarName :: (Monad m) => Parser m Symbol
pVarName = failReserved errMsgHandler >>  pVarName' <* spaces
  where
  pVarName' = liftA2 mappend pInitial $ fromMaybe "" <$> optionMaybe pBody
  pInitial  = pure <$> asum [char '_',letter]
  pBody     = many1 $ alphaNum <|> char '_'
  errMsgHandler = mappend "Variable names cannot be reserved words. Found: " . show

parse' :: Parser Identity a -> String -> Either ParseError a
parse' p = runParser p () ""

fully :: Monad m => Parser m a -> Parser m a
fully p = spaces *> p <* eof

{-
    Programita de ejemplo:

    [
        Z -> Z chs := Z x -> minus(0)(x);
        Z -> (Z -> Z) plus := (Z x -> (Z y -> minus(x)(chs(y))));
        Z x := 7;
        Z y := 8;
        Z q := plus(x)(y);
        show(q);                        -- imprime: q ==> 15
        lazy Z z := 'plus(x)(y)'
        show(z);                        -- imprime: z ==> 15
        x := plus(x)(1);
        show(q);                        -- imprime: q ==> 15
        show(z);                        -- imprime: z ==> 16
    ]
-}

----------------------
-- Test              |
----------------------

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

testType :: IO Counts
testType = runTestTT . TestList $
  [ "Parsing Z" 
    ~: Right Z         ~=? parse' (fully pType) "Z"
  , "Parsing Lazy Z" 
    ~: Right (Lazy Z)  ~=? parse' (fully pType) "lazy Z"
  , "Parsing Z -> Z as Fun Z Z" 
    ~: Right (Fun Z Z) ~=? parse' (fully pType) "Z -> Z"
  , "Parsing Z->Z as Fun Z Z" 
    ~: Right (Fun Z Z) ~=? parse' (fully pType) "Z->Z"
  , "Parsing Z->        Z as Fun Z Z" 
    ~: Right (Fun Z Z) ~=? parse' (fully pType) "Z->        Z"
  , "Parsing (Z -> Z) as Fun Z Z" 
    ~: Right (Fun Z Z) ~=? parse' (fully pType) "(Z -> Z)"
  , "Parsing Z -> Z -> Z as Fun Z (Fun Z Z)" 
    ~: Right (Fun Z (Fun Z Z)) ~=? parse' (fully pType) "Z -> Z -> Z"
  , "Parsing Z -> (Z -> Z) as Fun Z (Fun Z Z)" 
    ~: Right (Fun Z (Fun Z Z)) ~=? parse' (fully pType) "Z -> (Z -> Z)"
  , "Parsing (lazy Z) -> Z as Fun (lazy Z) Z" 
    ~: Right (Fun (Lazy Z) Z) ~=? parse' (fully pType) "(lazy Z) -> Z"
  , "Parsing lazy Z -> Z as Fun (lazy Z) Z" 
    ~: Right (Fun (Lazy Z) Z) ~=? parse' (fully pType) "lazy Z -> Z"
  , "Parsing lazy Z -> lazy Z as Fun (lazy Z) (lazy Z)" 
    ~: Right (Fun (Lazy Z) (Lazy Z)) ~=? parse' (fully pType) "lazy Z -> lazy Z"
  , "Parsing lazy Z -> (lazy Z) as Fun (lazy Z) (lazy Z)" 
    ~: Right (Fun (Lazy Z) (Lazy Z)) ~=? parse' (fully pType) "lazy Z -> (lazy Z)"
  , "Parsing (Z -> Z) -> (Z -> Z) -> Z as Fun (Fun Z Z) (Fun (Fun Z Z) Z)" 
    ~: Right (Fun (Fun Z Z) (Fun (Fun Z Z) Z)) ~=? parse' (fully pType) "(Z -> Z) -> (Z -> Z) -> Z"
  , "Parsing (Z -> Z) -> ((Z -> Z) -> Z) as Fun (Fun Z Z) (Fun (Fun Z Z) Z)" 
    ~: Right (Fun (Fun Z Z) (Fun (Fun Z Z) Z)) ~=? parse' (fully pType) "(Z -> Z) -> ((Z -> Z) -> Z)"
  , "Parsing (Z -> Z) -> lazy ((Z -> Z) -> Z) as Fun (Fun Z Z)  (Lazy (Fun (Fun Z Z) Z))" 
    ~: Right (Fun (Fun Z Z) (Lazy (Fun (Fun Z Z) Z))) ~=? parse' (fully pType) "(Z -> Z) -> lazy ((Z -> Z) -> Z)"
  , "Parsing Z -> Should be Left" 
    ~: isLeft (parse' (fully pType) "Z -> ") ~? "Z -> Should throw an error"
  ]

testVal :: IO Counts
testVal = runTestTT . TestList $
  [ "Parsing 0 should be Val 0"   ~: Right (Val 0) ~=? parse' (fully pVal) "0"
  , "Parsing 44 should be Val 44" ~: Right (Val 44) ~=? parse' (fully pVal) "44"
  , "Parsing -1 should be Left"   ~: isLeft ( parse' (fully pVal) "-1") ~? "-1 Should throw an error"
  , "Parsing 1.0 should be Left"  ~: isLeft ( parse' (fully pVal) "1.0") ~? "1.0 Should throw an error"
  , "Parsing 1.9 should be Left"  ~: isLeft ( parse' (fully pVal) "1.9") ~? "1.9 Should throw an error"
  , "Parsing .9 should be Left"   ~: isLeft ( parse' (fully pVal) ".9") ~? ".9 Should throw an error"
  , "Parsing 1e3 should be Left"  ~: isLeft ( parse' (fully pVal) "1e3") ~? "1e3 Should throw an error"
  , "Parsing 1e-3 should be Left" ~: isLeft ( parse' (fully pVal) "1e-3") ~? "1e-3 Should throw an error"
  ]


testSym :: IO Counts
testSym = runTestTT . TestList $
  [ "Parsing _ should be Sym _" 
    ~: Right (Sym "_") ~=? parse' (fully pSym) "_"
  , "Parsing a should be Sym a" 
    ~: Right (Sym "a") ~=? parse' (fully pSym) "a"
  , "Parsing X should be Sym X" 
    ~: Right (Sym "X") ~=? parse' (fully pSym) "X"
  , "Parsing ä should be Sym ä" 
    ~: Right (Sym "ä") ~=? parse' (fully pSym) "ä"
  , "Parsing Á should be Sym Á" 
    ~: Right (Sym "Á") ~=? parse' (fully pSym) "Á"
  , "Parsing _12A3aÁXσ should be Sym _12A3aÁXσ" 
    ~: Right (Sym "_12A3aÁXσ") ~=? parse' (fully pSym) "_12A3aÁXσ"
  , "Parsing a12A3aÁXσ should be Sym a12A3aÁXσ" 
    ~: Right (Sym "a12A3aÁXσ") ~=? parse' (fully pSym) "a12A3aÁXσ"
  , "Parsing X12A3aÁXσ should be Sym X12A3aÁXσ" 
    ~: Right (Sym "X12A3aÁXσ") ~=? parse' (fully pSym) "X12A3aÁXσ"
  , "Parsing ä12A3aÁXσ should be Sym ä12A3aÁXσ" 
    ~: Right (Sym "ä12A3aÁXσ") ~=? parse' (fully pSym) "ä12A3aÁXσ"
  , "Parsing Á12A3aÁXσ should be Sym Á12A3aÁXσ" 
    ~: Right (Sym "Á12A3aÁXσ") ~=? parse' (fully pSym) "Á12A3aÁXσ"
  , "Parsing '  Á12A3aÁXσ   ' should be Sym Á12A3aÁXσ" 
    ~: Right (Sym "Á12A3aÁXσ") ~=? parse' (fully pSym) "  Á12A3aÁXσ   "
  , "Parsing 1 Should be Left" 
    ~: isLeft (parse' (fully pSym) "1") ~? "1 Should throw an error"
  , "Parsing 1_ Should be Left" 
    ~: isLeft (parse' (fully pSym) "1_") ~? "1_ Should throw an error"
  , "Parsing 1a_b Should be Left" 
    ~: isLeft (parse' (fully pSym) "1a_b") ~? "1a_b Should throw an error"
  ]

testFailReserve :: IO Counts
testFailReserve = runTestTT . TestList $
  reservedWords <&> \reservedWord ->
    concat ["Parsing ", reservedWord, " should be left"]
    ~: isLeft (parse' (fully pSym) reservedWord) ~? concat [reservedWord," Should throw an error"]

testLambda :: IO Counts
testLambda = runTestTT . TestList $
  [ "Parsing: Z x -> x should be Lambda Z (Sym x) (Sym x)"
    ~: Right (Lambda Z (Sym "x") (Sym "x")) ~=? parse' (fully pExpression) "Z x -> x"
  , "Parsing: Z x -> Z y -> minus(x)(y) should be Lambda Z (Sym x) (Lambda Z (Minus (Sym x) (Sym y)))"
    ~: Right (Lambda Z (Sym "x") (Lambda Z (Sym "y") (Minus (Sym "x") (Sym "y")))) ~=? parse' (fully pExpression) "Z x -> Z y -> minus(x)(y)"
  , "Parsing: Z x -> (Z -> Z) f -> f(x) should be Lambda Z (Sym x) (Lambda (Fun Z Z) (Sym f) (Apply (Sym f) (Sym x)))"
    ~: Right (Lambda Z (Sym "x") (Lambda (Fun Z Z) (Sym "f") (Apply (Sym "f") (Sym "x")))) ~=? parse' (fully pExpression) "Z x -> (Z -> Z) f -> f(x)"
  ]

testMinus :: IO Counts
testMinus = runTestTT . TestList $
  [ "Parsing minus(x)(y) should be Minus (Sym x) (Sym y)"
    ~: Right (Minus (Sym "x") (Sym "y")) ~=? parse' (fully pExpression) "minus(x)(y)"
  ]


testAll :: IO ()
testAll = do
  putStrLn "-------------------------"
  putStrLn "testType"
  putStrLn "-------------------------"
  print =<< testType

  putStrLn "-------------------------"
  putStrLn "testVal"
  putStrLn "-------------------------"
  print =<< testVal

  putStrLn "-------------------------"
  putStrLn "testSym"
  putStrLn "-------------------------"
  print =<< testSym

  putStrLn "-------------------------"
  putStrLn "testFailReserve"
  putStrLn "-------------------------"
  print =<< testFailReserve

  putStrLn "-------------------------"
  putStrLn "testLambda"
  putStrLn "-------------------------"
  print =<< testLambda

  putStrLn "-------------------------"
  putStrLn "testMinus"
  putStrLn "-------------------------"
  print =<< testMinus