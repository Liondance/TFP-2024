module ZTest2 where

import Debug.Trace

-- Definitions
import Zilly (Z,Symbol,T(..),E(..),Statement(..),Program, mkParens, rvalue, exec, State, empty)

-- Useful for parsing
import Data.Functor
import Control.Applicative hiding (optional, empty)
import Control.Monad
import Data.Functor.Identity
import Text.Parsec hiding ((<|>),many,State(..))
import Data.Maybe (fromMaybe)

-- Testing
import Test.HUnit hiding(State)

type Programs = [Program]
type Parser m a = ParsecT String () m a

----------------------
-- Type Parser       |
----------------------

-- | Parses a type
pType :: Monad m => Parser m T
pType = p2
  where
    base = parens pType <|> Z    <$  lexeme' "Z"
    p1   = prefix (lexeme' "lazy" $> Lazy) base
    p2   = chainr1 p1 (Fun <$ lexeme' "->")

-- | Parses a type between '<' '>'
pType' :: Monad m => Parser m T
pType' = between (char' '<') (char' '>') pType

----------------------
-- Expression Parser  |
----------------------

-- | Parses an integer
pVal :: (Monad m) => Parser m E
pVal = Val <$> lexeme pInteger


-- | Parses a symbol by the rules of `pVarName`
pSym :: (Monad m) => Parser m E
pSym = Sym <$> pVarName

-- | Parses a (possibly chain of) lambda
pLambda :: Monad m => Parser m E -> Parser m E
pLambda =  prefix (Lambda <$> pType' <*>  pSym <* lexeme' "->")

-- | Parses a (possibly chain of) function applications
pApply :: Monad m => Parser m E -> Parser m E -> Parser m E
pApply p q = postfix (flip Apply <$> parens q) p

-- | Parses an if expression
pIf :: Monad m => Parser m E -> Parser m E
pIf expr = lexeme' "if" >> If <$> parens ((,,) <$> expr <*> (char' ',' *> expr) <*> (char' ',' *> expr))

-- | Parses a deferred expression
pDefer :: Monad m => Parser m E -> Parser m E
pDefer = fmap Defer . between pQuote pQuote
  where
  pQuote = char' '\''

-- | Parses a 'less than' function application
pLess :: Monad m => Parser m E -> Parser m E
pLess p = lexeme' "lt" *> (Less <$> parens p <*> parens p)

-- | Parses a 'minus' function application
pMinus :: Monad m => Parser m E -> Parser m E
pMinus p = lexeme' "minus" >> Minus <$> parens p <*> parens p

-- | Parses a 'formula' function application
pFormula :: Monad m => Parser m E -> Parser m E
pFormula p = lexeme' "formula" >> Formula <$> parens p

-- | Parses an expression
pExpression :: Monad m => Parser m E
pExpression =  p2
  where
    base = pDefer pExpression
      <|> parens pExpression
      <|> pVal
      <|> pSym
    p1 = asum
      [ pLess pExpression
      , pFormula pExpression
      , pMinus pExpression
      , pIf pExpression
      , pApply base pExpression
      ]
    p2 = pLambda p1



----------------------
-- Action Parser     |
----------------------

-- | Parses a 'Halt' action
pHalt :: Monad m => Parser m Statement
pHalt = lexeme' "halt" *> manyTill (pComment <|> void anyChar) (lookAhead $ char ']') $> Halt -- *> (mzero <?> "Halt action issued")

-- | Parses a 'Define' action
pDefine :: Monad m => Parser m Statement
pDefine = Define <$> pType' <*> (pSym <?> "Only variables support assignment") <*> (lexeme' ":=" *> pExpression)

-- | Parses an 'Assign' action
pAssign :: Monad m => Parser m Statement
pAssign = Assign <$> pSym <*> (lexeme' ":=" *> pExpression)


-- | Parses a 'show' magic command
pShow :: Monad m => Parser m Statement
pShow = lexeme' ".show" *> parens (Show <$> (pString <* char' ',') <*> pExpression)
  where
    pString = between (char' '"') (char' '"') (many $ noneOf ['"'] )

-- | Parses any 'Action'.
pAction :: Monad m => Parser m Statement
pAction = pHalt <|> pShow <|> pDefine <|> pAssign




----------------------
-- Parser utilities  |
----------------------

-- | Parses any 0 or more occurences of a prefix operator  followed by another parser.
prefix :: Monad m => Parser m (a -> a) -> Parser m a -> Parser m a
prefix op p = op <*> prefix  op p <|> p


-- | Parses the given parser followed by 0 or more occurences of a prefix operator.
postfix :: Monad m => Parser m (a -> a) -> Parser m a -> Parser m a
postfix  op p =  p <**> pipeline
  where
    pipeline = pure (flip (.) ) <*> op <*> pipeline <|> pure id

-- | Statement list of reserved words
reservedWords :: [String]
reservedWords = ["Z","lazy","if","lt","minus","show","formula"]

-- | Statement way of making a parser a lexeme
lexeme :: Monad m => Parser m a -> Parser m a
lexeme p = p <* spaces

-- | Statement way of making a string a lexeme
lexeme' :: Monad m => String -> Parser m String
lexeme' p = string' p <* spaces

-- | As `char` but consumming spaces
char' :: Monad m => Char -> Parser m Char
char' c = char c <* spaces

-- | Parses the given parser between parentheses
parens :: Monad m => Parser m a -> Parser m a
parens = between (char' '(') (char' ')')

-- | Parses a reserved word
pReserved :: (Monad m) => Parser m String
pReserved = asum (string' <$> reservedWords)

-- | Fails when parses a reverved word
failReserved :: (Monad m) => Parser m ()
failReserved = notFollowedBy pReserved

-- | Parses an integral number
pInteger :: (Read a, Monad m) => Parser m a
pInteger = read <$> many1 digit

{-| Parses a symbol such that:

  - It isn't a reserved word
  - It begins with either '_' or a letter
  - It optionally ends with any combination of numbers, letters and '_'
-}
pVarName :: (Monad m) => Parser m Symbol
pVarName = (failReserved <?> "An identifier which isn't a reserved word") >>  pVarName' <* spaces
  where
  pVarName' = liftA2 mappend pInitial $ fromMaybe "" <$> optionMaybe pBody
  pInitial  = pure <$> asum [char '_',letter]
  pBody     = many1 $ alphaNum <|> char '_'

-- | Fully runs the given parser, making sure it consumes everything.
fully :: Monad m => Parser m a -> Parser m a
fully p = spaces *> p <* eof

-- | Statement shorthand for running a parser
parse' :: Parser Identity a -> String -> Either ParseError a
parse' p  = parse p ""



------------------------------
-- Main Parsers And functions |
------------------------------

pComment :: Monad m => Parser m ()
pComment = string' "--" >> manyTill anyChar endOfLine >> spaces $> ()

-- | Parses a single program. 
pProgram :: Monad m => Parser m Program
pProgram = between open close pProgram'
  where
    pProgram' = pComment *> pProgram'
      <|> sepEndBy pAction (char' ';' >> pComment <|> optionalEOL )
    optionalEOL = optional endOfLine >> spaces
    open  = char '[' >> pComment <|> optionalEOL
    close = char ']' >> optionalEOL

-- | Parses a file as a list of programs.
pFile :: String -> [Either ParseError Program]
pFile = parseNextProgram
  where
    parseNextProgram leftOver = if leftOver == "" then [] else case parseWithLeftOver pProgram leftOver of
      Right (prog,newLeftOver) -> Right prog : parseNextProgram newLeftOver
      (Left e)                        -> [Left e]
    parseWithLeftOver p = parse' (liftA2 (,) p  getInput)


-- | Main function, pass filepath to file.
parseZillyFile :: String -> IO ()
parseZillyFile fp = do
  contents <- readFile fp
  forM_ ([0 :: Int ..] `zip` pFile contents) $ \(i,r) -> do
    putStrLn "----------------------"
    putStrLn $ "Program: " <> show i
    putStrLn "----------------------"
    print r

execStatementIO :: State -> Statement -> IO State
execStatementIO store (Show s e) = do
  putStrLn $ s <> show (rvalue store e)
  pure store
execStatementIO store action = pure $ exec store action

execProgramIO :: State -> Program -> IO State
execProgramIO = foldM execStatementIO

execZillyFileIO :: Programs -> IO [State]
execZillyFileIO = traverse (execProgramIO empty)

run :: String -> IO [Maybe State]
run fp = do
  contents <- readFile fp
  forM ([0 :: Int ..] `zip` pFile contents) $ \(i,r) -> do
    putStrLn "----------------------"
    putStrLn $ "Program: " <> show i
    putStrLn "----------------------"
    case r of
      Left e  -> print e >> pure Nothing
      Right p -> Just <$> execProgramIO empty p



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
  , "Parsing \"  Á12A3aÁXσ   \" should be Sym Á12A3aÁXσ"
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
  [ "Parsing: <Z> x -> x should be Lambda Z (Sym x) (Sym x)"
    ~: Right (Lambda Z (Sym "x") (Sym "x")) ~=? parse' (fully pExpression) "<Z> x -> x"
  , "Parsing: (<Z> x -> x) should be Lambda Z (Sym x) (Sym x)"
    ~: Right (Lambda Z (Sym "x") (Sym "x")) ~=? parse' (fully pExpression) "(<Z> x -> x)"
  , "Parsing: <(Z)> x -> x should be Lambda Z (Sym x) (Sym x)"
    ~: Right (Lambda Z (Sym "x") (Sym "x")) ~=? parse' (fully pExpression) "<(Z)> x -> x"
  , "Parsing: (<(Z)> x -> x) should be Lambda Z (Sym x) (Sym x)"
    ~: Right (Lambda Z (Sym "x") (Sym "x")) ~=? parse' (fully pExpression) "(<(Z)> x -> x)"
  , "Parsing: <Z> x -> <Z> y -> minus(x)(y) should be Lambda Z (Sym x) (Lambda Z (Minus (Sym x) (Sym y)))"
    ~: Right (Lambda Z (Sym "x") (Lambda Z (Sym "y") (Minus (Sym "x") (Sym "y")))) ~=? parse' (fully pExpression) "<Z> x -> <Z> y -> minus(x)(y)"
  , "Parsing: <Z> x -> <Z -> Z> f -> f(x) should be Lambda Z (Sym x) (Lambda (Fun Z Z) (Sym f) (Apply (Sym f) (Sym x)))"
    ~: Right (Lambda Z (Sym "x") (Lambda (Fun Z Z) (Sym "f") (Apply (Sym "f") (Sym "x")))) ~=? parse' (fully pExpression) "<Z> x -> <Z -> Z> f -> f(x)"
  , "Parsing <Z> x -> <Z> y -> <Z -> Z> f -> minus(x)(f(y)) should be:\n"
    <> "Lambda Z (Sym x) (Lambda Z (Sym y) (Lambda (Fun Z Z) (Sym f) (Minus (Sym x) (Apply (Sym f) (Sym y)))))"
    ~: Right (Lambda Z (Sym "x") (Lambda Z (Sym "y") (Lambda (Fun Z Z) (Sym "f") (Minus (Sym "x") (Apply (Sym "f") (Sym "y")))))) ~=? parse' (fully pExpression) "<Z> x -> <Z> y -> <Z -> Z> f -> minus(x)(f(y))"
  ]




testApply :: IO Counts
testApply = runTestTT . TestList $
  [ "Parsing: (<Z> x -> x)(5) should be Apply (Lambda Z (Sym x) (Sym x))(Val 5)"
    ~: Right (Apply (Lambda Z (Sym "x") (Sym "x")) (Val 5)) ~=? parse' (fully pExpression) "(<Z> x -> x)(5)"
  , "Parsing: f(5) should be Apply (Sym f) (Val 5)"
    ~: Right (Apply (Sym "f") (Val 5)) ~=? parse' (fully pExpression) "f(5)"
  , "Parsing: (f)(5) should be Apply (Sym f) (Val 5)"
    ~: Right (Apply (Sym "f") (Val 5)) ~=? parse' (fully pExpression) "(f)(5)"
  , "Parsing: f(g(minus(5)(y))(f)) should be: Apply (Sym f) (Apply (Apply (Sym g) (Minus (Val 5) (Sym y))) (Sym f))"
    ~: Right (Apply (Sym "f") (Apply (Apply (Sym "g") (Minus (Val 5) (Sym "y"))) (Sym "f"))) ~=? parse' (fully pExpression) "f(g(minus(5)(y))(f))"
  , "Parsing: (<Z> x -> <Z> y -> <Z -> Z> f -> minus(x)(f(y)))(5)(6) should be:\n"
    <> "Apply (Apply (Lambda Z (Sym x) (Lambda Z (Sym y) (Lambda (Fun Z Z) (Sym f) (Minus (Sym x) (Apply (Sym f) (Sym y)))))) (Val 5)) (Val 6)"
    ~: Right (Apply
      (Apply
        (Lambda
          Z
          (Sym "x")
          (Lambda
            Z
            (Sym "y")
            (Lambda
              (Fun Z Z)
              (Sym "f")
              (Minus
                (Sym "x")
                (Apply
                  (Sym "f")
                  (Sym "y")
                )
              )
            )
          )
        )
        (Val 5))
      (Val 6)) ~=? parse' (fully pExpression) "(<Z> x -> <Z> y -> <Z -> Z> f -> minus(x)(f(y)))(5)(6)"
  ]



-- * Should we change if(b,y,z) to if(b)(y)(z)?
testIf :: IO Counts
testIf = runTestTT . TestList $
  [ "Parsing if(lt(x)(5),x,y) should be If (Less (Sym x) (Val 5), (Sym x), (Sym y))"
    ~: Right (If (Less (Sym "x") (Val 5), Sym "x", Sym "y")) ~=? parse' (fully pExpression) "if(lt(x)(5),x,y)"
  ]

{-
We dont provide much testing for predifined functions since they are essentially unfolds
of pApply.
-}
testLess :: IO Counts
testLess = runTestTT . TestList $
  [ "Parsing lt(x)(y) should be Less (Sym x) (Sym y)"
    ~: Right (Less (Sym "x") (Sym "y")) ~=? parse' (fully pExpression) "lt(x)(y)"
  , "Parsing lt(minus(x)(4))(y) should be Less (Minus (Sym x) (Val 4)) (Sym y)"
    ~: Right (Less (Minus (Sym "x") (Val 4)) (Sym "y")) ~=? parse' (fully pExpression) "lt(minus(x)(4))(y)"
  , "Parsing lt(f(x)(4))(y) should be Less (Apply (Apply (Sym f) (Sym x) )(Val 4)) (Sym y)"
    ~: Right (Less (Apply (Apply (Sym "f") (Sym "x")) (Val 4)) (Sym "y")) ~=? parse' (fully pExpression) "lt(f(x)(4))(y)"
  ]

testMinus :: IO Counts
testMinus = runTestTT . TestList $
  [ "Parsing minus(x)(y) should be Minus (Sym x) (Sym y)"
    ~: Right (Minus (Sym "x") (Sym "y")) ~=? parse' (fully pExpression) "minus(x)(y)"
  , "Parsing minus(minus(x)(4))(y) should be Minus (Minus (Sym x) (Val 4)) (Sym y)"
    ~: Right (Minus (Minus (Sym "x") (Val 4)) (Sym "y")) ~=? parse' (fully pExpression) "minus(minus(x)(4))(y)"
  , "Parsing minus(f(x)(4))(y) should be Minus (Apply (Apply (Sym f) (Sym x) )(Val 4)) (Sym y)"
    ~: Right (Minus (Apply (Apply (Sym "f") (Sym "x")) (Val 4)) (Sym "y")) ~=? parse' (fully pExpression) "minus(f(x)(4))(y)"
  ]


testDefer :: IO Counts
testDefer = runTestTT . TestList $
  [ "Parsing '44' should be Defer (Val 44)" ~: Right (Defer (Val 44)) ~=? parse' (fully pExpression) "'44'"
  , "Parsing ''44'' should be Defer (Defer (Val 44))" ~: Right (Defer (Defer (Val 44))) ~=? parse' (fully pExpression) "''44''"
  , "Parsing \"  ' Á12A3aÁXσ '   \" should be Defer (Sym Á12A3aÁXσ)"
    ~: Right (Defer (Sym "Á12A3aÁXσ")) ~=? parse' (fully pExpression) "  'Á12A3aÁXσ'   "
  , "Parsing <Z> x -> '<Z> y -> <Z -> Z> f -> minus(x)(f(y))' should be:\n"
    <> "Lambda Z (Sym x) (Defer $ Lambda Z (Sym y) (Lambda (Fun Z Z) (Sym f) (Minus (Sym x) (Apply (Sym f) (Sym y)))))"
    ~: Right (Lambda Z (Sym "x") (Defer $ Lambda Z (Sym "y") (Lambda (Fun Z Z) (Sym "f") (Minus (Sym "x") (Apply (Sym "f") (Sym "y")))))) ~=? parse' (fully pExpression) "<Z> x -> '<Z> y -> <Z -> Z> f -> minus(x)(f(y))'"
  , "Parsing: '(<Z> x -> <Z> y -> <Z -> Z> f -> minus(x)(f(y)))(5)'(6) should be:\n"
    <> "Apply (Defer $ Apply (Lambda Z (Sym x) (Lambda Z (Sym y) (Lambda (Fun Z Z) (Sym f) (Minus (Sym x) (Apply (Sym f) (Sym y)))))) (Val 5)) (Val 6)"
    ~: Right (Apply
      (Defer $ Apply
        (Lambda
          Z
          (Sym "x")
          (Lambda
            Z
            (Sym "y")
            (Lambda
              (Fun Z Z)
              (Sym "f")
              (Minus
                (Sym "x")
                (Apply
                  (Sym "f")
                  (Sym "y")
                )
              )
            )
          )
        )
        (Val 5))
      (Val 6)) ~=? parse' (fully pExpression) "'(<Z> x -> <Z> y -> <Z -> Z> f -> minus(x)(f(y)))(5)'(6)"
  , "Parsing: f('x') should be: Apply (Sym f) (Defer (Sym x))"
    ~: Right (Apply (Sym "f") (Defer (Sym "x"))) ~=? parse' (fully pExpression) "f('x')"
  ]

testDefine :: IO Counts
testDefine = runTestTT . TestList $
  [ "Parsing: <Z -> Z -> Z> f := <Z> x -> <Z> y -> minus(x)(y) should be: \n"
    <> "Define (Fun Z (Fun Z Z)) (Sym f) (Lambda Z (Sym x) (Lambda Z (Sym y) (Minus (Sym x) (Sym y))))"
    ~: Right (Define (Fun Z (Fun Z Z)) (Sym "f") (Lambda Z (Sym "x") (Lambda Z (Sym "y") (Minus (Sym "x") (Sym "y")))))
    ~=? parse' (fully pAction) "<Z -> Z -> Z> f := <Z> x -> <Z> y -> minus(x)(y)"
  , "Parsing: Z minus(3,2) := 3 should throw Left"
    ~: isLeft ( parse' (fully pAction) "Z minus(3,2) := 3") ~? "'Z minus(3,2) := 3' Should throw an error"
  ]

testAssign :: IO Counts
testAssign = runTestTT . TestList $
  [ "Parsing:  f := <Z> x -> <Z> y -> minus(x)(y) should be: \n"
    <> "Assign (Sym f) (Lambda Z (Sym x) (Lambda Z (Sym y) (Minus (Sym x) (Sym y))))"
    ~: Right (Assign (Sym "f") (Lambda Z (Sym "x") (Lambda Z (Sym "y") (Minus (Sym "x") (Sym "y")))))
    ~=? parse' (fully pAction) "f := <Z> x -> <Z> y -> minus(x)(y)"
  , "Parsing: minus(3,2) := 3 should throw Left"
    ~: isLeft ( parse' (fully pAction) "minus(3,2) := 3") ~? "'minus(3,2) := 3' Should throw an error"
  ]

testMagic :: IO Counts
testMagic = runTestTT . TestList $
  [ "Parsing: show(\"\",f(g(minus(5)(y))(f))) should be: Magic . Show $ Apply (Sym \"f\") (Apply (Apply (Sym \"g\") (Minus (Val 5) (Sym \"y\"))) (Sym \"f\"))"
    ~: Right (Show "" $ Apply (Sym "f") (Apply (Apply (Sym "g") (Minus (Val 5) (Sym "y"))) (Sym "f")))
    ~=? parse' (fully pAction) ".show(\"\",f(g(minus(5)(y))(f)))"
  ]

-- | Run this to test everything.
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
  putStrLn "testLess"
  putStrLn "-------------------------"
  print =<< testLess

  putStrLn "-------------------------"
  putStrLn "testIf"
  putStrLn "-------------------------"
  print =<< testIf

  putStrLn "-------------------------"
  putStrLn "testMinus"
  putStrLn "-------------------------"
  print =<< testMinus

  putStrLn "-------------------------"
  putStrLn "testApply"
  putStrLn "-------------------------"
  print =<< testApply

  putStrLn "-------------------------"
  putStrLn "testDefer"
  putStrLn "-------------------------"
  print =<< testDefer


  putStrLn "-------------------------"
  putStrLn "testDefine"
  putStrLn "-------------------------"
  print =<< testDefine

  putStrLn "-------------------------"
  putStrLn "testAssign"
  putStrLn "-------------------------"
  print =<< testAssign

  putStrLn "-------------------------"
  putStrLn "testMagic"
  putStrLn "-------------------------"
  print =<< testMagic

