module Parser where

import Text.ParserCombinators.Parsec hiding (space, spaces)
import qualified Data.Set as S
import qualified Data.Map as M
import Types
import Operations

eol = do {char '\n'; return ()} <|> eof
space = choice $ map char " \v\f\t"
spaces' = many1 space
spaces = many space
restOfLine = manyTill anyChar eol

number = do {
    ds <- many1 digit;
    return $ ((read ds) :: Int)
    } <?> "number"
numberRange :: Parser [Int]
numberRange = numberRange' <?> "number range"
numberRange' = do
    ds <- many1 digit
    char '-'
    dt <- many1 digit
    return $ [(read ds)..(read dt)]
numspec :: Parser [Int]
numspec = (fmap concat $
    ((try numberRange) <|> (fmap (:[]) number)) `sepBy1` (char ','))
    <?> "number list spec."

----- individual spec parsers
donep, notdonep, prefp, sprefp, wherep, contextp, namep, priorityp :: Parser Spec

prefixfield :: Parser a -> Char -> (a -> b) -> Parser b
prefixfield f c g = do {
    char c;
    n <- f;
    return $ g n;
    } <?> ("prefix field " ++ [c])

numspecfield :: Char -> ([Int] -> a) -> Parser a
numspecfield = prefixfield numspec
stringfield :: Char -> (Str -> a) -> Parser a
stringfield = prefixfield (many1 alphaNum)
stringfield' :: Char -> (Str -> a) -> Parser a
stringfield' = prefixfield (many alphaNum)

prefp     = numspecfield '!' (PRef . map show)
sprefp    = numspecfield '?' (SPRef . map show)
donep     = stringfield' '+' Done
notdonep  = stringfield' '-' NotDone
wherep    = stringfield  '@' Where
contextp  = stringfield  '#' Context
priorityp = (prefixfield number) '*' Priority
namep     = fmap Name $ do
                c <- anyChar
                l <- restOfLine
                return $ c : l

--------------
specp :: Parser Spec
specp = choice [donep, notdonep, prefp, sprefp, priorityp, wherep, contextp]

taskspec :: Parser (String, [Spec])
taskspec = do
    n <- number
    spaces
    p <- specp `endBy` spaces
    nm <- namep
    return (show n, nm : p)

specListToTodo :: [Spec] -> Todo
specListToTodo = foldr sltt' defaultT
  where sltt' :: (Spec -> Todo -> Todo)
        sltt' p x = case p of
          Done r     -> x { progress = Right r }
          NotDone r  -> x { progress = Left r }
          PRef rs    -> x { prereqs = S.union (S.fromList rs) (prereqs x) }
          SPRef rs   -> x { softprs = S.union (S.fromList rs) (softprs x) }
          Where s    -> x { wheres = s : (wheres x) }
          --Context s  -> x { contexts = s : (contexts x) }
          Context s  -> x { context = Just s }
          Priority n -> x { priority = n }
          Name s     -> x { name = s }
          --Depends rs -> x

testParser :: IO BP
testParser = do
    c <- readFile "testdoc"
    let p = parse (many taskspec) "" c
    --let m = foldl max 0 $ map fst (r p)
    let a = M.fromList $ getRight p []
    let a' = fmap (specListToTodo) a
    return a'
