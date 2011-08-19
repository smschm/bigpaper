import Text.ParserCombinators.Parsec hiding (space, spaces)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Array
import Data.List (nub)
import Data.Maybe (catMaybes)
import System.IO.Unsafe
{- META-TODO
    done?: parser of doc -> specList
    * specList -> todoList conversion
    * todoList -> todoNow
    -----
    * interaction
        or
    * daemonization
-}
-- vim: set expandtab

type Str = String
type Ident = Str

type Progress = Either Str Str
-- where Left is Not Done, Right is Done

data Todo = Todo {
    name :: Str,
    prereqs :: S.Set Ident,
    softprs :: S.Set Ident,
    progress :: Progress,
    priority :: Int,
    wheres :: [Str],
    context :: Maybe Str
} deriving (Eq, Show)

data Spec = Done Str | NotDone Str | PRef [Ident] | SPRef [Ident] | --Depends [Int] |
    Where Str | Context Str | Priority Int | Name Str
    deriving (Eq, Show)

type TDL = M.Map Ident Todo

getLinkTuples tdl = let f (x,y) = map ((,) x) (S.toList (prereqs y))
    in (M.toList tdl) >>= f
--M.fromList tdl >>= (\(x,y) -> map ((,) x) (S.toList (prereqs y)))
{-getDependents tdl = M.foldWithKey addDependentsOf M.empty tdl
    where
        addDependentsOf k v m = M.alter (adep' k) v m
        adep' k Nothing = [k]
        adep' k (Just j) = k:j-}

inject :: String -> a -> a
inject s x = seq (unsafePerformIO (putStr s >> return x)) x

getRight :: (Show l) => (Either l r) -> r -> r
getRight e dflt = case e of
    Left l  -> inject (show l) dflt
    Right r -> r

isDone :: Todo -> Bool
isDone t = case progress t of
    Left _  -> False
    Right _ -> True

defaultT = Todo "" S.empty S.empty (Left "") 1 [] Nothing
--specListToTodo = sltt' defaultT
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

(!!!) :: TDL -> Ident -> Todo
x !!! n = let v = M.lookup n x in case v of
    (Just j) -> j
    Nothing -> inject ("warning: task #" ++ (show n) ++ " not found.\n") defaultT

canDoNow :: TDL -> Ident -> Bool
canDoNow tdl n = (not . isDone $ tdl !!! n) &&
    (and . map (isDone . (tdl !!!)) . S.toList . prereqs $ tdl !!! n)

-------- parser:
------ generals
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

test :: IO TDL
test = do
    c <- readFile "testdoc"
    let p = parse (many taskspec) "" c
    --let m = foldl max 0 $ map fst (r p)
    let a = M.fromList $ getRight p []
    let a' = fmap (specListToTodo) a
    return a'
--    return a

printT :: Todo -> IO ()
printT t = do
    putStr (show (priority t))
    putStr ":\t"
    putStrLn (name t)

doNow = do
    a <- test
    let di = filter (canDoNow a) (M.keys a)
    mapM_ printT $ map (a !!!) di

contexts :: TDL -> [Str]
contexts = nub . catMaybes . M.elems . fmap context

data DotRenderSubgraphStyle = DRSS {
    drssSubgraphs   :: TDL -> [Maybe Str],
    drssGetSubgraph :: Todo -> Maybe Str,
    drssLabelCmd    :: Str -> Str,
    drssStyleStr    :: Str
}

defaultDRSS = DRSS ((Nothing :) . map Just . contexts) context
    (\x -> "label=\"" ++ x ++ "\";") ("color = black;")
nullDRSS = DRSS (const [Nothing]) (const Nothing) undefined undefined

data DotRenderStyle = DRS {
    drsSubgraphStyle :: DotRenderSubgraphStyle,
    drsGraph         :: Str,
    drsLabel         :: Todo -> Str,
    drsPrqArrows     :: Str,
    drsSprqArrows    :: Str,
    drsNode          :: Str,
    drsDo            :: Str,
    drsDone          :: Str,
    drsNotDone       :: Str
}

defaultDRS = DRS defaultDRSS "" name "" "[style=dotted]"
    ",shape=box,fontname=Helvetica"
    ",style=filled,color=\"#ccccff\""
    ",style=filled,"
    ""
noSubgraphDRS = defaultDRS { drsSubgraphStyle = nullDRSS }

dotRender :: DotRenderStyle -> TDL -> IO ()
dotRender drs tdl = do
    putStrLn "digraph G {"
    putStrLn (drsGraph drs)
    mapM_
        (\x -> renderCtx x . M.toList . M.filter (\e -> (gsub e) == x) $ tdl)
        (drssSubgraphs drss tdl)
    putStrLn "}"
    --mapM_ dotRenderT . M.toList $ tdl
    where
        drss = drsSubgraphStyle drs
        gsub = drssGetSubgraph drss
        renderCtx Nothing l = do
            mapM_ dotRenderT l
        renderCtx (Just j) l = do
            putStrLn ("subgraph cluster" ++ j ++ " {")
            putStrLn (drssLabelCmd drss j)
            putStrLn (drssStyleStr drss)
            mapM_ dotRenderT l
            putStrLn "}"
        dotRenderT (n,t) = do
            putStrLn ("\t" ++ (n) ++ " [label=\"" ++ (drsLabel drs t) ++ "\"" ++ (drsNode drs) ++
                (if isDone t then drsDone drs else if canDoNow tdl n then drsDo drs else drsNotDone drs) ++ "];")
            --mapM_ (arrowRender n False) $ (S.toList (softprs t))
            --mapM_ (arrowRender n True)  $ (S.toList (softprs t))
            -- let's DRY this to the xxx-treme!
            mapM_ (\(d,f) -> mapM_ (arrowRender n d) $ (S.toList (f t)))
              [(0,prereqs),(1,softprs)]
        arrowRender f style t = putStrLn ("\t" ++ (t) ++ " -> " ++ (f) ++
          case style of { 0 -> drsPrqArrows drs ; _ -> drsSprqArrows drs } ++ ";" )

main = do
    a <- test
    --doNow
    --dotRender noSubgraphDRS a
    dotRender defaultDRS a
    --print (contexts a)
