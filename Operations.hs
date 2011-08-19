module Operations where

import qualified Data.Set as S
import qualified Data.Map as M
import System.IO.Unsafe
import Types

getLinkTuples bp = let f (x,y) = map ((,) x) (S.toList (prereqs y))
    in (M.toList bp) >>= f

-- | Inject a string into IO during a computation
isDone :: Todo -> Bool
isDone t = case progress t of
    Left _  -> False
    Right _ -> True


(!!!) :: BP -> Ident -> Todo
x !!! n = let v = M.lookup n x in case v of
    (Just j) -> j
    Nothing -> inject ("warning: task #" ++ (show n) ++ " not found.\n") defaultT

canDoNow :: BP -> Ident -> Bool
canDoNow bp n = (not . isDone $ bp !!! n) &&
    (and . map (isDone . (bp !!!)) . S.toList . prereqs $ bp !!! n)

inject :: String -> a -> a
inject s x = seq (unsafePerformIO (putStr s >> return x)) x

getRight :: (Show l) => (Either l r) -> r -> r
getRight e dflt = case e of
    Left l  -> inject (show l) dflt
    Right r -> r

