import qualified Data.Set as S
import qualified Data.Map as M
import Data.Array
import Data.List (nub)
import Data.Maybe (catMaybes)

import Types
import Operations
import DotRender
import Parser
-------- parser:
------ generals

printT :: Todo -> IO ()
printT t = do
    putStr (show (priority t))
    putStr ":\t"
    putStrLn (name t)

doNow = do
    a <- testParser
    let di = filter (canDoNow a) (M.keys a)
    mapM_ printT $ map (a !!!) di

main = do
    a <- testParser
    --doNow
    --dotRender noSubgraphDRS a
    dotRender defaultDRS a
    --print (contexts a)
