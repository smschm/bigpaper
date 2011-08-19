import qualified Data.Set as S
import qualified Data.Map as M
import Data.Array

import Types
import Operations
import DotRender
import Parser

-- | Simple test: print tasks that can be done now
doNow = do
    a <- testParser
    let di = filter (canDoNow a) (M.keys a)
    mapM_ print $ map (a !!!) di

main = do
    a <- testParser
    --doNow
    --dotRender noSubgraphDRS a
    --dotRender defaultDRS a
