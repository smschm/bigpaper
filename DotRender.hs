module DotRender where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (nub)
import Data.Maybe (catMaybes)
import Operations
import Types

contexts :: BP -> [Str]
contexts = nub . catMaybes . M.elems . fmap context

data DotRenderSubgraphStyle = DRSS {
    drssSubgraphs   :: BP -> [Maybe Str],
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

dotRender :: DotRenderStyle -> BP -> IO ()
dotRender drs bp = do
    putStrLn "digraph G {"
    putStrLn (drsGraph drs)
    mapM_
        (\x -> renderCtx x . M.toList . M.filter (\e -> (gsub e) == x) $ bp)
        (drssSubgraphs drss bp)
    putStrLn "}"
    --mapM_ dotRenderT . M.toList $ bp
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
                (if isDone t then drsDone drs else if canDoNow bp n then drsDo drs else drsNotDone drs) ++ "];")
            --mapM_ (arrowRender n False) $ (S.toList (softprs t))
            --mapM_ (arrowRender n True)  $ (S.toList (softprs t))
            -- let's DRY this to the xxx-treme!
            mapM_ (\(d,f) -> mapM_ (arrowRender n d) $ (S.toList (f t)))
              [(0,prereqs),(1,softprs)]
        arrowRender f style t = putStrLn ("\t" ++ (t) ++ " -> " ++ (f) ++
          case style of { 0 -> drsPrqArrows drs ; _ -> drsSprqArrows drs } ++ ";" )

