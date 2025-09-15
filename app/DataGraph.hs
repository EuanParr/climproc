{- defining processing nodes and things for caching (memoising) their results

(the results ultimately derive from file contents so Haskell does not memoise them,
but those files are not to change during program execution so memoising the results is appropriate)

because the results are of various types, the cache takes the form of a record with a field for each node
-}

module DataGraph where

import qualified Control.Monad

import qualified Graphics.Rendering.Chart.Easy as ChartEasy
import qualified Graphics.Rendering.Chart.Layout as ChartLayout

import qualified CdlProcessing as Cdl

-- a field in the cache, which may or may not contain a value
data CacheEntry a = CacheEntry
  {
    name :: String,
    result :: Maybe a
  }

-- a node in the data processing graph
data Node a = Node {node::(Cache -> IO (Cache, a))}

{-
(nodes become IO actions because some nodes are file reads and evaluating any node
may require the evaluation of nodes it depends on if their results are not in cache)
-}

-- Node is a state monad, where the cache holds the state
instance Functor Node where
  fmap = Control.Monad.liftM

instance Applicative Node where
  pure x = Node (\c -> pure (c, x))
  (<*>) = Control.Monad.ap

instance Monad Node where
  return = pure
  (Node xm) >>= f =
    Node (\c -> do
             (c', y) <- xm c
             node (f y) c')

-- turn an IO action into a node that doesn't change the cache
liftIO :: IO a -> Node a
liftIO xm = Node (\c -> xm >>= \x -> pure (c, x))

-- turn a node into an IO action by giving it an empty cache
execute :: Node a -> IO a
execute (Node xm) = do
  (_, x) <- xm initialCache
  pure x

-- given a node and a function to get its cache entry, memoises the node
memoise :: (Cache -> CacheEntry a) -> Node a -> Node a
memoise getter n = Node $ \c ->
  case getter c of
    CacheEntry _ (Just x) -> pure (c, x)
    CacheEntry _ Nothing -> node n c

-- the form of the cache record, very subject to change as nodes are added or changed
data Cache = Cache
  {
    ljungqvist2010 :: CacheEntry [(Integer,Integer,Rational,Rational,Rational,Rational)],
    hadcrut3v :: CacheEntry ([Rational], [Rational], [Rational], [[[Maybe Rational]]]),
    hadcrut5 :: CacheEntry ([Rational], [Rational], [Rational], [[[Maybe Rational]]]),
    binned_GSHHS_c :: CacheEntry Cdl.Group,
    ljungqvist2010Reconstruction :: CacheEntry [(Double, Double, Double, Double)],
    hadcrut3v_30_90n_decadal :: CacheEntry [(Double, Double)],
    hadcrut5_30_90n_decadal :: CacheEntry [(Double, Double)],
    world_map :: CacheEntry [[(Rational, Rational)]],
    equalAreaGlobeGraph :: CacheEntry (ChartEasy.EC (ChartLayout.Layout Double Double) () -> ChartEasy.EC (ChartLayout.Layout Double Double) ())
  }

-- create an empty cache entry
blank :: String -> CacheEntry a
blank xs = CacheEntry xs Nothing

-- the empty cache
initialCache :: Cache
initialCache = Cache
  {
    ljungqvist2010 = blank "Ljungvist data",
    hadcrut3v = blank "HadCRUT3v",
    hadcrut5 = blank "HadCRUT5",
    binned_GSHHS_c = blank "GSHHS, binned, crude resolution",
    ljungqvist2010Reconstruction = blank "Ljungqvist reconstruction",
    hadcrut3v_30_90n_decadal = blank "HadCRUT3v, 30-90N, decadal",
    hadcrut5_30_90n_decadal = blank "HadCRUT5, 30-90N, decadal",
    world_map = blank "GSHHS segments, crude resolution",
    equalAreaGlobeGraph = blank "Equal area globe projection graph"
  }
