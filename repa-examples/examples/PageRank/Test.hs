
-- | Page data for testing the algorithm.
module Test where
import Page
import Rank
import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as U

-- | Pages with no out-links.
pages0 :: V.Vector Page 
pages0  = V.fromList
        [ Page 0 $ U.fromList []
        , Page 1 $ U.fromList []
        , Page 2 $ U.fromList []
        , Page 3 $ U.fromList []
        , Page 4 $ U.fromList []
        , Page 5 $ U.fromList []
        , Page 6 $ U.fromList [] ]


-- | Inital ranks of pages with no outlinks.
ranks0 :: U.Vector Rank
ranks0 = initialRanks pages0


-- | Pages with some outlinks.
pages1 :: V.Vector Page 
pages1   = V.fromList
        [ Page 0 $ U.fromList []
        , Page 1 $ U.fromList [2, 3]
        , Page 2 $ U.fromList []
        , Page 3 $ U.fromList [1, 2, 5]
        , Page 4 $ U.fromList [5, 6]
        , Page 5 $ U.fromList [3, 6]
        , Page 6 $ U.fromList [3] ]


-- | Initial ranks for pages with some outlinks.
ranks1 :: U.Vector Rank
ranks1 = initialRanks pages0

