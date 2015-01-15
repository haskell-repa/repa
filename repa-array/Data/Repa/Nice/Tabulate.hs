
module Data.Repa.Nice.Tabulate
        (Tabulate (..))
where
import Data.Repa.Nice.Display   as A
import Data.Repa.Nice           as A
import Data.Repa.Array          as A
import Data.List                as L
import Data.Monoid
import Data.Maybe


class Nicer a => Tabulate a where
 tabulate :: a -> String

instance Tabulate Int where
 tabulate x     = show x

instance Tabulate Float where
 tabulate x     = show x

instance Tabulate Double where
 tabulate x     = show x


-- Tabulate a single vector by snowing all the values on a single line,
-- but with the same spacing.
instance (Bulk r DIM1 a, Nicer a, Show (Nice a))
      => Tabulate (A.Vector r a) where
 tabulate xs    
  = let strs    = L.map (show . nice) $ A.toList xs
        lens    = L.map L.length strs
        len     = maximum lens
    in  concatMap (padR (len + 1)) strs


-- Tabulate nested arrays by showing them as matrices.
instance ( Bulk r1 DIM1 (A.Vector r2 a)
         , Bulk r2 DIM1 a
         , Nicer a, Show (Nice a))
      => Tabulate (A.Vector r1 (A.Vector r2 a)) where
 tabulate xs    
  = let 
        -- Convert all the fields to strings.
        strss   = L.map (L.map (show . nice)) 
                $ A.toLists xs

        -- Decide how to display a single column.
        displayOfCol c
                = mconcat
                $ mapMaybe (\line -> if c >= L.length line
                                        then Nothing
                                        else Just (takeDisplay (line !! c)))
                $ strss

        -- How many columns we have.
        nCols    = maximum $ L.map L.length strss

        -- Decide how to display all the columns.
        displays = L.map displayOfCol [0.. nCols - 1]

        makeLine line
         = L.intercalate " " $ L.zipWith display displays line

    in  L.intercalate "\n" $ L.map makeLine strss
