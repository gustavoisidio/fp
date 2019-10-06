import Data.List

prepSet :: Ord a => [ a ] -> [ a ]
prepSet x = map head . group . sort $ x
