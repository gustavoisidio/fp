import Control.Monad
--import Test.QuickCheck
import Data.List
--import Control.Monad.Random

newtype Gen a = Gen ( Rand -> a )

