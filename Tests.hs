import Char
import List
import qualified  Test.QuickCheck
import  Test.QuickCheck hiding (test)
import Text.Printf
import Data.Bloom
import Data.Digest.MD5

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

instance Arbitrary Char where
    arbitrary     = choose ('\0', '\128')
    coarbitrary c = variant (ord c `rem` 4)

md5hash s = Data.Digest.MD5.hash (map (fromIntegral.ord) s) ++ md5hash ('a':s)

testbloom = bloom md5hash 256 5

prop_test_is_true_for_added_items s = test (add testbloom s) s
    where  _ = s :: String

prop_test_false_for_empty s = test testbloom s == False

tests = [("test (add blm s) s == True", Test.QuickCheck.test prop_test_is_true_for_added_items),
        ("test blm s == False for empty blm", Test.QuickCheck.test prop_test_is_true_for_added_items)]