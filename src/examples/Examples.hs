{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

test_nonEmpty = do assertEqual [1] (myReverse [1])
                   assertEqual [3,2,1] (myReverse [1,2,3])

test_empty = assertEqual ([] :: [Int]) (myReverse [])

main = htfMain htf_thisModulesTests
