{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}


import           Test.Framework
import           Strang.Interpreter

splitCommas = interpretProgram "s/,/"

test_nonEmpty = do assertEqual (Right "[hello,world]") (fmap ($ "hello,world") splitCommas)
                   --assertEqual [3,2,1] (myReverse [1,2,3])

-- test_empty = assertEqual ([] :: [Int]) [] --(myReverse [])

main = htfMain htf_thisModulesTests
