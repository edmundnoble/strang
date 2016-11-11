{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}


import           Test.Framework
import           Strang.Interpreter

fromRight :: Either a b -> b
fromRight (Right b) = b

splitCommas = fromRight $ interpretProgram "s/,/"
commasToNewlines = fromRight $ interpretProgram "s/,/j/\n/"

test_nonEmpty = do assertEqual "[hello,world]" (splitCommas "hello,world")
                  --  assertEqual "hello\nworld" (commasToNewlines "hello,world")
                   --assertEqual [3,2,1] (myReverse [1,2,3])

-- test_empty = assertEqual ([] :: [Int]) [] --(myReverse [])

main = htfMain htf_thisModulesTests
