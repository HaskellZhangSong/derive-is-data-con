{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

import Test.HUnit
import Cases

listTests :: [Test]
listTests = [bool, maybe', either', op, gadtOp, recTest]

main :: IO ()
main = mapM_ runTestTT listTests
