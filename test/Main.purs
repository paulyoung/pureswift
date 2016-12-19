module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.Assert (ASSERT)
import Test.PureSwift.CodeGen (testCodeGen)

main :: forall e. Eff (assert :: ASSERT, console :: CONSOLE | e) Unit
main = do
  testCodeGen
  log ""
