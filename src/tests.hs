module Main where

import Test.Framework (defaultMain)
import Circular.Syntax.Concrete.Tests

main = defaultMain Main.tests

tests =
  [  Circular.Syntax.Concrete.Tests.tests
  ]
