module Main where

import Prelude ()

import Circular.Syntax
import Circular.Typing
import Circular.Repl
import Circular.Repl.Monad

import Data.Either
import Data.List ((++))

import Circular.State
import Control.Monad (Monad (..))
import Control.Monad.State (evalStateT)
import Control.Monad.Error (runErrorT)

import Unbound.LocallyNameless.Fresh (runFreshMT)

main = do
  result <- runErrorT (runFreshMT (runM repl `evalStateT` initialState))
  case result of
    Left error -> putStrLn (error ++ " :(")
    Right result -> return result
