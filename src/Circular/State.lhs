> module Circular.State where
>
> import Circular.Syntax
>
> data State = State {
>   defs :: [S]
> }

> initialState :: State
> initialState = State {
>   defs = []
> }
