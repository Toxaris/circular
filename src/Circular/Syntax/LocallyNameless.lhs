Abstract Syntax (Locally Nameless Style)
----------------------------------------

> module Circular.Syntax.LocallyNameless where

> import Unbound.LocallyNameless

> type X   =  Name E
> data E   =  Abs (Bind X (E, E))
>          |  App E E
>          |  Var X
>          |  Fun (Bind X (E, E))
>          |  Typ
>          
> data S   =  Def X E E
> data SE  =  S S
>          |  E E
> data P   =  P [S]
