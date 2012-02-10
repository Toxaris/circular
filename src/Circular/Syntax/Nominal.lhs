Abstract Syntax (Nominal Style)
-------------------------------

> {-# LANGUAGE TemplateHaskell #-}
> module Circular.Syntax.Nominal where
>
> import Control.Isomorphism.Partial
> import Control.Isomorphism.Partial.TH
>
> type X   =  String
> data E   =  Abs X E E
>          |  App E E
>          |  Var X
>          |  Fun X E E
>          |  Typ
>   deriving (Eq)
>
> data S   =  Def X E E
>
> data SE  =  S S 
>          |  E E
>
> data P   =  P [S]
>
> $(defineIsomorphisms ''E)
> $(defineIsomorphisms ''S)
> $(defineIsomorphisms ''SE)
> $(defineIsomorphisms ''P)

