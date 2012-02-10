Binding Structure
-----------------

> isFun :: Fresh m => E -> m (X, E, E)
> isFun e1 = do
>   e1' <- hnf e1
>   case e1' of
>     Fun b -> do
>       (x, (e1, e2)) <- unbind b
>       return (x, e1, e2)
>     _ -> do
>       fail "wrong type."

> x = s2n "x"
> y = s2n "y"
> z = s2n "z"

> identity = Fun (bind x (Typ, Fun (bind y (Var x, Var y))))
