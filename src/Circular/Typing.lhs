> {-# LANGUAGE TemplateHaskell
>            , FlexibleInstances
>            , MultiParamTypeClasses
>            , FlexibleContexts
>            , UndecidableInstances #-}
>
> module Circular.Typing where
>
> import Circular.Syntax
> import Circular.Evaluation

> type Env = [(X, E)]

> hasType :: Fresh m => Env -> E -> E -> m ()
> hasType ctx (Fun b) Typ = do
>   (x, (e1, e2)) <- unbind b
>   hasType ((x, e1) : ctx) e1 Typ
>   hasType ((x, e1) : ctx) e2 Typ
> hasType ctx (Abs b1) (Fun b2) = do
>   Just (x1, (e1, e2), x2, (e3, e4)) <- unbind2 b1 b2
>   sameTerm e1 e3
>   hasType ((x1, e1) : ctx) e2 e4
>   hasType ((x1, e1) : ctx) e1 Typ
> hasType ctx (App e1 e2) e3 = do
>   e1' <- typeOf ctx e1
>   (x, e3, e4) <- isFun e1'
>   hasType ctx e2 (subst x e2 e3)
> hasType ((x1, e1) : ctx) (Var x2) e2
>   |  x1 == x2
>   =  sameTerm e1 e2
>   |  x1 /= x2 
>   =  hasType ctx (Var x2) e2
> hasType ctx Typ Typ = do
>   return ()
> hasType ctx _ _ = do
>   fail "wrong type."

> isFun :: Fresh m => E -> m (X, E, E)
> isFun e1 = do
>   e1' <- hnf e1
>   case e1' of
>     Fun b -> do
>       (x, (e1, e2)) <- unbind b
>       return (x, e1, e2)
>     _ -> do
>       fail "wrong type."

> typeOf :: Fresh m => Env -> E -> m E
> typeOf ctx (Abs b) = do
>   (x, (e1, e2)) <- unbind b
>   hasType ((x, e1) : ctx) e1 Typ
>   e3 <- typeOf ((x, e1) : ctx) e2
>   return (Fun (bind x (e1, e3)))
> typeOf ctx (Fun b) = do
>   (x, (e1, e2)) <- unbind b
>   hasType ((x, e1) : ctx) e1 Typ
>   hasType ((x, e1) : ctx) e2 Typ
>   return Typ
> typeOf ctx (App e1 e2) = do
>   e1' <- typeOf ctx e1
>   (x, e3, e4) <- isFun e1'
>   hasType ctx e2 (subst x e2 e3)
>   return (subst x e2 e4)
> typeOf ((x1, e1) : ctx) (Var x2)
>   |  x1 == x2
>   =  return e1
>   |  x1 /= x2 
>   =  typeOf ctx (Var x2)
> typeOf ctx Typ = do
>   return Typ
> typeOf _ _ = do
>   fail "wrong type."
