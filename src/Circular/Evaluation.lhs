> {-# LANGUAGE TemplateHaskell
>            , FlexibleInstances
>            , MultiParamTypeClasses
>            , FlexibleContexts
>            , UndecidableInstances #-}
>
> module Circular.Evaluation where

> import Circular.Syntax

> hnf :: Fresh m => E -> m E
> hnf (Abs b) = do
>   return (Abs b)
> hnf (Fun b) = do
>   return (Fun b)
> hnf (App e1 e2) = do
>   e1' <- hnf e1
>   case e1' of
>     Abs b -> do
>       (x, (_, e3)) <- unbind b
>       hnf (subst x e2 e3)
>     _ -> do
>       return (App e1' e2)
> hnf (Var x) = do
>   return (Var x)
> hnf (Typ) = do
>   return (Typ)

> nf :: Fresh m => E -> m E
> nf (Fun b) = do
>   (x, (e1, e2)) <- unbind b
>   e1' <- nf e1
>   e2' <- nf e2
>   return (Fun (bind x (e1', e2')))
> nf (Abs b) = do
>   (x, (e1, e2)) <- unbind b
>   e1' <- nf e1
>   e2' <- nf e2
>   return (Abs (bind x (e1', e2')))
> nf (App e1 e2) = do
>   e1' <- nf e1
>   e2' <- nf e2
>   case e1' of
>     Abs b -> do
>       (x, (_, e3)) <- unbind b
>       nf (subst x e2' e3)
>     _ -> do
>       return (App e1' e2')
> nf (Var x) = do
>   return (Var x)
> nf (Typ) = do
>   return (Typ)

> sameTerm :: Fresh m => E -> E -> m ()
> sameTerm e1 e2 = do
>   e1' <- nf e1
>   e2' <- nf e2
>   if aeq e1' e2'
>     then return ()
>     else fail "not the same term."
