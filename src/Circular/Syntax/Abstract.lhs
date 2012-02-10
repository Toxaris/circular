> {-# LANGUAGE TemplateHaskell
>            , FlexibleInstances
>            , MultiParamTypeClasses
>            , FlexibleContexts
>            , UndecidableInstances #-}
>
> module Circular.Syntax where

> import Unbound.LocallyNameless

> type X  =  Name E
> data E  =  Abs (Bind X (E, E))
>         |  App E E
>         |  Var X
>         |  Fun (Bind X (E, E))
>         |  Typ

> $(derive [''E])

> parens :: Monad m => Bool -> m String -> m String
> parens False p = p
> parens True p = do
>   x <- p
>   return ("(" ++ x ++ ")")

> showE :: Fresh m => Int -> E -> m String
> showE p (Fun b) = parens (p < 2) $ do
>   (x, (e1, e2)) <- unbind b
>   s2 <- showE 2 e2
>   if x `elem` fv e2
>     then do
>       s1 <- showE 2 e1
>       return ("(" ++ name2String x ++ " : " ++ s1 ++ ") -> " ++ s2)
>     else do
>       s1 <- showE 1 e1
>       return (s1 ++ " -> " ++ s2)
> showE p (App e1 e2) = parens (p < 1) $ do
>   s1 <- showE 0 e1
>   s2 <- showE 1 e2
>   return (s1 ++ " " ++ s2)
> showE p (Var x) = do
>   return (name2String x)
> showE p (Typ) = do
>   return "*"

> instance Show E where 
>   show e = runFreshM (showE 2 e) 

> instance Alpha E where
> instance Subst E E where
>   isvar (Var x)  =  Just (SubstName x)
>   isvar _        =  Nothing

> hnf :: Fresh m => E -> m E
> hnf (Fun b) = do
>   return (Fun b)
> hnf (App e1 e2) = do
>   e1' <- hnf e1
>   case e1' of
>     Fun b -> do
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
> nf (App e1 e2) = do
>   e1' <- nf e1
>   e2' <- nf e2
>   case e1' of
>     Fun b -> do
>       (x, (_, e3)) <- unbind b
>       nf (subst x e2' e3)
>     _ -> do
>       return (App e1' e2')
> nf (Var x) = do
>   return (Var x)
> nf (Typ) = do
>   return (Typ)

> type Env = [(X, E)]

> hasType :: Fresh m => Env -> E -> E -> m ()
> hasType ctx (Fun b) Typ = do
>   (x, (e1, e2)) <- unbind b
>   hasType ((x, e1) : ctx) e2 Typ
>   hasType ((x, e1) : ctx) e1 Typ
> hasType ctx (Fun b1) (Fun b2) = do
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

> sameTerm :: Fresh m => E -> E -> m ()
> sameTerm e1 e2 = do
>   e1' <- nf e1
>   e2' <- nf e2
>   if aeq e1' e2'
>     then return ()
>     else fail "wrong type."

> typeOf :: Fresh m => Env -> E -> m E
> typeOf ctx (Fun b) = do
>   (x, (e1, e2)) <- unbind b
>   hasType ((x, e1) : ctx) e1 Typ
>   e3 <- typeOf ((x, e1) : ctx) e2
>   return (Fun (bind x (e1, e3)))
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

> x = s2n "x"
> y = s2n "y"
> z = s2n "z"

> identity = Fun (bind x (Typ, Fun (bind y (Var x, Var y))))
