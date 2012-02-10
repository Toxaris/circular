Name Resolution
===============

> {-# LANGUAGE TemplateHaskell
>            , FlexibleInstances
>            , MultiParamTypeClasses
>            , FlexibleContexts
>            , UndecidableInstances #-}
>
> module Circular.Syntax.NameResolution where
>
> import Unbound.LocallyNameless
>
> import Text.Syntax.Parser.Naive as Parser (Parser (Parser))
> import Text.Syntax.Printer.Naive as Printer (print)
>
> import qualified Circular.Syntax.Nominal as Nominal
> import Circular.Syntax.LocallyNameless
> import Circular.Syntax.Concrete

Binding Structure
-----------------

The binding structure of the abstract syntax in the locally
nameless representation is expressed using the `unbound` library.
We derive the necessary instances.

> $(derive [''E])
> 
> instance Alpha E where
> instance Subst E E where
>   isvar (Var x)  =  Just (SubstName x)
>   isvar _        =  Nothing

Resolving Names
---------------

We resolve names by converting from the nominal to the locally
nameless representation. The `bind` function does most of the
work. That function is provided by the `unbound` library.

> resolveE :: Nominal.E -> E
> resolveE (Nominal.Abs x e1 e2)  =  Abs (bind (s2n x) (resolveE e1, resolveE e2))
> resolveE (Nominal.App e1 e2)    =  App (resolveE e1) (resolveE e2)
> resolveE (Nominal.Var x)        =  Var (s2n x)
> resolveE (Nominal.Fun x e1 e2)  =  Fun (bind (s2n x) (resolveE e1, resolveE e2))
> resolveE (Nominal.Typ)          =  Typ

> resolveS :: Nominal.S -> S
> resolveS (Nominal.Def x e1 e2)  =  Def (s2n x) (resolveE e1) (resolveE e2)

> resolveSE :: Nominal.SE -> SE
> resolveSE (Nominal.S s) = S (resolveS s)
> resolveSE (Nominal.E e) = E (resolveE e)

> resolveP :: Nominal.P -> P
> resolveP (Nominal.P defs) = P (map resolveS defs)

Synthesizing Names
------------------

We synthesize names by converting from the locally nameless to
the nominal representation. The `unbind` function does most of
the work. Again, that function is provided by the `unbound`
library.

> unresolveE :: Fresh m => E -> m Nominal.E
> unresolveE (Abs b) = do
>   (x, (e1, e2)) <- unbind b
>   e1'  <-  unresolveE e1
>   e2'  <-  unresolveE e2
>   return (Nominal.Abs (show x) e1' e2')
> unresolveE (App e1 e2) = do
>   e1'  <-  unresolveE e1
>   e2'  <-  unresolveE e2
>   return (Nominal.App e1' e2')
> unresolveE (Var x) = do
>   return (Nominal.Var (show x))
> unresolveE (Fun b) = do
>   (x, (e1, e2)) <- unbind b
>   e1'  <-  unresolveE e1
>   e2'  <-  unresolveE e2
>   return (Nominal.Fun (show x) e1' e2')
> unresolveE (Typ) = do
>   return Nominal.Typ

> unresolveS :: Fresh m => S -> m Nominal.S
> unresolveS (Def x e1 e2) = do
>   e1'  <-  unresolveE e1
>   e2'  <-  unresolveE e2
>   return (Nominal.Def (show x) e1' e2')

> unresolveSE :: Fresh m => SE -> m Nominal.SE
> unresolveSE (S s) = do
>   s' <- unresolveS s
>   return (Nominal.S s')
> unresolveSE (E e) = do
>   e' <- unresolveE e
>   return (Nominal.E e')

> unresolveP :: Fresh m => P -> m Nominal.P
> unresolveP (P defs) = do
>   defs' <- mapM unresolveS defs
>   return (Nominal.P defs')

Parsing and Printing
--------------------

Thanks to the conversion between locally nameless and nominal
representation, we can now use the invertible syntax description
for the nominal representation also to parse or print the locally
nameless representation

> instance Show E where 
>   show e = case Printer.print syntaxE (runFreshM (unresolveE e)) of
>     Just text -> text
>     Nothing -> error "unable to print expression"
>
> instance Read E where
>   readsPrec _ text = case syntaxE of 
>     Parser p -> [(resolveE e, rest) | (e, rest) <- p text]

> instance Show S where 
>   show s = case Printer.print syntaxS (runFreshM (unresolveS s)) of
>     Just text -> text
>     Nothing -> error "unable to print statement"
>
> instance Read S where
>   readsPrec _ text = case syntaxS of 
>     Parser p -> [(resolveS s, rest) | (s, rest) <- p text]

> instance Show SE where 
>   show se = case Printer.print syntaxSE (runFreshM (unresolveSE se)) of
>     Just text -> text
>     Nothing -> error "unable to print statement or expression"
>
> instance Read SE where
>   readsPrec _ text = case syntaxSE of 
>     Parser p -> [(resolveSE se, rest) | (se, rest) <- p text]

> instance Show P where 
>   show p = case Printer.print syntaxP (runFreshM (unresolveP p)) of
>     Just text -> text
>     Nothing -> error "unable to print statement or expression"
>
> instance Read P where
>   readsPrec _ text = case syntaxP of 
>     Parser p -> [(resolveP p, rest) | (p, rest) <- p text]
