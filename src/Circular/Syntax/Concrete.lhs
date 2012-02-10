Concrete Syntax
---------------

> module Circular.Syntax.Concrete
>   (  syntaxE
>   ,  syntaxS
>   ,  syntaxSE
>   ,  syntaxP
>   )  where
>
> import Prelude ()
> import Control.Isomorphism.Partial
> import Text.Syntax
> import Circular.Syntax.Nominal
> import Data.Char (Char, isLetter, isDigit)
> import Data.Either

> syntaxE :: Syntax delta => delta E
> syntaxE = e2 where
>   e0  =    typ        <$>  text "*"
>       <|>  var        <$>  identifier
>       <|>                  parens e2
>   e1  =    foldl app  <$>  e0 
>                       <*>  many (sepSpace *> e0)
>   e2  =    abs        <$>  binder identifier e2 (text ".") e2
>       <|>  fun        <$>  binder identifier e2 (text "->") e2
>       <|>                  e1
>
> syntaxS :: Syntax delta => delta S
> syntaxS
>   =    def  
>   <$>  identifier
>   <*>  optSpace
>   *>   text ":"
>   *>   optSpace
>   *>   syntaxE
>   <*>  optSpace
>   *>   text "="
>   *>   optSpace
>   *>   syntaxE
>
> syntaxSE :: Syntax delta => delta SE
> syntaxSE
>   =    s <$> syntaxS
>   <|>  e <$> syntaxE
>
> syntaxP :: Syntax delta => delta P
> syntaxP
>   =    p <$> sepBy syntaxS sepSpace
>
> binder :: Syntax delta => delta a -> delta b -> delta () -> delta c -> delta (a, (b, c))
> binder name qualifier sep body
>   =    text "("
>   *>   skipSpace
>   *>   name
>   <*>  optSpace
>   *>   text ":"
>   *>   optSpace
>   *>   qualifier
>   <*>  skipSpace
>   *>   text ")"
>   *>   optSpace
>   *>   sep
>   *>   optSpace
>   *>   body

> letter, digit :: Syntax delta => delta Char
> letter  =  subset isLetter <$> token
> digit   =  subset isDigit <$> token

> identifier :: Syntax delta => delta X
> identifier 
>   = cons <$> letter <*> many (letter <|> digit)

> parens :: Syntax delta => delta alpha -> delta alpha
> parens inner
>   = text "(" *> skipSpace *> inner <* skipSpace <* text ")"
