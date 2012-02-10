> module Circular.Syntax.Concrete.Tests where
>
> import Test.Framework (testGroup)
> import Test.Framework.Providers.HUnit
> import Test.HUnit (assertFailure)
>
> import Text.Syntax.Parser.Naive (Parser (Parser))
> import Text.Syntax.Printer.Naive (Printer (Printer))
> import Circular.Syntax.Nominal
> import Circular.Syntax.Concrete

> testParse concrete abstract
>   =  testGroup concrete
>      [  testCase ("Parse") $
>         case syntaxE of 
>           Parser p -> case [x | (x, "") <- p concrete] of
>             [] -> assertFailure "No result."
>             [result]
>               | result == abstract -> return ()
>               | otherwise -> assertFailure "Wrong result."
>             _ -> assertFailure "Ambiguous result."
>      ,  testCase ("Print") $
>         case syntaxE of
>           Printer p -> case p abstract of
>             Nothing -> assertFailure "No result."
>             Just result
>               |  result == concrete -> return ()
>               |  otherwise -> assertFailure "Wrong result."
>      ]

> star
>   =  testParse
>        "*"
>        Typ

> identifier
>   =  testParse
>        "x"
>        (Var "x")

> identityOnTypes
>   =  testParse
>        "(x : *) . x"
>        (Abs "x" Typ (Var "x"))

> typeOfIdentityOnTypes
>   =  testParse
>        "(x : *) -> *"
>        (Fun "x" Typ Typ)

> polymorphicIdentity
>   =  testParse
>        "(x : *) . (y : x) . y"
>        (Abs "x" Typ (Abs "y" (Var "x") (Var "y")))

> typeOfPolymorphicIdentity
>   =  testParse
>        "(x : *) -> (y : x) -> x"
>        (Fun "x" Typ (Fun "y" (Var "x") (Var "x")))

> application1
>   =  testParse
>        "((x : *) . (y : x) . y) *"
>        (App (Abs "x" Typ (Abs "y" (Var "x") (Var "y"))) Typ)

> application2
>   =  testParse
>        "((x : *) . (y : x) . y) ((x : *) -> (y : x) -> x)"
>        (App 
>          (Abs "x" Typ (Abs "y" (Var "x") (Var "y")))
>          (Fun "x" Typ (Fun "y" (Var "x") (Var "x"))))

> application3
>   =  testParse
>        "((x : *) . (y : x) . y) ((x : *) -> (y : x) -> x) ((x : *) . (y : x) . y)"
>        (App 
>          (App 
>            (Abs "x" Typ (Abs "y" (Var "x") (Var "y")))
>            (Fun "x" Typ (Fun "y" (Var "x") (Var "x"))))
>          (Abs "x" Typ (Abs "y" (Var "x") (Var "y"))))

> tests
>   =  testGroup "Circular.Syntax.Concrete"
>      [  star
>      ,  identifier
>      ,  identityOnTypes
>      ,  typeOfIdentityOnTypes
>      ,  polymorphicIdentity
>      ,  typeOfPolymorphicIdentity
>      ,  application1
>      ,  application2
>      ,  application3
>      ]
