> module Circular.Repl where
>
> import Prelude ()
>
> import Control.Monad (Monad (..))
> import Circular.Repl.Command
> import Circular.Repl.Monad
> import Circular.State
> import Circular.Syntax
> import Circular.Evaluation
> import Circular.Typing
> import Text.Show (Show (..))
> import Text.Read (Read (..), read)
> import Control.Monad (mapM_)
> import Control.Monad.Error (catchError)
> import Control.Monad.State (gets, modify)
> import Data.Char (isSpace)
> import Data.List (map, (++))
>
> printTypeOf :: E -> M ()
> printTypeOf e1 = do
>   e2 <- prepare e1
>   e3 <- typeOf [] e2
>   print e3
>
> prepare :: E -> M E
> prepare e = do
>   ds <- gets defs
>   return (substs [(x, e) | Def x _ e <- ds] e)
>
> loadFile :: FilePath -> M ()
> loadFile f = do
>   text <- readFile f
>   let P prog = read (map (\x -> if isSpace x then ' ' else x) text)
>   mapM_ evaluateS prog

> evaluateSE :: SE -> M ()
> evaluateSE (S s) = evaluateS s
> evaluateSE (E e) = evaluateE e

> evaluateS :: S -> M ()
> evaluateS (Def x e1 e2) = do
>   e1' <- prepare e1
>   e2' <- prepare e2
>   hasType [] e1' Typ
>   hasType [] e2' e1'
>   modify (\state -> state {defs = Def x e1' e2' : defs state})

> evaluateE :: E -> M ()
> evaluateE e1 = do
>   e2 <- prepare e1
>   e3 <- typeOf [] e2
>   e4 <- nf e2
>   putStrLn (show e4 ++ " : " ++ show e3)
>
> repl :: M ()
> repl = do
>   putStr "> "
>   flush
>   text <- getLine
>   command <- parseCommand text `catchError` (\e -> putStrLn e >> return NoOp)
>
>   case command of
>     NoOp -> repl
>     Quit -> return ()
>     Help -> putStrLn "No help available. Ask Tillmann." >> repl
>     Load f -> loadFile f `catchError` putStrLn >> repl
>     Type text -> do
>       printTypeOf (read text) `catchError` putStrLn >> repl
>     Eval text -> do
>       evaluateSE (read text)  `catchError` putStrLn >> repl
