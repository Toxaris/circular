> module Circular.Repl.Command where

> import Data.Char (isSpace)
> import Circular.Syntax

> data Command
>   =  NoOp
>   |  Quit
>   |  Help
>   |  Load String
>   |  Type String
>   |  Eval String

> parseCommand :: Monad m => String -> m Command
> -- :quit
> parseCommand (':' : 'q' : 'u' : 'i' : 't' : rest)
>   |  all isSpace rest  =  return (Quit)
>   |  otherwise         =  fail ":quit takes no parameters."
> parseCommand (':' : 'q' : 'u' : 'i' : rest)
>   |  all isSpace rest  =  return (Quit)
>   |  otherwise         =  fail ":quit takes no parameters."
> parseCommand (':' : 'q' : 'u' : rest)
>   |  all isSpace rest  =  return (Quit)
>   |  otherwise         =  fail ":quit takes no parameters."
> parseCommand (':' : 'q' : rest) 
>   |  all isSpace rest  =  return (Quit)
>   |  otherwise         =  fail ":quit takes no parameters."
>
> -- :help
> parseCommand (':' : 'h' : 'e' : 'l' : 'p' : rest)
>   |  all isSpace rest  =  return (Help)
>   |  otherwise         =  fail ":help takes no parameters."
> parseCommand (':' : 'h' : 'e' : 'l' : rest)
>   |  all isSpace rest  =  return (Help)
>   |  otherwise         =  fail ":help takes no parameters."
> parseCommand (':' : 'h' : 'e' : rest)
>   |  all isSpace rest  =  return (Help)
>   |  otherwise         =  fail ":help takes no parameters."
> parseCommand (':' : 'h' : rest) 
>   |  all isSpace rest  =  return (Help)
>   |  otherwise         =  fail ":help takes no parameters."
>
> -- :load
> parseCommand (':' : 'l' : 'o' : 'a' : 'd' : space : rest)
>   |  isSpace space     =  return (Load rest)
> parseCommand (':' : 'l' : 'o' : 'a' : space : rest)
>   |  isSpace space     =  return (Load rest)
> parseCommand (':' : 'l' : 'o' : space : rest)
>   |  isSpace space     =  return (Load rest)
> parseCommand (':' : 'l' : space : rest)
>   |  isSpace space     =  return (Load rest)
>
> -- :type
> parseCommand (':' : 't' : 'y' : 'p' : 'e' : space : rest)
>   |  isSpace space     =  return (Type rest)
> parseCommand (':' : 't' : 'y' : 'p' : space : rest)
>   |  isSpace space     =  return (Type rest)
> parseCommand (':' : 't' : 'y' : space : rest)
>   |  isSpace space     =  return (Type rest)
> parseCommand (':' : 't' : space : rest)
>   |  isSpace space     =  return (Type rest)

> -- :eval
> parseCommand (':' : 'e' : 'v' : 'a' : 'l' : space : rest)
>   |  isSpace space     =  return (Eval rest)
> parseCommand (':' : 'e' : 'v' : 'a' : space : rest)
>   |  isSpace space     =  return (Eval rest)
> parseCommand (':' : 'e' : 'v' : space : rest)
>   |  isSpace space     =  return (Eval rest)
> parseCommand (':' : 'e' : space : rest)
>   |  isSpace space     =  return (Eval rest)

> -- :noop
> parseCommand (':' : 'n' : 'o' : 'o' : 'p' : rest)
>   |  all isSpace rest  =  return (NoOp)
>   |  otherwise         =  fail ":noop takes no parameters."
> parseCommand (':' : 'n' : 'o' : 'o' : rest)
>   |  all isSpace rest  =  return (NoOp)
>   |  otherwise         =  fail ":noop takes no parameters."
> parseCommand (':' : 'n' : 'o' : rest)
>   |  all isSpace rest  =  return (NoOp)
>   |  otherwise         =  fail ":noop takes no parameters."
> parseCommand (':' : 'n' : rest) 
>   |  all isSpace rest  =  return (NoOp)
>   |  otherwise         =  fail ":noop takes no parameters."

> -- unknown command
> parseCommand (':' : rest)
>   =  fail "unknown command"

> -- no command given (:noop or :eval)
> parseCommand rest
>   |  all isSpace rest  =  return (NoOp)
>   |  otherwise         =  return (Eval rest)

