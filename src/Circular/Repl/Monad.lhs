> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> module Circular.Repl.Monad where
>
> import Prelude (String)
>
> import Control.Monad (Monad)
> import Control.Monad.State (StateT, MonadState)
> import Control.Monad.Trans (MonadIO (liftIO))
> import Control.Monad.Error (ErrorT, MonadError, Error)
> import Data.Functor (Functor)
> import Unbound.LocallyNameless.Fresh (Fresh, FreshMT)
> import Circular.State
> import System.IO (IO)
> import qualified System.IO as IO
> import Text.Show (Show (..))
>
> newtype M a = M {runM :: StateT State (FreshMT (ErrorT String IO)) a}
>   deriving (Functor, Monad, MonadState State, MonadIO, Fresh, MonadError String)

> type FilePath = IO.FilePath

> readFile :: MonadIO m => FilePath -> m String
> readFile f = liftIO (IO.readFile f)

> putStr :: MonadIO m => String -> m ()
> putStr text = liftIO (IO.putStr text)

> putStrLn :: MonadIO m => String -> m ()
> putStrLn text = liftIO (IO.putStrLn text)

> print :: (Show a, MonadIO m) => a -> m ()
> print text = liftIO (IO.print text)

> getLine :: MonadIO m => m String
> getLine = liftIO IO.getLine

> flush :: MonadIO m => m ()
> flush = liftIO (IO.hFlush IO.stdout)
