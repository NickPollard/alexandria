module Build where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import Data.Text

import Package

update :: MonadIO m => m ()
update = do
  p <- loadPackage "package.yaml"
  either (liftIO . print) update' p

update' :: MonadIO m => Package -> m ()
update' (Package deps) = forM_ (Map.toList deps) $ \(Name name, _) ->
  liftIO . putStrLn $ "Updating dependency '" <> unpack name <> "'..."
