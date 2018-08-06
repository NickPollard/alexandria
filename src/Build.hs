module Build where

import Control.Monad (forM_, void)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import Data.Text
import System.Directory
import System.FilePath ((</>))
import System.Process

import Package

clean :: MonadIO m => m ()
clean = do
  liftIO . putStrLn $ "Removing dependencies and managed files..."
  liftIO $ callProcess "rm" ["-rf", dependencyDir]
  liftIO . putStrLn $ "Dependencies cleaned"

update :: MonadIO m => m ()
update = do
  p <- loadPackage "package.yaml"
  either (liftIO . print) update' p

update' :: MonadIO m => Package -> m ()
update' (Package deps) = forM_ (Map.toList deps) $ \(Name name, dep) -> do
  liftIO . putStrLn $ "Updating dependency '" <> unpack name <> "'..."
  updateDep dep

updateDep :: MonadIO m => Dependency -> m ()
updateDep (Dependency (Name name) (Git repo ref)) = do
  let dir = dependencyDir </> unpack name
  unlessM (liftIO $ doesPathExist dir) $ clone repo dir
  checkout repo dir ref
updateDep (Dependency (Name name) _) = liftIO . putStrLn $ "Unhandled dependency type for dependency '" <> unpack name <> "'"

clone :: MonadIO m => Repo -> FilePath -> m ()
clone (Repo repo) dir = liftIO $ do
  callProcess "git" ["clone", unpack repo, dir]

checkout :: MonadIO m => Repo -> FilePath -> GitRef -> m ()
checkout (Repo repo) dir r = liftIO $ do
  putStrLn $ "Checking out ref '" <> ref r <> "'..."
  let process = (proc "git" ["checkout", ref r]){ cwd = Just dir }
  (_,_,_,handle) <- createProcess process
  void $ waitForProcess handle
    where ref (GitTag t) = unpack t
          ref (GitBranch b) = unpack b

-- TODO from config
dependencyDir :: FilePath
dependencyDir = "./.dependencies"
