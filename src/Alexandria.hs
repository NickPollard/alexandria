module Alexandria where

import Data.Text
import Shake

-- TODO from config
dependencyDir :: FilePath
dependencyDir = "./.dependencies"

data Config = Config {
  dependencyDir :: FilePath
}

data Alex m a = Alex (ReaderT Config m a)
  deriving (Functor, Applicative, Monad)

--
-- usage:
--
--   alexandria update
--
update :: Package -> IO ()
update package = forM_ (dependencies package) updateDep

--
-- usage:
--
--   alexandria clean
--
clean :: IO ()
clean = do
  cleanDir dependencyDir
    where
      cleanDir = cmd "rm -rf " <> dependecyDir <> "/*"

updateDep :: Dependency -> IO ()
updateDep (Local path) = path `copyRecursivelyTo` dependencyDir
updateDep (Git path) = path `cloneIntoSubDir` dependencyDir

copyRecursivelyTo :: LocalPath -> FilePath -> IO ()
copyRecursivelyTo src dst = do
  let dir = dependencyDir / package
  cleanDir dir
  cmd "cp -r " <> src <> " " <> dir

cloneIntoSubDir :: GitPath -> FilePath -> IO ()
cloneIntoSubDir repo dir = do
  cd dir
  -- TODO - if existing, update; if missing, clone
  git clone repo "."

dependencies = "dependencies"

shakeMain :: IO ()
shakeMain = do
  packageYaml <- ioLoadPackageYaml
  shakeArgs shakeOptions $ do
    phony dependencies $ do
      deps <- getDependencies <$> readPackage "package.yaml"
      need deps

    phony "clean" $ removeFilesAfter ".dependencies" ["//*"]

    want dependencies

    "*.git" %> \repo -> do
      need $ ".dependencies/" <> repo <> "/.repo"
      cd repoPath
      git fetch
      git checkout branch

    ".dependencies/*/.repo" %> \repo -> do
      mkdir repoPath
      cd repoPath
      git clone repo
      touch ".repo" -- touch a file so we can track it in the dependency system; we don't want to track a directory
