{-# LANGUAGE OverloadedStrings #-}

module Package where

{-
   The Alexandria Package Model

   packages are defined in a `package.yaml` file thusly:

    dependencies:
      - name: Foo
        git:
          repo: git@github.com:foo/foo.git
          tag: v0.0.1
      - name: Bar
        git:
          repo: git@github.com:foo/bar.git
          branch: master

-}

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Map
import qualified Data.Map as Map
import Data.Text
import Data.Yaml

newtype Name = Name Text deriving (Show, Eq, Ord)
newtype LocalPath = LocalPath FilePath deriving (Show, Eq)
newtype Repo = Repo Text deriving (Show, Eq)

{-
  name: Foo
  local:
    /home/user/foo

    -or-

  name: Foo
  git:
    repo: git@foo/foo.git
    tag:
      v1.2.0
-}
data Dependency = Dependency {
  depName :: Name,
  depSrc :: DependencySrc
} deriving (Eq, Show)

data DependencySrc = Local LocalPath
                   | Git Repo GitRef
                   deriving (Show, Eq)

{-
  tag:
    v1.2.0

    -or-

  branch:
    master
-}
data GitRef = GitBranch Text
            | GitTag Text
            deriving (Show, Eq)

instance FromJSON Dependency where
  parseJSON = withObject "Dependency" $ \o ->
                Dependency <$> (Name <$> o .: "name") <*> parseDependencySrc o

parseDependencySrc :: Object -> Parser DependencySrc
parseDependencySrc o = (o .: "git" >>= git)
                   <|> (o .: "local" >>= local)
  where
    git :: Object -> Parser DependencySrc
    git obj = Git <$> (Repo <$> obj .: "repo") <*> gitref obj
    local :: Text -> Parser DependencySrc
    local = return . Local . LocalPath . unpack
    gitref :: Object -> Parser GitRef
    gitref obj = (GitTag <$> obj .: "tag")
          <|> (GitBranch <$> obj .: "branch")

data Package = Package {
  dependencies :: Map Name Dependency
} deriving (Show, Eq)

instance FromJSON Package where
  parseJSON = withObject "Package" $ \o -> do
    deps <- o .: "dependencies"
    return . Package . Map.fromList $ (\d -> (depName d, d)) <$> deps

loadPackage :: MonadIO m => FilePath -> m (Either ParseException Package)
loadPackage packageFile = liftIO $ decodeFileEither packageFile
