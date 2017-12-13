{-# LANGUAGE DeriveGeneric #-}

module Site.Config
    ( Config(..)
    , readConfig
    ) where

import qualified Data.Yaml as Y
import           Control.Exception (throwIO)
import           System.FilePath.Posix (FilePath)
import           Data.Aeson (FromJSON)
import           GHC.Generics

data Config = Config
    { styleDir :: FilePath
    , styleMain :: FilePath
    , stripPrefixDirs :: [FilePath]
    , staticPageDir :: FilePath
    , ignoredPages :: [FilePath]
    } deriving (Show, Generic)

instance FromJSON Config

readConfig :: IO Config
readConfig = either throwIO return =<< Y.decodeFileEither "config.yaml"