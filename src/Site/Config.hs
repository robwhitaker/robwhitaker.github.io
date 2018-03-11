{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Config
    ( Config(..)
    , readConfig
    ) where

import Hakyll
import qualified Data.Yaml as Y
import           Control.Exception (throwIO)
import           Data.Aeson
import           GHC.Generics

data Config = Config
    { feedConfig :: FeedConfiguration
    } deriving (Generic)

instance FromJSON Config
-- TODO: Maybe wrap this in a newtype to avoid the orphan instance
instance FromJSON FeedConfiguration where
    parseJSON (Object v) = FeedConfiguration
        <$> v .: "feedTitle"
        <*> v .: "feedDescription"
        <*> v .: "feedAuthorName"
        <*> v .: "feedAuthorEmail"
        <*> v .: "feedRoot"
    parseJSON _ = mempty


readConfig :: IO Config
readConfig = either throwIO return =<< Y.decodeFileEither "config.yaml"