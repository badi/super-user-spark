{-# LANGUAGE OverloadedStrings #-}
module CoreTypes where

import           Control.Monad (mzero)
import           Data.Aeson    (FromJSON (..), ToJSON (..), Value (..), object,
                                (.:), (.=))
import           Data.Text     (Text)

type Directory = FilePath

-- | A 'Cmd' is a pair of (path to executable, arguments).
--
-- The path to the executable and either be bare (eg "file") in which
-- case it is found by inspecting the `$PATH`, or the absolute path to
-- the executable.
type Cmd = (FilePath, [Text])

-- | A 'Pipe' is a sequence of 'Cmd's that are applied sequentially
type Pipe = [Cmd]

-- | The kind of a deployment
data DeploymentKind = LinkDeployment
                    | CopyDeployment
                    | PipeDeployment Pipe
    deriving (Show, Eq)

instance Read DeploymentKind where
    readsPrec _ "link" = [(LinkDeployment,"")]
    readsPrec _ "copy" = [(CopyDeployment,"")]
    readsPrec _ _ = error "no implementation for Read.readsPrec on DeploymentKind[PipeDeployment" -- FIXME Read DeploymentKind

instance FromJSON DeploymentKind where
    parseJSON (String "link") = return LinkDeployment
    parseJSON (String "copy") = return CopyDeployment
    parseJSON (Object o) = PipeDeployment <$> o .: "pipe"
    parseJSON _ = mzero

instance ToJSON DeploymentKind where
    toJSON LinkDeployment = String "link"
    toJSON CopyDeployment = String "copy"
    toJSON (PipeDeployment cmd) = object [ "pipe" .= cmd ]

