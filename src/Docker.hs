{-# LANGUAGE OverloadedStrings #-}

module Docker where

--- This is what happens when such things can be used here
import RIO
import qualified Network.HTTP.Simple as HTTP
import qualified Socket
import qualified Data.Aeson as Aeson


newtype Image = Image Text deriving(Eq, Show)

imageToText :: Image -> Text
imageToText (Image image) = image


newtype ContainerExitCode = ContainerExitCode Int deriving(Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

data CreateContainerOptions
    = CreateContainerOptions
        {
            image :: Image
        }


createContainer :: CreateContainerOptions -> IO()
createContainer options = do
    manager <- Socket.newManager "/var/run/docker.sock"
    let body = Aeson.Null
    let req = HTTP.defaultRequest
            & HTTP.setRequestManager manager
            & HTTP.setRequestPath "/v1.41/containers/create"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body
    res <- HTTP.httpBS req
    traceShowIO res