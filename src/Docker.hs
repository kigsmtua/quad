{-# LANGUAGE OverloadedStrings #-}

module Docker where

import RIO
import qualified Network.HTTP.Simple as HTTP


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
    let body = () -- Figure out what body is and what we pass here
    let req = HTTP.defaultRequest -- This is what this looks like and we can say so
            & HTTP.setRequestPath "/v1.41/containers/create"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body
    res <- HTTP.httpBS req
    traceShowIO res