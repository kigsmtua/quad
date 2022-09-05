module Docker where

import RIO
import qualified Network.HTTP.Simple as HTTP
import qualified Socket
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.Aeson.Types as Aeson.Types


newtype Image = Image Text deriving(Eq, Show)

imageToText :: Image -> Text
imageToText (Image image) = image


newtype ContainerExitCode = ContainerExitCode Int deriving(Eq, Show)


exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code


newtype ContainerId = ContainerId Text deriving (Eq,Show)

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId c) = c

data CreateContainerOptions
    = CreateContainerOptions
        {
            image :: Image
        }

parseResponse ::
    HTTP.Response ByteString
    -> (Aeson.Value -> Aeson.Types.Parser a)
    -> IO a
parseResponse res parser = do
    let result = do
        value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
        Aeson.Types.parseEither parser value
    case result of
        Left e -> throwString e
        Right status -> pure status


createContainer :: CreateContainerOptions -> IO ContainerId
createContainer options = do
    ---- T
    let parser = Aeson.withObject "create-container" $ \o -> do
        cId <- o .: "Id"
        pure $ ContainerId cId

    manager <- Socket.newManager "/var/run/docker.sock"
    let image = imageToText options.image

    let body = Aeson.object
                 [
                     ("Image", Aeson.toJSON image),
                     ("Tty", Aeson.toJSON True),
                     ("Labels", Aeson.object [("quad", "")]),
                     ("Cmd", "echo hello"),
                     ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh","-c"])
                 ]
    let req = HTTP.defaultRequest
            & HTTP.setRequestManager manager
            & HTTP.setRequestPath "/v1.41/containers/create"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body
    res <- HTTP.httpBS req
    parseResponse res parser

startContainer :: ContainerId -> IO ()
startContainer container = do
    manager <- Socket.newManager "/var/run/docker.sock"
    let path = "/v1.41/containers/" <> containerIdToText container <> "/start"

    let req = HTTP.defaultRequest
            & HTTP.setRequestManager manager
            & HTTP.setRequestPath (encodeUtf8 path)
            & HTTP.setRequestMethod "POST"

    void $ HTTP.httpBS req