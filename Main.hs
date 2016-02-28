import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import System.Environment
import System.IO
import qualified Data.Map.Strict as Map

import qualified Data.Text as T
import qualified Data.Text.IO as T

{- Degrees -}
data LocationDegrees = LocationDegrees Double
    deriving (Generic, Show)
instance ToJSON LocationDegrees
instance FromJSON LocationDegrees

{- Coordinate -}
data LocationCoordinate = LocationCoordinate {
    latitude :: LocationDegrees,
    longitude :: LocationDegrees
} deriving (Generic, Show)
instance ToJSON LocationCoordinate
instance FromJSON LocationCoordinate

{- Request -}
data Request = LocationRequest 
             | ArrivalRequest LocationCoordinate
             | ChoiceRequest String String
             | StatusRequest String
             | VisualRequest
             deriving (Generic, Show)
instance ToJSON Request
instance FromJSON Request

{- Choice -}
data Choice = LeftChoice | RightChoice deriving (Generic, Show)
instance ToJSON Choice
instance FromJSON Choice

{- ImageUploadId -}
newtype ImageUploadId = ImageUploadId Int deriving (Generic, Show)
instance ToJSON ImageUploadId
instance FromJSON ImageUploadId

{- Response -}
data Response = LocationResponse LocationCoordinate
              | ArrivalResponse Int
              | ChoiceResponse Choice
              | StatusResponse Bool
              | VisualResponse ImageUploadId
              deriving (Generic, Show)
instance ToJSON Response
instance FromJSON Response

{- UserId -}
newtype UserId = UserId Int deriving (Generic, Show)
instance ToJSON UserId
instance FromJSON UserId

{- Interaction -}
data Interaction = RequestedInteraction { 
    requesterId :: String
}  deriving (Generic, Show)
instance ToJSON Interaction
instance FromJSON Interaction

{- Phone Number -}
newtype PhoneNumber = PhoneNumber String deriving (Generic, Show, Ord, Eq)
instance ToJSON PhoneNumber
instance FromJSON PhoneNumber

{- UUID -}
newtype UUID = UUID String deriving (Generic, Show)
instance ToJSON UUID
instance FromJSON UUID

{- Device -}
data Device = Device {
  uuid :: UUID,
  phoneNumber :: PhoneNumber
} deriving (Generic, Show)
instance ToJSON Device
instance FromJSON Device

type DeviceTable = Map.Map PhoneNumber UUID

emptyQueue :: IO (TVar [Interaction])
emptyQueue =
    newTVarIO []

emptyDeviceTable :: IO (TVar DeviceTable)
emptyDeviceTable =
    newTVarIO Map.empty

getQueue :: MonadIO m => TVar [Interaction] -> m [Interaction]
getQueue notes =
    liftIO $ readTVarIO notes

postRequest :: MonadIO m => TVar [Interaction] -> Interaction -> m [Interaction]
postRequest queue interaction =
    liftIO $ do
      T.putStrLn $ (pack . show) interaction
      atomically $ do
        oldQueue <- readTVar queue
        let newQueue = interaction : oldQueue
        writeTVar queue newQueue
        return newQueue

registerDevice :: MonadIO m => TVar DeviceTable -> Device -> m DeviceTable
registerDevice deviceTable device =
    liftIO $ do
      atomically $ do
        oldDeviceTable <- readTVar deviceTable
        let newDeviceTable = Map.insert (phoneNumber device) (uuid device) oldDeviceTable
        writeTVar deviceTable newDeviceTable
        return ()

type InteractionAPI =
         Get Text
    :<|> "request" :> ReqBody Interaction :> Post [Interaction]
    :<|> "register" :> ReqBody Device :> Post ()

interactionAPI :: Proxy InteractionAPI
interactionAPI =
    Proxy

server :: Text -> TVar [Interaction] -> TVar DeviceTable -> Server InteractionAPI
server home queue deviceTable =
         return home
    :<|> postRequest queue
    :<|> registerDevice deviceTable

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
        home = maybe "Welcome 5seconds - Server Test" T.pack $
                 lookup "TUTORIAL_HOME" env
    queue <- emptyQueue
    deviceTable <- emptyDeviceTable
    run port $ serve interactionAPI $ server home queue deviceTable
