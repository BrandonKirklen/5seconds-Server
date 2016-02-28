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

{- Phone Number -}
newtype PhoneNumber = PhoneNumber String deriving (Generic, Show, Ord, Eq)
instance ToJSON PhoneNumber
instance FromJSON PhoneNumber

{- UUID -}
newtype UUID = UUID String deriving (Generic, Show)
instance ToJSON UUID
instance FromJSON UUID

{- Interaction -}
data Interaction = Interaction { 
    fromUser :: PhoneNumber,
    toUser :: PhoneNumber,
    blob :: Value
}  deriving (Generic, Show)
instance ToJSON Interaction
instance FromJSON Interaction

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

postRequest :: MonadIO m => TVar [Interaction] -> TVar DeviceTable -> Interaction -> m Bool
postRequest queue deviceTable interaction =
    liftIO $ do
      T.putStrLn $ (pack . show) interaction
      atomically $ do
        oldQueue <- readTVar queue
        let newQueue = interaction : oldQueue
        writeTVar queue newQueue)
        actualDeviceTable <- readTVar deviceTable
        case Map.lookup (toUser interaction)  of
          Just uuid -> return True
          Nothing -> return False

registerDevice :: MonadIO m => TVar DeviceTable -> Device -> m ()
registerDevice deviceTable device =
    liftIO $ do
      atomically $ do
        oldDeviceTable <- readTVar deviceTable
        let newDeviceTable = Map.insert (phoneNumber device) (uuid device) oldDeviceTable
        writeTVar deviceTable newDeviceTable
        return ()

type InteractionAPI =
         Get Text
    :<|> "request" :> ReqBody Interaction :> Post Bool
    :<|> "register" :> ReqBody Device :> Post ()

interactionAPI :: Proxy InteractionAPI
interactionAPI =
    Proxy

server :: Text -> TVar [Interaction] -> TVar DeviceTable -> Server InteractionAPI
server home queue deviceTable =
         return home
    :<|> postRequest queue deviceTable
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
