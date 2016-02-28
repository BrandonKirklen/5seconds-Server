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

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Aeson
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics

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
data Interaction = Interaction { 
    requesterId :: UserId, 
    requestMessage :: Request,
    responderId :: UserId,
    responseMessage :: Response
}  deriving (Generic, Show)
instance ToJSON Interaction
instance FromJSON Interaction

newtype Note = Note
    { contents :: Text
    }
  deriving (Generic, Show)

instance FromJSON Note
instance ToJSON Note


emptyNotes :: IO (TVar [Note])
emptyNotes =
    newTVarIO []

getNotes :: MonadIO m => TVar [Note] -> m [Note]
getNotes notes =
    liftIO $ readTVarIO notes

postNote :: MonadIO m => TVar [Note] -> Note -> m [Note]
postNote notes note =
    liftIO $ do
      T.putStrLn $ contents note
      atomically $ do
        oldNotes <- readTVar notes
        let newNotes = note : oldNotes
        writeTVar notes newNotes
        return newNotes


type NoteAPI =
         Get Text
    :<|> "notes" :> Get [Note]
    :<|> "notes" :> ReqBody Note :> Post [Note]

noteAPI :: Proxy NoteAPI
noteAPI =
    Proxy

server :: Text -> TVar [Note] -> Server NoteAPI
server home notes =
         return home
    :<|> getNotes notes
    :<|> postNote notes


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    env <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
        home = maybe "Welcome 5seconds - Server Test" T.pack $
                 lookup "TUTORIAL_HOME" env
    notes <- emptyNotes
    run port $ serve noteAPI $ server home notes
