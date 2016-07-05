module Commander.Utils where

import Control.Applicative
import Control.Lens
import Network.AWS.EC2

import Data.Text (Text)
import qualified Data.Text as Text

getIPForInstance :: Instance -> Maybe Text
getIPForInstance inst = inst ^. insPublicIPAddress <|> inst ^. insPrivateIPAddress



