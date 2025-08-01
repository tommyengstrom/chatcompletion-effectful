module ChatCompletion.Providers.Google.API where

import ChatCompletion.Providers.Google.Types
import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.API qualified as Servant

-- | Servant API definition for Google Gemini
type GeminiAPI =
    "v1beta"
        Servant.:> "models"
        Servant.:> CaptureAll "segments" Text
        Servant.:> QueryParam' '[Required] "key" Text
        Servant.:> ReqBody '[JSON] GeminiChatRequest
        Servant.:> Post '[JSON] GeminiChatResponse

-- | API proxy
geminiAPI :: Proxy GeminiAPI
geminiAPI = Proxy
