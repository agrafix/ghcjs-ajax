{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveDataTypeable #-}
module JavaScript.Ajax
    ( sendRequest, StdMethod(..), Status(..)
    , RequestBody, ContentType
    , AjaxResponse(..)
    )
where

import Data.Aeson
import Data.Typeable
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import qualified Data.Text as T

#ifdef __GHCJS__
import Data.JSString
import Data.JSString.Text (textToJSString)
import GHCJS.Types
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
#endif

type RequestBody = T.Text
type ContentType = T.Text

data AjaxResponse
   = AjaxResponse
   { ar_status :: !Status
   , ar_body :: !T.Text
   } deriving (Show, Eq, Typeable)

instance FromJSON AjaxResponse where
    parseJSON =
        withObject "ajax_response" $ \o ->
        do st <- mkStatus <$> o .: "status" <*> pure ""
           bdy <- o .: "body"
           pure $ AjaxResponse st bdy

-- | Send an ajax request provided a HTTP-Method, a target url, optional a request
-- body and content type and a completion callback
sendRequest :: StdMethod -> T.Text -> Maybe RequestBody -> Maybe ContentType -> IO AjaxResponse
#ifdef __GHCJS__
sendRequest method url mBody mContentType =
    do jsCt <- toJSVal mContentType
       jsBody <- toJSVal mBody
       jsRes <- js_sendRequest (textToJSString url) jsMethod jsBody jsCt
       val <- fromJSValUnchecked jsRes
       case fromJSON val of
         Error msg -> fail $ "Internal error (JavaScript.Ajax): " ++ msg
         Success v -> pure v
    where
      jsMethod =
          case method of
            GET -> "GET"
            POST -> "POST"
            HEAD -> "HEAD"
            PUT -> "PUT"
            DELETE -> "DELETE"
            TRACE -> "TRACE"
            CONNECT -> "CONNECT"
            OPTIONS -> "OPTIONS"
            PATCH -> "PATCH"
#else
sendRequest = undefined
#endif

#ifdef __GHCJS__
foreign import javascript interruptible
    "ghcjsajax$sendRequest($1, $2, $3, $4, $c);"
    js_sendRequest :: JSString -> JSString -> JSVal -> JSVal -> IO JSVal
#endif
