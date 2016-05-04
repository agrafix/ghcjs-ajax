{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module JavaScript.Ajax where

import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import qualified Data.Text as T

#ifdef __GHCJS__
import Data.JSString
import Data.JSString.Text (textToJSString)
import GHCJS.Types
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Foreign.Callback
#endif

sendRequest :: StdMethod -> T.Text -> Maybe T.Text -> Maybe T.Text -> (Status -> T.Text -> IO ()) -> IO ()
#ifdef __GHCJS__
sendRequest method url mBody mContentType callback =
    do jsCt <- toJSVal mContentType
       jsBody <- toJSVal mBody
       jsCallback <-
           asyncCallback2 $ \jsStatus jsInnerText ->
           do status <- mkStatus <$> fromJSValUnchecked jsStatus <*> pure ""
              text <- fromJSValUnchecked jsInnerText
              callback status text
       js_sendRequest (textToJSString url) jsMethod jsBody jsCt jsCallback
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
foreign import javascript unsafe
    "ghcjsajax$sendRequest($1, $2, $3, $4, $5)"
    js_sendRequest ::
          JSString -> JSString -> JSVal -> JSVal
          -> Callback (JSVal -> JSVal -> IO ()) -> IO ()
#endif
