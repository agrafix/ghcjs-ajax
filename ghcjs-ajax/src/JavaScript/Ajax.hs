{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module JavaScript.Ajax
    ( sendRequest, StdMethod(..), Status(..)
    , RequestBody, ContentType
    )
where

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

type RequestBody = T.Text
type ContentType = T.Text

-- | Send an ajax request provided a HTTP-Method, a target url, optional a request
-- body and content type and a completion callback
sendRequest :: StdMethod -> T.Text -> Maybe RequestBody -> Maybe ContentType -> (Status -> T.Text -> IO ()) -> IO ()
#ifdef __GHCJS__
sendRequest method url mBody mContentType callback =
    mdo jsCt <- toJSVal mContentType
        jsBody <- toJSVal mBody
        jsCallback <-
            asyncCallback2 $ \jsStatus jsInnerText ->
            do status <- mkStatus <$> fromJSValUnchecked jsStatus <*> pure ""
               text <- fromJSValUnchecked jsInnerText
               callback status text
               releaseCallback jsCallback
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
