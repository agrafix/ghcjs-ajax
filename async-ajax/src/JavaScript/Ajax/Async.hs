module JavaScript.Ajax.Async
    ( sendRequestAsync
    , AjaxResponse(..)
    , StdMethod(..), RequestBody, ContentType
    , Async, wait, waitCatch, cancel
    )
where

import Control.Concurrent.Async
import JavaScript.Ajax
import qualified Data.Text as T

-- | Send an ajax request provided a HTTP-Method, a target url, optional a request
-- body and content type
sendRequestAsync :: StdMethod -> T.Text -> Maybe RequestBody -> Maybe ContentType -> IO (Async AjaxResponse)
sendRequestAsync method url body ct = async $ sendRequest method url body ct
