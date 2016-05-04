module JavaScript.Ajax.Async
    ( sendRequestAsync
    , StdMethod(..), RequestBody, ContentType
    , Async, wait, waitCatch, cancel
    )
where

import JavaScript.Ajax
import qualified Data.Text as T
import Control.Concurrent.Async
import Control.Concurrent.MVar

-- | Send an ajax request provided a HTTP-Method, a target url, optional a request
-- body and content type
sendRequestAsync :: StdMethod -> T.Text -> Maybe RequestBody -> Maybe ContentType -> IO (Async (Status, T.Text))
sendRequestAsync method url body ct =
    async $
    do out <- newEmptyMVar
       sendRequest method url body ct (curry $ putMVar out)
       takeMVar out
