{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module JavaScript.Ajax.DataSource
    ( AjaxReq(..), RequestBody, ContentType
    , AjaxResponse(..), Status(..)
    , initGlobalState
    )
where

import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Exception
import Data.Hashable
import Data.Typeable
import Haxl.Core
import JavaScript.Ajax
import qualified Data.Text as T

data AjaxResponse
   = AjaxResponse
   { ar_status :: !Status
   , ar_body :: !T.Text
   } deriving (Show, Eq, Typeable)

data AjaxReq a where
    GetRequest :: T.Text -> AjaxReq AjaxResponse
    PostRequest :: T.Text -> RequestBody -> ContentType -> AjaxReq AjaxResponse
    PutRequest :: T.Text -> RequestBody -> ContentType -> AjaxReq AjaxResponse

deriving instance Eq (AjaxReq a)
deriving instance Show (AjaxReq a)

instance Hashable (AjaxReq a) where
  hashWithSalt s (GetRequest url) = hashWithSalt s (0::Int, url)
  hashWithSalt s (PostRequest url rb ct) = hashWithSalt s (1::Int, url, rb, ct)
  hashWithSalt s (PutRequest url rb ct) = hashWithSalt s (2::Int, url, rb, ct)

instance Show1 AjaxReq where show1 = show

instance StateKey AjaxReq where
    data State AjaxReq =
        AjaxReqState
        { ars_maxConcurrentReq :: !Int
        }

instance DataSourceName AjaxReq where
    dataSourceName _ = "Ajax"

instance DataSource u AjaxReq where
    fetch = fetchAjax

initGlobalState :: Int -> State AjaxReq
initGlobalState i = AjaxReqState { ars_maxConcurrentReq = i }

fetchAjax :: State AjaxReq -> Flags -> u -> [BlockedFetch AjaxReq] -> PerformFetch
fetchAjax st _ _ bfs =
    AsyncFetch $ \inner ->
    do sem <- newQSem (ars_maxConcurrentReq st)
       asyncs <- mapM (fetchAsync sem) bfs
       inner
       mapM_ wait asyncs

fetchAsync :: QSem -> BlockedFetch AjaxReq -> IO (Async ())
fetchAsync sem (BlockedFetch req rvar) =
    async $
    bracket_ (waitQSem sem) (signalQSem sem) $
    do e <- Control.Exception.try $ fetchReq rvar req
       case e of
         Left ex -> putFailure rvar (ex :: SomeException)
         Right () -> pure ()

fetchReq :: ResultVar a -> AjaxReq a -> IO ()
fetchReq rvar req =
    case req of
        GetRequest url ->
            withVar $ \run -> sendRequest GET url Nothing Nothing (run rvar)
        PostRequest url body ct ->
            withVar $ \run -> sendRequest POST url (Just body) (Just ct) (run rvar)
        PutRequest url body ct ->
            withVar $ \run -> sendRequest PUT url (Just body) (Just ct) (run rvar)
    where
        withVar run =
            run (\v a b -> putSuccess v (AjaxResponse a b))
