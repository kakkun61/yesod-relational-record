{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


-- | A counterpart to 'Yesod.Auth.YesodAuthPersist' using [haskell-relational-record](http://khibino.github.io/haskell-relational-record/).
module Yesod.Auth.Relational
    ( YesodAuthRelational (..)
    , defaultMaybeAuthId
    , maybeAuth
    , maybeAuthPair
    , requireAuth
    , requireAuthPair
    ) where

import Control.Monad.Trans.Maybe
import Data.Typeable (Typeable)

import Yesod.Core
import Yesod.Auth (YesodAuth, AuthId, maybeAuthId, credsKey)
import Yesod.Relational

class (YesodAuth master, YesodRelational master) => YesodAuthRelational master where
    type AuthModel master :: *

    getAuthModel :: AuthId master -> HandlerT master IO (Maybe (AuthModel master))

defaultMaybeAuthId
    :: (YesodAuthRelational master, Typeable (AuthModel master))
    => HandlerT master IO (Maybe (AuthId master))
defaultMaybeAuthId = runMaybeT $ do
    s   <- MaybeT $ lookupSession credsKey
    aid <- MaybeT $ return $ fromPathPiece s
    _   <- MaybeT $ cachedAuth aid
    return aid

cachedAuth
    :: (YesodAuthRelational master, Typeable (AuthModel master))
    => AuthId master -> HandlerT master IO (Maybe (AuthModel master))
cachedAuth
    = fmap unCachedMaybeAuth
    . cached
    . fmap CachedMaybeAuth
    . getAuthModel

newtype CachedMaybeAuth val = CachedMaybeAuth { unCachedMaybeAuth :: Maybe val }
    deriving Typeable

maybeAuth :: (YesodAuthRelational master, Typeable (AuthModel master))
          => HandlerT master IO (Maybe (AuthModel master))
maybeAuth = runMaybeT $ do
    (_, ae) <- MaybeT maybeAuthPair
    return ae

maybeAuthPair :: (YesodAuthRelational master, Typeable (AuthModel master))
              => HandlerT master IO (Maybe (AuthId master, AuthModel master))
maybeAuthPair = runMaybeT $ do
    aid <- MaybeT maybeAuthId
    ae  <- MaybeT $ cachedAuth aid
    return (aid, ae)

requireAuth :: (YesodAuthRelational master, Typeable (AuthModel master))
            => HandlerT master IO (AuthModel master)
requireAuth = maybeAuth >>= maybe handleAuthLack return

requireAuthPair :: (YesodAuthRelational master, Typeable (AuthModel master))
                => HandlerT master IO (AuthId master, AuthModel master)
requireAuthPair = maybeAuthPair >>= maybe handleAuthLack return

handleAuthLack :: Yesod master => HandlerT master IO a
handleAuthLack = do
    aj <- acceptsJson
    if aj then notAuthenticated else redirectLogin

redirectLogin :: Yesod master => HandlerT master IO a
redirectLogin = do
    y <- getYesod
    setUltDestCurrent
    case authRoute y of
        Just z -> redirect z
        Nothing -> permissionDenied "Please configure authRoute"
