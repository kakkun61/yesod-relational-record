{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module is to use haskell-relational-record with Yesod.
--
-- 'YesodRelational' instance example:
--
-- @
--   import Yesod.Relational
--   import Database.HDBC.MySQL (Connection, MySQLConnectInfo, connectMySQL)
--
--   connectDB :: IO Connection
--   connectDB = connectMySQL MySQLConnectInfo
--
--   instance YesodRelational YourApp where
--       data YesodRelationalConnection YourApp = Connection
--       runRelational action = do
--           con <- liftIO connectDB
--           runReaderT action con
-- @
module Yesod.Relational
    ( -- * Monads
      YesodRelational (..)
    , YesodRelationalMonad
      -- * Generalized Statement
    , prepareNoFetch
    , executeBound
    , executeNoFetch
      -- * Select
    , prepareQuery
    , fetch
    , runQuery
    , runQuery'
      -- * Insert Values
    , prepareInsert
    , runInsert
      -- * Insert Select Results
    , prepareInsertQuery
    , runInsertQuery
      -- * Update
    , prepareUpdate
    , runUpdate
      -- * Delete
    , prepareDelete
    , runDelete
      -- * Update by Key
    , prepareKeyUpdate
    , runKeyUpdate
      -- * General
    , run
    ) where

import           Database.HDBC.Types (IConnection)
import           Database.HDBC.Record.Statement (BoundStatement, ExecutedStatement, PreparedStatement)
import qualified Database.HDBC.Record.Statement as R (prepareNoFetch, executeBound, executeNoFetch)
import           Database.HDBC.Record.Query (PreparedQuery)
import qualified Database.HDBC.Record.Query as R (prepareQuery, fetch, runQuery, runQuery')
import           Database.HDBC.Record.Update (PreparedUpdate)
import qualified Database.HDBC.Record.Update as R (prepareUpdate, runUpdate)
import           Database.HDBC.Record.Insert (PreparedInsert)
import qualified Database.HDBC.Record.Insert as R (prepareInsert, runInsert)
import           Database.HDBC.Record.InsertQuery (PreparedInsertQuery)
import qualified Database.HDBC.Record.InsertQuery as R (prepareInsertQuery, runInsertQuery)
import           Database.HDBC.Record.Delete (PreparedDelete)
import qualified Database.HDBC.Record.Delete as R (prepareDelete, runDelete)
import           Database.HDBC.Record.KeyUpdate (PreparedKeyUpdate)
import qualified Database.HDBC.Record.KeyUpdate as R (prepareKeyUpdate, runKeyUpdate)
import           Database.HDBC.SqlValue (SqlValue)
import           Database.Relational.Query (Query, Update, Insert, Delete, KeyUpdate, UntypeableNoFetch, InsertQuery)
import           Database.Record.ToSql (ToSql)
import           Database.Record.FromSql (FromSql)

import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.IO.Class (liftIO)

import Yesod.Core.Handler (HandlerT)

type YesodRelationalMonad site = ReaderT (YesodRelationalConnection site) (HandlerT site IO)

class Monad (YesodRelationalMonad site) => YesodRelational site where
    type YesodRelationalConnection site :: *
    runRelational :: YesodRelationalMonad site a -> HandlerT site IO a

-- | 'Database.HDBC.Record.Statement.prepareNoFetch'
prepareNoFetch :: (IConnection (YesodRelationalConnection site), UntypeableNoFetch s) => s p -> YesodRelationalMonad site (PreparedStatement p ())
prepareNoFetch statement = ask >>= \conn -> liftIO $ R.prepareNoFetch conn statement

-- | 'Database.HDBC.Record.Statement.executeBound'
executeBound :: BoundStatement a -> YesodRelationalMonad site (ExecutedStatement a)
executeBound = liftIO . R.executeBound

-- | 'Database.HDBC.Record.Statement.executeNoFetch'
executeNoFetch :: BoundStatement () -> YesodRelationalMonad site Integer
executeNoFetch = liftIO . R.executeNoFetch

-- | 'Database.HDBC.Record.Query.prepareQuery'
prepareQuery :: IConnection (YesodRelationalConnection site) => Query p a -> YesodRelationalMonad site (PreparedQuery p a)
prepareQuery query = ask >>= \conn -> liftIO $ R.prepareQuery conn query

-- | 'Database.HDBC.Record.Query.fetch'
fetch :: FromSql SqlValue a => ExecutedStatement a -> YesodRelationalMonad site (Maybe a)
fetch = liftIO . R.fetch

-- | 'Database.HDBC.Record.Query.runQuery'
runQuery :: (IConnection (YesodRelationalConnection site), ToSql SqlValue p, FromSql SqlValue a) => Query p a -> p -> YesodRelationalMonad site [a]
runQuery query param = ask >>= \conn -> liftIO $ R.runQuery conn query param

-- | 'Database.HDBC.Record.Query.runQuery''
runQuery' :: (IConnection (YesodRelationalConnection site), ToSql SqlValue p, FromSql SqlValue a) => Query p a -> p -> YesodRelationalMonad site [a]
runQuery' query param = ask >>= \conn -> liftIO $ R.runQuery' conn query param

-- | 'Database.HDBC.Record.Insert.prepareInsert'
prepareInsert :: IConnection (YesodRelationalConnection site) => Insert a -> YesodRelationalMonad site (PreparedInsert a)
prepareInsert query = ask >>= \conn -> liftIO $ R.prepareInsert conn query

-- | 'Database.HDBC.Record.Insert.runInsert'
runInsert :: (IConnection (YesodRelationalConnection site), ToSql SqlValue p) => Insert p -> p -> YesodRelationalMonad site Integer
runInsert insert param = ask >>= \conn -> liftIO $ R.runInsert conn insert param

-- | 'Database.HDBC.Record.InsertQuery.prepareInsertQuery'
prepareInsertQuery :: IConnection (YesodRelationalConnection site) => InsertQuery p -> YesodRelationalMonad site (PreparedInsertQuery p)
prepareInsertQuery query = ask >>= \conn -> liftIO $ R.prepareInsertQuery conn query

-- | 'Database.HDBC.Record.InsertQuery.runInsertQuery'
runInsertQuery :: (IConnection (YesodRelationalConnection site), ToSql SqlValue p) => InsertQuery p -> p -> YesodRelationalMonad site Integer
runInsertQuery query param = ask >>= \conn -> liftIO $ R.runInsertQuery conn query param

-- | 'Database.HDBC.Record.Update.prepareUpdate'
prepareUpdate :: IConnection (YesodRelationalConnection site) => Update p -> YesodRelationalMonad site (PreparedUpdate p)
prepareUpdate query = ask >>= \conn -> liftIO $ R.prepareUpdate conn query

-- | 'Database.HDBC.Record.Update.runUpdate'
runUpdate :: (IConnection (YesodRelationalConnection site), ToSql SqlValue p) => Update p -> p -> YesodRelationalMonad site Integer
runUpdate update param = ask >>= \conn -> liftIO $ R.runUpdate conn update param

-- | 'Database.HDBC.Record.Delete.prepareDelete'
prepareDelete :: IConnection (YesodRelationalConnection site) => Delete p -> YesodRelationalMonad site (PreparedDelete p)
prepareDelete query = ask >>= \conn -> liftIO $ R.prepareDelete conn query

-- | 'Database.HDBC.Record.Delete.runDelete'
runDelete :: (IConnection (YesodRelationalConnection site), ToSql SqlValue p) => Delete p -> p -> YesodRelationalMonad site Integer
runDelete query param = ask >>= \conn -> liftIO $ R.runDelete conn query param

-- | 'Database.HDBC.Record.KeyUpdate.prepareKeyUpdate'
prepareKeyUpdate :: IConnection (YesodRelationalConnection site) => KeyUpdate p a -> YesodRelationalMonad site (PreparedKeyUpdate p a)
prepareKeyUpdate query = ask >>= \conn -> liftIO $ R.prepareKeyUpdate conn query

-- | 'Database.HDBC.Record.KeyUpdate.runKeyUpdate'
runKeyUpdate :: (IConnection (YesodRelationalConnection site), ToSql SqlValue a) => KeyUpdate p a -> a -> YesodRelationalMonad site Integer
runKeyUpdate query param = ask >>= \conn -> liftIO $ R.runKeyUpdate conn query param

run :: (IConnection (YesodRelationalConnection site)) => (YesodRelationalConnection site -> IO a) -> YesodRelationalMonad site a
run f = ask >>= (liftIO . f)
