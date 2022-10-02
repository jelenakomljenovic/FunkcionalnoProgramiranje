{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad.Logger
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Int (Int64)
import           Database.Persist hiding ((==.))
import           Database.Persist.Postgresql hiding ((==.))
import           Database.Esqueleto (select, from, where_, (^.), val, (==.), on,
                                     InnerJoin(..), limit, orderBy, desc)

import BasicSchema

type PGInfo = ConnectionString

localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=nutrition password=jelena123"

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $ \backend ->
   runReaderT action backend

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False


fetchProductPG :: PGInfo -> Int64 -> IO (Maybe Product)
fetchProductPG connString fid = runAction connString (get (toSqlKey fid))

fetchAllProductsForFoodPG :: PGInfo -> Int64  -> IO [Entity Product]
fetchAllProductsForFoodPG connString lid = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Product]
    fetchAction = select . from $ \products2 -> do
      where_ (products2 ^. ProductFoodId ==. val (toSqlKey lid))
      return products2


fetchAllProductsPG :: PGInfo -> IO [Entity Product]
fetchAllProductsPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Product]
    fetchAction = select . from $ \products2 -> do
      return products2


fetchPrekoJoinaPG :: PGInfo -> IO [Entity Product]
fetchPrekoJoinaPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Product]
    fetchAction = select . from $ \(food `InnerJoin` products) -> do
      on (food ^. FoodId  ==. products ^. ProductFoodId)
      orderBy [desc (products ^. ProductKcal)]
      limit 10
      return products

createProductPG :: PGInfo -> Product -> IO Int64 
createProductPG connString product = fromSqlKey <$> runAction connString (insert product)

createFoodPG :: PGInfo -> Food  -> IO Int64
createFoodPG connString food = fromSqlKey <$> runAction connString (insert food)

deleteProductPG :: PGInfo -> Int64 -> IO ()
deleteProductPG connString fid = runAction connString (delete productKey)
  where
    productKey :: Key Product
    productKey = toSqlKey fid
