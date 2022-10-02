{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module BasicServer where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Database (fetchProductPG, fetchAllProductsPG, fetchAllProductsForFoodPG, fetchPrekoJoinaPG, createFoodPG, createProductPG, deleteProductPG, localConnString)
import           BasicSchema


type ProductsAPI = 
       "products" :> Capture "productid" Int64 :> Get '[JSON] Product
  :<|> "products" :> ReqBody '[JSON] Product :> Post '[JSON] Int64
  :<|> "products" :> "all" :> Get '[JSON] [Entity Product]
  :<|> "products" :> "fruit" :> Get '[JSON] [Entity Product]
  :<|> "products" :> "vegetables" :> Get '[JSON] [Entity Product]
  :<|> "products" :> "fastFood" :> Get '[JSON] [Entity Product]
  :<|> "products" :> "milkAndDairy" :> Get '[JSON] [Entity Product]
  :<|> "products" :> "meat" :> Get '[JSON] [Entity Product]
  :<|> "products" :> "candyAndSweets" :> Get '[JSON] [Entity Product]
  :<|> "products" :> "drinksAndBeverages" :> Get '[JSON] [Entity Product]
  :<|> "products" :> "oilAndFats" :> Get '[JSON] [Entity Product]
  :<|> "products" :> "nutsAndSeeds" :> Get '[JSON] [Entity Product]
  :<|> "products" :> "soups" :> Get '[JSON] [Entity Product]
  :<|> "products" :> "join" :> Get '[JSON] [Entity Product]
  :<|> "products" :> Capture "productid" Int64 :> Post '[JSON] () --Delete ZASAD

productsAPI :: Proxy ProductsAPI
productsAPI = Proxy :: Proxy ProductsAPI

fetchProductsHandler :: ConnectionString -> Int64 -> Handler Product
fetchProductsHandler connString fid = do
  maybeProduct <- liftIO $ fetchProductPG connString fid
  case maybeProduct of
    Just product -> return product
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find product with that ID" })

fetchAllFruit :: ConnectionString -> Handler [Entity Product]
fetchAllFruit connString = liftIO $ fetchAllProductsForFoodPG connString 1

fetchAllVegetables :: ConnectionString -> Handler [Entity Product]
fetchAllVegetables connString = liftIO $ fetchAllProductsForFoodPG connString 2

fetchAllFastFood :: ConnectionString -> Handler [Entity Product]
fetchAllFastFood connString = liftIO $ fetchAllProductsForFoodPG connString 3

fetchAllMilkAndDairy :: ConnectionString -> Handler [Entity Product]
fetchAllMilkAndDairy connString = liftIO $ fetchAllProductsForFoodPG connString 4

fetchAllMeat :: ConnectionString -> Handler [Entity Product]
fetchAllMeat connString = liftIO $ fetchAllProductsForFoodPG connString 5

fetchAllCandyAndSweets :: ConnectionString -> Handler [Entity Product]
fetchAllCandyAndSweets connString = liftIO $ fetchAllProductsForFoodPG connString 6

fetchAllDrinksAndBeverages :: ConnectionString -> Handler [Entity Product]
fetchAllDrinksAndBeverages connString = liftIO $ fetchAllProductsForFoodPG connString 7

fetchAllOilAndFats :: ConnectionString -> Handler [Entity Product]
fetchAllOilAndFats connString = liftIO $ fetchAllProductsForFoodPG connString 8

fetchAllNutsAndSeeds :: ConnectionString -> Handler [Entity Product]
fetchAllNutsAndSeeds connString = liftIO $ fetchAllProductsForFoodPG connString 9

fetchAllSoups :: ConnectionString -> Handler [Entity Product]
fetchAllSoups connString = liftIO $ fetchAllProductsForFoodPG connString 10

fetchAll :: ConnectionString -> Handler [Entity Product]
fetchAll connString = liftIO $ fetchAllProductsPG connString

createProductHandler :: ConnectionString -> Product -> Handler Int64
createProductHandler connString product = liftIO $ createProductPG connString product

createFoodHandler :: ConnectionString -> Food -> Handler Int64
createFoodHandler connString food = liftIO $ createFoodPG connString food

fetchPrekoJoinaHandler :: ConnectionString -> Handler [Entity Product]
fetchPrekoJoinaHandler connString = liftIO $ fetchPrekoJoinaPG localConnString

deleteFood :: ConnectionString -> Int64 -> Handler ()
deleteFood connString fid = liftIO $ deleteProductPG connString fid

productsServer :: ConnectionString -> Server ProductsAPI
productsServer connString = 
  (fetchProductsHandler connString) :<|> 
  (createProductHandler connString) :<|>
  (fetchAll connString) :<|>
  (fetchAllFruit connString) :<|>
  (fetchAllVegetables connString) :<|>
  (fetchAllFastFood connString) :<|>
  (fetchAllMilkAndDairy connString) :<|>
  (fetchAllMeat connString) :<|>
  (fetchAllCandyAndSweets connString) :<|>
  (fetchAllDrinksAndBeverages connString) :<|>
  (fetchAllOilAndFats connString) :<|>
  (fetchAllNutsAndSeeds connString) :<|>
  (fetchAllSoups connString) :<|>
  (fetchPrekoJoinaHandler connString) :<|>
  (deleteFood connString)

runServer :: IO ()
runServer = run 5000 (serve productsAPI (productsServer localConnString))

