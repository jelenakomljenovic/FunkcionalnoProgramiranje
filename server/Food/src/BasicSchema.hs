{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module BasicSchema where

import           Data.Aeson
import           Data.Aeson.Types
import           Database.Persist (Entity(..), Entity)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)


PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Food sql=food
    name Text
    UniqueTitle name
    deriving Show Read Eq

  Product sql=products
    name Text
    thumbnail Text
    kcal Int
    proteins Int
    fat Int
    carbohydrates Int
    link Text
    foodId FoodId
    UniqueText name
    deriving Show Read Eq
|]

instance ToJSON (Entity Product) where
  toJSON (Entity fid product) = object $
    "id" .= (fromSqlKey fid) : productPairs product

instance ToJSON Product where
  toJSON product = object (productPairs product)

productPairs :: Product -> [Pair]
productPairs product =
  [ "name" .= productName product
  , "thumbnail" .= productThumbnail product
  , "kcal" .= productKcal product
  , "proteins" .= productProteins product
  , "fat" .= productFat product
  , "carbohydrates" .= productCarbohydrates product
  , "link" .= productLink product
  , "foodId" .= productFoodId product
  ]

instance FromJSON (Entity Product) where
  parseJSON = withObject "Product Entity" $ \o -> do
    product <- parseProduct o
    fid <- o .: "id"
    return $ Entity (toSqlKey fid) product


instance FromJSON Product where
  parseJSON = withObject "Product" parseProduct
  

parseProduct :: Object -> Parser Product
parseProduct o = do
  uName <- o .: "name"
  uThumbnail <- o .: "thumbnail"
  uKcal <- o .: "kcal"
  uProteins <- o .: "proteins"
  uFat <- o .: "fat"
  uCarbohydrates <- o .: "carbohydrates"
  uLink <- o .: "link"
  uFoodId <- o .: "foodId"
  return Product
    { productName = uName
    , productThumbnail = uThumbnail
    , productKcal = uKcal
    , productProteins = uProteins
    , productFat = uFat
    , productCarbohydrates = uCarbohydrates
    , productLink = uLink
    , productFoodId = uFoodId
    }

instance ToJSON (Entity Food) where
  toJSON (Entity lid food) = object $
    "id" .= (fromSqlKey lid) : foodPairs food

instance ToJSON Food where
  toJSON food = object (foodPairs food)

foodPairs :: Food -> [Pair]
foodPairs food =
  [ "name" .= foodName food
  ]

instance FromJSON (Entity Food) where
  parseJSON = withObject "Food Entity" $ \o -> do
    food <- parseFood o
    lid <- o .: "id"
    return $ Entity (toSqlKey lid) food


instance FromJSON Food where
  parseJSON = withObject "Food" parseFood

parseFood :: Object -> Parser Food
parseFood o = do
  uName <- o .: "name"
  return Food
    { foodName = uName
    }