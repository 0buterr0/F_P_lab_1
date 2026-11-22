-- | Перетворення між значеннями MySQL та типами Haskell.
--   Тут визначені три класи типів: FromValue, ToValue, FromRow.
module Db.Value
  ( FromValue (..)
  , ToValue (..)
  , FromRow (..)
  , intOf
  , valueText
  , showValue
  , badRow
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (Day, LocalTime (..), defaultTimeLocale, formatTime, midnight)
import Database.MySQL.Base (MySQLValue (..))

-- | Значення з БД -> значення Haskell.
class FromValue a where
  fromValue :: MySQLValue -> a

-- | Значення Haskell -> параметр SQL-запиту.
class ToValue a where
  toValue :: a -> MySQLValue

-- | Рядок результату запиту -> запис Haskell.
class FromRow a where
  fromRow :: [MySQLValue] -> a

-- | Будь-який цілочисельний тип MySQL -> Int.
intOf :: MySQLValue -> Maybe Int
intOf v = case v of
  MySQLInt8 x   -> Just (fromIntegral x)
  MySQLInt8U x  -> Just (fromIntegral x)
  MySQLInt16 x  -> Just (fromIntegral x)
  MySQLInt16U x -> Just (fromIntegral x)
  MySQLInt32 x  -> Just (fromIntegral x)
  MySQLInt32U x -> Just (fromIntegral x)
  MySQLInt64 x  -> Just (fromIntegral x)
  MySQLInt64U x -> Just (fromIntegral x)
  MySQLYear x   -> Just (fromIntegral x)
  MySQLBit x    -> Just (fromIntegral x)
  _             -> Nothing

-- | Текстове значення (TEXT/VARCHAR можуть прийти як Text або як байти UTF-8).
valueText :: MySQLValue -> Text
valueText v = case v of
  MySQLText t  -> t
  MySQLBytes b -> decodeUtf8With lenientDecode b
  MySQLNull    -> T.empty
  other        -> T.pack (showValue other)

-- | Значення для виведення у звітах.
showValue :: MySQLValue -> String
showValue v = case v of
  MySQLNull        -> "-"
  MySQLText t      -> T.unpack t
  MySQLBytes b     -> T.unpack (decodeUtf8With lenientDecode b)
  MySQLDate d      -> formatTime defaultTimeLocale "%Y-%m-%d" d
  MySQLDateTime t  -> formatTime defaultTimeLocale "%Y-%m-%d %H:%M" t
  MySQLTimeStamp t -> formatTime defaultTimeLocale "%Y-%m-%d %H:%M" t
  _                -> maybe (show v) show (intOf v)

badRow :: String -> [MySQLValue] -> a
badRow table row = error ("Неочікуваний формат рядка (" ++ table ++ "): " ++ show row)

---------------------------------------------------------------------
-- Екземпляри (instances)
---------------------------------------------------------------------

instance FromValue Int where
  fromValue v = case intOf v of
    Just n  -> n
    Nothing -> error ("Очікувалось ціле число, отримано: " ++ show v)

instance FromValue Text where
  fromValue = valueText

instance FromValue Day where
  fromValue (MySQLDate d)      = d
  fromValue (MySQLDateTime t)  = localDay t
  fromValue (MySQLTimeStamp t) = localDay t
  fromValue v                  = error ("Очікувалась дата, отримано: " ++ show v)

instance FromValue LocalTime where
  fromValue (MySQLDateTime t)  = t
  fromValue (MySQLTimeStamp t) = t
  fromValue (MySQLDate d)      = LocalTime d midnight
  fromValue v                  = error ("Очікувались дата і час, отримано: " ++ show v)

-- | NULL -> Nothing, інакше Just.
instance FromValue a => FromValue (Maybe a) where
  fromValue MySQLNull = Nothing
  fromValue v         = Just (fromValue v)

instance ToValue Int where
  toValue = MySQLInt64 . fromIntegral

instance ToValue Text where
  toValue = MySQLText

instance ToValue Day where
  toValue = MySQLDate

instance ToValue LocalTime where
  toValue = MySQLDateTime

instance ToValue a => ToValue (Maybe a) where
  toValue = maybe MySQLNull toValue
