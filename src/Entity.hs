{-# LANGUAGE ScopedTypeVariables #-}

-- | Клас Entity — таблиця БД, з якою пов'язаний тип Haskell.
--   Операції CRUD реалізовані один раз як методи за замовчуванням,
--   тому кожному екземпляру достатньо описати назву таблиці та колонки.
module Entity
  ( Entity (..)
  , selectSql
  ) where

import Data.List (intercalate)
import Data.Proxy (Proxy (..))

import Db (MySQLConn, MySQLValue, OK (..), exec, queryAs)
import Db.Value (FromRow, ToValue (..))

class FromRow a => Entity a where
  -- | Назва для меню (напр. "Автори").
  entityTitle :: Proxy a -> String
  -- | Назва таблиці в MySQL.
  tableName   :: Proxy a -> String
  -- | Первинний ключ.
  keyColumn   :: Proxy a -> String
  keyColumn _ = "id"
  -- | Колонки без ключа, у тому ж порядку, що й toRow.
  dataColumns :: Proxy a -> [String]
  -- | Значення колонок dataColumns.
  toRow       :: a -> [MySQLValue]
  getKey      :: a -> Int

  -- | Усі записи таблиці.
  findAll :: MySQLConn -> IO [a]
  findAll conn = queryAs conn (selectSql p ++ " ORDER BY " ++ keyColumn p) []
    where
      p = Proxy :: Proxy a

  -- | Пошук за первинним ключем.
  findById :: MySQLConn -> Int -> IO (Maybe a)
  findById conn k = do
    xs <- queryAs conn (selectSql p ++ " WHERE " ++ keyColumn p ++ " = ?") [toValue k]
    pure $ case xs of
      (x : _) -> Just x
      []      -> Nothing
    where
      p = Proxy :: Proxy a

  -- | Додавання запису; повертає id, присвоєний AUTO_INCREMENT.
  insert :: MySQLConn -> a -> IO Int
  insert conn x = do
    ok <- exec conn sql (toRow x)
    pure (fromIntegral (okLastInsertID ok))
    where
      p    = proxyOf x
      cols = dataColumns p
      sql  = "INSERT INTO " ++ tableName p
          ++ " (" ++ intercalate ", " cols ++ ") VALUES ("
          ++ intercalate ", " (map (const "?") cols) ++ ")"

  -- | Коригування запису (за його ключем).
  update :: MySQLConn -> a -> IO ()
  update conn x = do
    _ <- exec conn sql (toRow x ++ [toValue (getKey x)])
    pure ()
    where
      p    = proxyOf x
      sets = intercalate ", " [c ++ " = ?" | c <- dataColumns p]
      sql  = "UPDATE " ++ tableName p ++ " SET " ++ sets
          ++ " WHERE " ++ keyColumn p ++ " = ?"

  -- | Видалення за ключем; True, якщо запис було видалено.
  delete :: MySQLConn -> Proxy a -> Int -> IO Bool
  delete conn p k = do
    ok <- exec conn ("DELETE FROM " ++ tableName p ++ " WHERE " ++ keyColumn p ++ " = ?") [toValue k]
    pure (okAffectedRows ok > 0)

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

selectSql :: Entity a => Proxy a -> String
selectSql p =
  "SELECT " ++ intercalate ", " (keyColumn p : dataColumns p) ++ " FROM " ++ tableName p
