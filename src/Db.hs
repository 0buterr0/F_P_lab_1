-- | Підключення до MySQL і виконання запитів.
module Db
  ( DbConfig (..)
  , loadConfig
  , openConnection
  , closeConnection
  , queryRows
  , queryAs
  , exec
  , MySQLConn
  , MySQLValue (..)
  , OK (..)
  ) where

import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.MySQL.Base
  ( ConnectInfo (..)
  , MySQLConn
  , MySQLValue (..)
  , OK (..)
  , close
  , connect
  , defaultConnectInfoMB4
  , execute
  , query
  )
import System.Environment (lookupEnv)
import qualified System.IO.Streams as Streams
import Text.Read (readMaybe)

import Db.Value (FromRow (..))

data DbConfig = DbConfig
  { cfgHost     :: String
  , cfgPort     :: Int
  , cfgUser     :: String
  , cfgPassword :: String
  , cfgDatabase :: String
  }

-- | Параметри підключення: зі змінних середовища або значення за замовчуванням.
loadConfig :: IO DbConfig
loadConfig = do
  host <- envOr "MYSQL_HOST" "127.0.0.1"
  port <- envOr "MYSQL_PORT" "3306"
  user <- envOr "MYSQL_USER" "lab"
  pass <- envOr "MYSQL_PASSWORD" "lab"
  db   <- envOr "MYSQL_DATABASE" "dept_resources"
  pure DbConfig
    { cfgHost     = host
    , cfgPort     = fromMaybe 3306 (readMaybe port)
    , cfgUser     = user
    , cfgPassword = pass
    , cfgDatabase = db
    }
  where
    envOr key def = fromMaybe def <$> lookupEnv key

openConnection :: DbConfig -> IO MySQLConn
openConnection c =
  connect defaultConnectInfoMB4
    { ciHost     = cfgHost c
    , ciPort     = fromIntegral (cfgPort c)
    , ciUser     = utf8 (cfgUser c)
    , ciPassword = utf8 (cfgPassword c)
    , ciDatabase = utf8 (cfgDatabase c)
    }
  where
    utf8 = TE.encodeUtf8 . T.pack

closeConnection :: MySQLConn -> IO ()
closeConnection = close

-- | SELECT з параметрами (?), результат — список рядків.
queryRows :: MySQLConn -> String -> [MySQLValue] -> IO [[MySQLValue]]
queryRows conn sql params = do
  (_, stream) <- query conn (fromString sql) params
  Streams.toList stream

-- | SELECT, рядки якого одразу перетворюються у записи потрібного типу.
queryAs :: FromRow a => MySQLConn -> String -> [MySQLValue] -> IO [a]
queryAs conn sql params = map fromRow <$> queryRows conn sql params

-- | INSERT / UPDATE / DELETE.
exec :: MySQLConn -> String -> [MySQLValue] -> IO OK
exec conn sql params = execute conn (fromString sql) params
