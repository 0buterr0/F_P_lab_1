{-# LANGUAGE ScopedTypeVariables #-}

-- | Введення даних з клавіатури.
--   Клас Field — як розібрати рядок у значення поля і як його показати.
--   Клас Editable — як ввести новий запис і як відкоригувати наявний.
module Input
  ( Field (..)
  , Editable (..)
  , trim
  , prompt
  , ask
  , askDef
  , askRef
  , askChoice
  , confirm
  , safely
  , nowLocal
  , today
  ) where

import Control.Exception (Handler (..), SomeException, catches, throwIO)
import Data.Char (isSpace, toLower)
import Data.List (isInfixOf)
import Data.Proxy (Proxy)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
  ( Day
  , LocalTime (..)
  , TimeOfDay (..)
  , defaultTimeLocale
  , formatTime
  , getZonedTime
  , parseTimeM
  , zonedTimeToLocalTime
  )
import System.Exit (ExitCode, exitSuccess)
import System.IO (hFlush, isEOF, stdout)
import Text.Read (readMaybe)

import Db (MySQLConn)
import Display (Display (..))
import Entity (Entity (..))

---------------------------------------------------------------------
-- Клас Field
---------------------------------------------------------------------

class Field a where
  parseField  :: String -> Maybe a
  renderField :: a -> String

instance Field Int where
  parseField  = readMaybe . trim
  renderField = show

-- | Обов'язковий текст (порожній рядок не приймається).
instance Field Text where
  parseField s =
    let t = T.pack (trim s)
    in  if T.null t then Nothing else Just t
  renderField = T.unpack

instance Field Day where
  parseField  = parseTimeM True defaultTimeLocale "%Y-%m-%d" . trim
  renderField = formatTime defaultTimeLocale "%Y-%m-%d"

instance Field LocalTime where
  parseField  = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" . trim
  renderField = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

-- | Необов'язкове поле: "-" означає «немає значення» (NULL).
instance Field a => Field (Maybe a) where
  parseField s
    | trim s == "-" = Just Nothing
    | otherwise     = Just <$> parseField s
  renderField = maybe "-" renderField

---------------------------------------------------------------------
-- Клас Editable
---------------------------------------------------------------------

class Editable a where
  -- | Введення нового запису (id = 0, його присвоїть БД).
  inputNew  :: MySQLConn -> IO a
  -- | Коригування: Enter залишає поточне значення поля.
  inputEdit :: MySQLConn -> a -> IO a

---------------------------------------------------------------------
-- Допоміжні функції введення
---------------------------------------------------------------------

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  eof <- isEOF
  if eof
    then putStrLn "" >> exitSuccess
    else getLine

-- | Запит значення, поки не буде введено коректне.
ask :: Field a => String -> IO a
ask label = do
  s <- prompt ("  " ++ label ++ ": ")
  case parseField s of
    Just v  -> pure v
    Nothing -> do
      putStrLn "  ! Некоректне значення, спробуйте ще раз."
      ask label

-- | Те саме, але з поточним значенням: порожній ввід залишає його.
askDef :: Field a => String -> a -> IO a
askDef label old = do
  s <- prompt ("  " ++ label ++ " [" ++ renderField old ++ "]: ")
  if null (trim s)
    then pure old
    else case parseField s of
      Just v  -> pure v
      Nothing -> do
        putStrLn "  ! Некоректне значення, спробуйте ще раз."
        askDef label old

-- | Вибір id запису з іншої таблиці (зовнішній ключ) з перевіркою існування.
askRef :: forall b. (Entity b, Display b) => MySQLConn -> Proxy b -> String -> Maybe Int -> IO Int
askRef conn p label mOld = do
  items <- findAll conn :: IO [b]
  if null items
    then throwIO (userError ("спочатку додайте записи в розділ «" ++ entityTitle p ++ "»"))
    else pure ()
  putStrLn ("  -- " ++ entityTitle p ++ ":")
  mapM_ (\x -> putStrLn ("     " ++ display x)) items
  let loop = do
        k <- maybe (ask label) (askDef label) mOld
        if k `elem` map getKey items
          then pure k
          else do
            putStrLn "  ! Запису з таким id немає."
            loop
  loop

-- | Вибір значення зі списку варіантів.
askChoice :: String -> [Text] -> Maybe Text -> IO Text
askChoice label opts mOld = do
  putStrLn ("  " ++ label ++ ":")
  mapM_ (\(i, o) -> putStrLn ("     " ++ show i ++ ". " ++ T.unpack o)) (zip [1 :: Int ..] opts)
  let loop = do
        s <- prompt ("  Номер варіанта" ++ maybe "" (\o -> " [" ++ T.unpack o ++ "]") mOld ++ ": ")
        case (trim s, mOld) of
          ("", Just o) -> pure o
          (t, _) -> case readMaybe t of
            Just n | n >= 1 && n <= length opts -> pure (opts !! (n - 1))
            _ -> do
              putStrLn "  ! Оберіть номер зі списку."
              loop
  loop

confirm :: String -> IO Bool
confirm q = do
  s <- prompt ("  " ++ q ++ " (т/н): ")
  pure (map toLower (trim s) `elem` ["т", "так", "y", "yes"])

-- | Виконати дію, перехопивши помилки (БД, введення), щоб програма не падала.
safely :: IO () -> IO ()
safely act =
  act `catches`
    [ Handler (\(e :: ExitCode) -> throwIO e)
    , Handler (\(e :: SomeException) -> putStrLn (friendly (show e)))
    ]
  where
    friendly msg
      | "1451" `isInfixOf` msg = "  ! Неможливо видалити/змінити: на запис посилаються інші таблиці."
      | "1452" `isInfixOf` msg = "  ! Порушення зв'язку: пов'язаного запису не існує."
      | "1062" `isInfixOf` msg = "  ! Такий запис уже існує (порушення унікальності)."
      | "3819" `isInfixOf` msg = "  ! Значення порушує обмеження таблиці (CHECK)."
      | "1406" `isInfixOf` msg = "  ! Занадто довге значення для поля."
      | otherwise              = "  ! Помилка: " ++ msg

nowLocal :: IO LocalTime
nowLocal = do
  LocalTime d (TimeOfDay h m _) <- zonedTimeToLocalTime <$> getZonedTime
  pure (LocalTime d (TimeOfDay h m 0))

today :: IO Day
today = localDay <$> nowLocal
