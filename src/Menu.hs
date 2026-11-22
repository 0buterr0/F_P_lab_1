{-# LANGUAGE ScopedTypeVariables #-}

-- | Меню програми. entityMenu — одна поліморфна функція, що працює з будь-яким
--   типом, який має екземпляри Entity, Display і Editable.
module Menu
  ( mainMenu
  , entityMenu
  ) where

import Data.Proxy (Proxy (..))

import Db (MySQLConn)
import Display
import Entity
import Input
import Model
import Search

entityMenu :: forall a. (Entity a, Display a, Editable a) => MySQLConn -> Proxy a -> IO ()
entityMenu conn p = loop
  where
    loop = do
      putStrLn ""
      putStrLn ("=== " ++ entityTitle p ++ " ===")
      putStrLn "1. Показати всі записи"
      putStrLn "2. Переглянути запис за id"
      putStrLn "3. Додати запис"
      putStrLn "4. Редагувати запис"
      putStrLn "5. Видалити запис"
      putStrLn "0. Назад"
      c <- prompt "> "
      case trim c of
        "1" -> safely showAll >> loop
        "2" -> safely showOne >> loop
        "3" -> safely addNew >> loop
        "4" -> safely editOne >> loop
        "5" -> safely deleteOne >> loop
        "0" -> pure ()
        _   -> putStrLn "  ! Невідомий пункт меню." >> loop

    showAll = do
      xs <- findAll conn :: IO [a]
      if null xs
        then putStrLn "  (таблиця порожня)"
        else mapM_ (putStrLn . ("  " ++) . display) xs
      putStrLn ("  Усього записів: " ++ show (length xs))

    showOne = do
      k <- ask "id"
      mx <- findById conn k :: IO (Maybe a)
      maybe (putStrLn "  Запис не знайдено.") (putStr . displayFull) mx

    addNew = do
      putStrLn ("  Новий запис: " ++ entityTitle p)
      x <- inputNew conn :: IO a
      k <- insert conn x
      putStrLn ("  Запис додано, id = " ++ show k)

    editOne = do
      k <- ask "id запису для редагування"
      mx <- findById conn k :: IO (Maybe a)
      case mx of
        Nothing -> putStrLn "  Запис не знайдено."
        Just x -> do
          putStr (displayFull x)
          putStrLn "  (Enter — залишити поточне значення, \"-\" — очистити необов'язкове поле)"
          x' <- inputEdit conn x
          update conn x'
          putStrLn "  Зміни збережено."

    deleteOne = do
      k <- ask "id запису для видалення"
      mx <- findById conn k :: IO (Maybe a)
      case mx of
        Nothing -> putStrLn "  Запис не знайдено."
        Just x -> do
          putStrLn ("  " ++ display x)
          ok <- confirm "Видалити цей запис?"
          if ok
            then do
              deleted <- delete conn p k
              putStrLn (if deleted then "  Запис видалено." else "  Нічого не видалено.")
            else putStrLn "  Скасовано."

mainMenu :: MySQLConn -> IO ()
mainMenu conn = loop
  where
    loop = do
      putStrLn ""
      putStrLn "=============================================="
      putStrLn "   ІНФОРМАЦІЙНІ РЕСУРСИ КАФЕДРИ"
      putStrLn "=============================================="
      putStrLn " 1. Інформаційні ресурси"
      putStrLn " 2. Автори"
      putStrLn " 3. Автори ресурсів (зв'язки)"
      putStrLn " 4. Адреси ресурсів у мережі"
      putStrLn " 5. Види ресурсів"
      putStrLn " 6. Призначення ресурсів"
      putStrLn " 7. Користувачі"
      putStrLn " 8. Статистика використання (журнал звернень)"
      putStrLn " 9. Пошук ресурсів"
      putStrLn "10. Звіти зі статистики використання"
      putStrLn " 0. Вихід"
      c <- prompt "> "
      case trim c of
        "1"  -> entityMenu conn (Proxy :: Proxy Resource) >> loop
        "2"  -> entityMenu conn (Proxy :: Proxy Author) >> loop
        "3"  -> entityMenu conn (Proxy :: Proxy ResourceAuthor) >> loop
        "4"  -> entityMenu conn (Proxy :: Proxy ResourceUrl) >> loop
        "5"  -> entityMenu conn (Proxy :: Proxy ResourceType) >> loop
        "6"  -> entityMenu conn (Proxy :: Proxy Purpose) >> loop
        "7"  -> entityMenu conn (Proxy :: Proxy User) >> loop
        "8"  -> entityMenu conn (Proxy :: Proxy UsageRecord) >> loop
        "9"  -> searchMenu conn >> loop
        "10" -> reportsMenu conn >> loop
        "0"  -> putStrLn "До побачення!"
        _    -> putStrLn "  ! Невідомий пункт меню." >> loop
