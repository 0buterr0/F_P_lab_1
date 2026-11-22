{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Лабораторна робота №1. Інформаційна система «Інформаційні ресурси кафедри».
module Main (main) where

import Control.Exception (SomeException, finally, try)
import System.Exit (exitFailure)
import System.IO

import Db
import Menu (mainMenu)

#if defined(mingw32_HOST_OS)
import System.Win32.Console (setConsoleCP, setConsoleOutputCP)
#endif

-- | У консолі Windows вмикаємо кодову сторінку UTF-8 (те саме, що chcp 65001).
setupConsole :: IO ()
setupConsole = do
#if defined(mingw32_HOST_OS)
  setConsoleOutputCP 65001
  setConsoleCP 65001
#endif
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  hSetBuffering stdout NoBuffering

main :: IO ()
main = do
  setupConsole
  cfg <- loadConfig
  putStrLn ("Підключення до MySQL: " ++ cfgUser cfg ++ "@" ++ cfgHost cfg ++ ":"
            ++ show (cfgPort cfg) ++ "/" ++ cfgDatabase cfg ++ " ...")
  result <- try (openConnection cfg)
  case result of
    Left (e :: SomeException) -> do
      putStrLn ("Не вдалося підключитися до БД: " ++ show e)
      putStrLn "Перевірте, що сервер MySQL запущено і база створена (див. README.md)."
      exitFailure
    Right conn -> do
      putStrLn "Підключено."
      mainMenu conn `finally` closeConnection conn
