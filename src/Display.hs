-- | Клас Display — текстове подання записів, і табличний друк звітів.
module Display
  ( Display (..)
  , txt
  , orDash
  , printTable
  ) where

import Data.List (intercalate, transpose)
import Data.Text (Text)
import qualified Data.Text as T

class Display a where
  -- | Короткий однорядковий вигляд (для списків).
  display :: a -> String
  -- | Повна картка запису.
  displayFull :: a -> String
  displayFull x = display x ++ "\n"

txt :: Text -> String
txt = T.unpack

orDash :: Maybe Text -> String
orDash = maybe "-" T.unpack

-- | Друк таблиці з вирівнюванням колонок.
printTable :: [String] -> [[String]] -> IO ()
printTable headers rows = do
  putStrLn ("  " ++ line headers)
  putStrLn ("  " ++ intercalate "-+-" (map (`replicate` '-') widths))
  mapM_ (putStrLn . ("  " ++) . line) rows
  if null rows then putStrLn "  (немає даних)" else pure ()
  where
    maxW   = 45
    widths = map (min maxW . maximum . map length) (transpose (headers : rows))
    cell w s =
      let s' = if length s > w then take (w - 1) s ++ "…" else s
      in  s' ++ replicate (w - length s') ' '
    line cs = intercalate " | " (zipWith cell widths cs)
