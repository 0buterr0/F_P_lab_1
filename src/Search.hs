-- | Пошук ресурсів (запити з JOIN по кількох таблицях) і звіти зі статистики.
module Search
  ( searchMenu
  , reportsMenu
  ) where

import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import Text.Read (readMaybe)

import Db
import Db.Value
import Display
import Entity
import Input
import Model

---------------------------------------------------------------------
-- Зведене подання ресурсу (результат JOIN) — окремий тип з власними
-- екземплярами FromRow і Display.
---------------------------------------------------------------------

data ResourceView = ResourceView
  { rvId         :: Int
  , rvTitle      :: Text
  , rvType       :: Text
  , rvPurpose    :: Text
  , rvOpenDate   :: Day
  , rvTerm       :: Maybe Int
  , rvAuthors    :: Text
  , rvAnnotation :: Text
  , rvConditions :: Text
  }

instance FromRow ResourceView where
  fromRow [i, t, ty, pu, d, tm, au, an, c] =
    ResourceView (fromValue i) (fromValue t) (fromValue ty) (fromValue pu) (fromValue d)
                 (fromValue tm) (fromValue au) (fromValue an) (fromValue c)
  fromRow r = badRow "resource view" r

instance Display ResourceView where
  display v = "[" ++ show (rvId v) ++ "] " ++ txt (rvTitle v) ++ " (" ++ txt (rvType v) ++ ")"
  displayFull v = unlines
    [ replicate 70 '-'
    , "[" ++ show (rvId v) ++ "] " ++ txt (rvTitle v)
    , "   Автор(и):      " ++ txt (rvAuthors v)
    , "   Вид:           " ++ txt (rvType v)
    , "   Призначення:   " ++ txt (rvPurpose v)
    , "   Відкрито:      " ++ renderField (rvOpenDate v)
    , "   Термін:        " ++ termText (rvOpenDate v) (rvTerm v)
    , "   Умови:         " ++ txt (rvConditions v)
    , "   Анотація:      " ++ txt (rvAnnotation v)
    ]

-- | Запит зведеного подання з довільною умовою WHERE.
viewSql :: String -> String
viewSql cond = unlines
  [ "SELECT r.id, r.title, t.name, p.name, r.open_date, r.usage_term_months,"
  , "       COALESCE(GROUP_CONCAT(DISTINCT a.full_name ORDER BY a.full_name SEPARATOR ', '), '-'),"
  , "       r.annotation, r.usage_conditions"
  , "FROM resources r"
  , "JOIN resource_types t ON t.id = r.type_id"
  , "JOIN purposes p ON p.id = r.purpose_id"
  , "LEFT JOIN resource_authors ra ON ra.resource_id = r.id"
  , "LEFT JOIN authors a ON a.id = ra.author_id"
  , "WHERE " ++ cond
  , "GROUP BY r.id, r.title, t.name, p.name, r.open_date, r.usage_term_months,"
  , "         r.annotation, r.usage_conditions"
  , "ORDER BY r.title"
  ]

findResources :: MySQLConn -> String -> [MySQLValue] -> IO [ResourceView]
findResources conn cond = queryAs conn (viewSql cond)

showResults :: [ResourceView] -> IO ()
showResults [] = putStrLn "  Нічого не знайдено."
showResults vs = do
  mapM_ (putStr . displayFull) vs
  putStrLn (replicate 70 '-')
  putStrLn ("  Знайдено ресурсів: " ++ show (length vs))

like :: Text -> MySQLValue
like t = MySQLText (T.concat [T.pack "%", t, T.pack "%"])

authorCond :: String
authorCond =
  "r.id IN (SELECT ra2.resource_id FROM resource_authors ra2 \
  \JOIN authors a2 ON a2.id = ra2.author_id WHERE a2.full_name LIKE ?)"

-- | Звіт: довільний SELECT, надрукований таблицею.
report :: MySQLConn -> [String] -> String -> [MySQLValue] -> IO ()
report conn headers sql ps = do
  rows <- queryRows conn sql ps
  printTable headers (map (map showValue) rows)

-- | Спільний цикл підменю.
menuLoop :: String -> [(String, String, IO ())] -> IO ()
menuLoop title items = loop
  where
    loop = do
      putStrLn ""
      putStrLn ("=== " ++ title ++ " ===")
      mapM_ (\(k, name, _) -> putStrLn (k ++ ". " ++ name)) items
      putStrLn "0. Назад"
      c <- prompt "> "
      case trim c of
        "0" -> pure ()
        k   -> do
          case [act | (k', _, act) <- items, k' == k] of
            (act : _) -> safely act
            []        -> putStrLn "  ! Невідомий пункт меню."
          loop

---------------------------------------------------------------------
-- Пошук
---------------------------------------------------------------------

searchMenu :: MySQLConn -> IO ()
searchMenu conn = menuLoop "Пошук інформаційних ресурсів"
  [ ("1", "За ключовим словом (назва, анотація)", byKeyword)
  , ("2", "За автором", byAuthor)
  , ("3", "За видом ресурсу", byType)
  , ("4", "За призначенням", byPurpose)
  , ("5", "За датою відкриття (період)", byPeriod)
  , ("6", "Ресурси, доступні на вказану дату", activeOn)
  , ("7", "За адресою в мережі (фрагмент URL)", byUrl)
  , ("8", "Комбінований пошук (кілька критеріїв)", combined)
  , ("9", "Повна картка ресурсу (автори, адреси, статистика)", details)
  ]
  where
    run cond ps = findResources conn cond ps >>= showResults

    byKeyword = do
      kw <- ask "Ключове слово"
      run "(r.title LIKE ? OR r.annotation LIKE ?)" [like kw, like kw]

    byAuthor = do
      name <- ask "Прізвище або частина ПІБ автора"
      run authorCond [like name]

    byType = do
      k <- askRef conn (Proxy :: Proxy ResourceType) "id виду" Nothing
      run "r.type_id = ?" [toValue k]

    byPurpose = do
      k <- askRef conn (Proxy :: Proxy Purpose) "id призначення" Nothing
      run "r.purpose_id = ?" [toValue k]

    byPeriod = do
      d1 <- ask "Дата від (РРРР-ММ-ДД)" :: IO Day
      d2 <- ask "Дата до (РРРР-ММ-ДД)" :: IO Day
      run "r.open_date BETWEEN ? AND ?" [toValue d1, toValue d2]

    activeOn = do
      d <- askDef "Дата (РРРР-ММ-ДД)" =<< today
      run "r.open_date <= ? AND (r.usage_term_months IS NULL \
          \OR DATE_ADD(r.open_date, INTERVAL r.usage_term_months MONTH) >= ?)"
          [toValue d, toValue d]

    byUrl = do
      s <- ask "Фрагмент адреси"
      run "r.id IN (SELECT u.resource_id FROM resource_urls u WHERE u.url LIKE ?)" [like s]

    combined = do
      putStrLn "  (Enter — пропустити критерій)"
      kw <- prompt "  Ключове слово: "
      au <- prompt "  Автор: "
      types <- findAll conn :: IO [ResourceType]
      mapM_ (putStrLn . ("     " ++) . display) types
      ty <- prompt "  id виду: "
      purposes <- findAll conn :: IO [Purpose]
      mapM_ (putStrLn . ("     " ++) . display) purposes
      pu <- prompt "  id призначення: "
      d  <- prompt "  Доступні на дату (РРРР-ММ-ДД): "
      let textCond s c n =
            [ (c, replicate n (like (T.pack (trim s)))) | not (null (trim s)) ]
          intCond s c = case readMaybe (trim s) :: Maybe Int of
            Just k  -> [(c, [toValue k])]
            Nothing -> []
          dateCond s = case parseField s :: Maybe Day of
            Just day ->
              [ ( "r.open_date <= ? AND (r.usage_term_months IS NULL \
                  \OR DATE_ADD(r.open_date, INTERVAL r.usage_term_months MONTH) >= ?)"
                , [toValue day, toValue day] ) ]
            Nothing -> []
          parts = textCond kw "(r.title LIKE ? OR r.annotation LIKE ?)" 2
               ++ textCond au authorCond 1
               ++ intCond ty "r.type_id = ?"
               ++ intCond pu "r.purpose_id = ?"
               ++ dateCond d
          cond = if null parts then "1 = 1" else intercalate " AND " (map fst parts)
      run cond (concatMap snd parts)

    details = do
      k <- ask "id ресурсу" :: IO Int
      vs <- findResources conn "r.id = ?" [toValue k]
      case vs of
        [] -> putStrLn "  Ресурс не знайдено."
        (v : _) -> do
          putStr (displayFull v)
          putStrLn "\n  Автори та їхні ролі:"
          report conn ["Автор", "Посада", "Ступінь", "Роль"]
            "SELECT a.full_name, a.position, a.academic_degree, ra.role \
            \FROM resource_authors ra JOIN authors a ON a.id = ra.author_id \
            \WHERE ra.resource_id = ? ORDER BY a.full_name"
            [toValue k]
          putStrLn "\n  Адреси в мережі:"
          report conn ["URL", "Опис"]
            "SELECT url, description FROM resource_urls WHERE resource_id = ? ORDER BY id"
            [toValue k]
          putStrLn "\n  Статистика використання:"
          report conn ["Звернень", "Користувачів", "Хвилин", "Останнє звернення"]
            "SELECT COUNT(*), COUNT(DISTINCT user_id), \
            \CAST(COALESCE(SUM(duration_min), 0) AS SIGNED), MAX(access_time) \
            \FROM usage_stats WHERE resource_id = ?"
            [toValue k]

---------------------------------------------------------------------
-- Звіти зі статистики використання
---------------------------------------------------------------------

reportsMenu :: MySQLConn -> IO ()
reportsMenu conn = menuLoop "Звіти: статистика використання"
  [ ("1", "Популярність ресурсів", popularity)
  , ("2", "Активність користувачів", userActivity)
  , ("3", "Використання за видами ресурсів", byType)
  , ("4", "Використання за місяцями", byMonth)
  , ("5", "Звернення до ресурсів за період", byPeriod)
  , ("6", "Стан терміну використання ресурсів", termStatus)
  , ("7", "Загальні показники системи", totals)
  ]
  where
    popularity = report conn
      ["id", "Ресурс", "Звернень", "Користувачів", "Хвилин", "Останнє"]
      "SELECT r.id, r.title, COUNT(s.id), COUNT(DISTINCT s.user_id), \
      \CAST(COALESCE(SUM(s.duration_min), 0) AS SIGNED), MAX(s.access_time) \
      \FROM resources r LEFT JOIN usage_stats s ON s.resource_id = r.id \
      \GROUP BY r.id, r.title ORDER BY 3 DESC, r.title"
      []

    userActivity = report conn
      ["Користувач", "Категорія", "Група", "Звернень", "Ресурсів", "Хвилин", "Останнє"]
      "SELECT u.full_name, u.category, u.study_group, COUNT(s.id), \
      \COUNT(DISTINCT s.resource_id), CAST(COALESCE(SUM(s.duration_min), 0) AS SIGNED), \
      \MAX(s.access_time) \
      \FROM users u LEFT JOIN usage_stats s ON s.user_id = u.id \
      \GROUP BY u.id, u.full_name, u.category, u.study_group ORDER BY 4 DESC, u.full_name"
      []

    byType = report conn
      ["Вид ресурсу", "Ресурсів", "Звернень", "Хвилин"]
      "SELECT t.name, COUNT(DISTINCT r.id), COUNT(s.id), \
      \CAST(COALESCE(SUM(s.duration_min), 0) AS SIGNED) \
      \FROM resource_types t \
      \LEFT JOIN resources r ON r.type_id = t.id \
      \LEFT JOIN usage_stats s ON s.resource_id = r.id \
      \GROUP BY t.id, t.name ORDER BY 3 DESC, t.name"
      []

    byMonth = report conn
      ["Місяць", "Звернень", "Користувачів", "Ресурсів", "Хвилин"]
      "SELECT DATE_FORMAT(s.access_time, '%Y-%m') AS m, COUNT(*), \
      \COUNT(DISTINCT s.user_id), COUNT(DISTINCT s.resource_id), \
      \CAST(SUM(s.duration_min) AS SIGNED) \
      \FROM usage_stats s GROUP BY m ORDER BY m"
      []

    byPeriod = do
      d1 <- ask "Дата від (РРРР-ММ-ДД)" :: IO Day
      d2 <- ask "Дата до (РРРР-ММ-ДД)" :: IO Day
      report conn
        ["Ресурс", "Звернень", "Користувачів", "Хвилин"]
        "SELECT r.title, COUNT(s.id), COUNT(DISTINCT s.user_id), \
        \CAST(SUM(s.duration_min) AS SIGNED) \
        \FROM usage_stats s JOIN resources r ON r.id = s.resource_id \
        \WHERE s.access_time >= ? AND s.access_time < DATE_ADD(?, INTERVAL 1 DAY) \
        \GROUP BY r.id, r.title ORDER BY 2 DESC"
        [toValue d1, toValue d2]

    termStatus = report conn
      ["id", "Ресурс", "Відкрито", "Міс.", "Діє до", "Стан"]
      "SELECT r.id, r.title, r.open_date, r.usage_term_months, \
      \DATE_ADD(r.open_date, INTERVAL r.usage_term_months MONTH), \
      \CASE WHEN r.usage_term_months IS NULL THEN ? \
      \     WHEN DATE_ADD(r.open_date, INTERVAL r.usage_term_months MONTH) >= CURDATE() THEN ? \
      \     ELSE ? END \
      \FROM resources r ORDER BY r.id"
      (map (MySQLText . T.pack) ["безстроково", "діє", "термін завершився"])

    totals = report conn
      ["Показник", "Значення"]
      "SELECT ?, COUNT(*) FROM resources \
      \UNION ALL SELECT ?, COUNT(*) FROM authors \
      \UNION ALL SELECT ?, COUNT(*) FROM users \
      \UNION ALL SELECT ?, COUNT(*) FROM resource_urls \
      \UNION ALL SELECT ?, COUNT(*) FROM usage_stats \
      \UNION ALL SELECT ?, CAST(COALESCE(SUM(duration_min), 0) AS SIGNED) FROM usage_stats"
      (map (MySQLText . T.pack)
        [ "Ресурсів", "Авторів", "Користувачів", "Адрес у мережі"
        , "Звернень до ресурсів", "Хвилин використання" ])
