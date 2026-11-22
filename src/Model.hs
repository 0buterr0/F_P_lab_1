-- | Типи даних, що відповідають таблицям БД, та їхні екземпляри класів
--   FromRow, Entity, Display, Editable.
module Model where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, LocalTime, addGregorianMonthsClip)

import Db.Value
import Display
import Entity
import Input

---------------------------------------------------------------------
-- 1. Вид ресурсу (resource_types)
---------------------------------------------------------------------

data ResourceType = ResourceType
  { rtId          :: Int
  , rtName        :: Text
  , rtDescription :: Maybe Text
  } deriving (Show, Eq)

instance FromRow ResourceType where
  fromRow [i, n, d] = ResourceType (fromValue i) (fromValue n) (fromValue d)
  fromRow r         = badRow "resource_types" r

instance Entity ResourceType where
  entityTitle _ = "Види ресурсів"
  tableName _   = "resource_types"
  dataColumns _ = ["name", "description"]
  toRow x       = [toValue (rtName x), toValue (rtDescription x)]
  getKey        = rtId

instance Display ResourceType where
  display x = "[" ++ show (rtId x) ++ "] " ++ txt (rtName x)
           ++ maybe "" (\d -> " — " ++ txt d) (rtDescription x)

instance Editable ResourceType where
  inputNew _ = ResourceType 0
    <$> ask "Назва виду"
    <*> ask "Опис (- якщо немає)"
  inputEdit _ x = ResourceType (rtId x)
    <$> askDef "Назва виду" (rtName x)
    <*> askDef "Опис" (rtDescription x)

---------------------------------------------------------------------
-- 2. Призначення (purposes)
---------------------------------------------------------------------

data Purpose = Purpose
  { puId          :: Int
  , puName        :: Text
  , puDescription :: Maybe Text
  } deriving (Show, Eq)

instance FromRow Purpose where
  fromRow [i, n, d] = Purpose (fromValue i) (fromValue n) (fromValue d)
  fromRow r         = badRow "purposes" r

instance Entity Purpose where
  entityTitle _ = "Призначення ресурсів"
  tableName _   = "purposes"
  dataColumns _ = ["name", "description"]
  toRow x       = [toValue (puName x), toValue (puDescription x)]
  getKey        = puId

instance Display Purpose where
  display x = "[" ++ show (puId x) ++ "] " ++ txt (puName x)
           ++ maybe "" (\d -> " — " ++ txt d) (puDescription x)

instance Editable Purpose where
  inputNew _ = Purpose 0
    <$> ask "Призначення"
    <*> ask "Опис (- якщо немає)"
  inputEdit _ x = Purpose (puId x)
    <$> askDef "Призначення" (puName x)
    <*> askDef "Опис" (puDescription x)

---------------------------------------------------------------------
-- 3. Автор (authors)
---------------------------------------------------------------------

data Author = Author
  { auId       :: Int
  , auFullName :: Text
  , auPosition :: Text
  , auDegree   :: Maybe Text
  , auEmail    :: Maybe Text
  } deriving (Show, Eq)

instance FromRow Author where
  fromRow [i, n, p, d, e] =
    Author (fromValue i) (fromValue n) (fromValue p) (fromValue d) (fromValue e)
  fromRow r = badRow "authors" r

instance Entity Author where
  entityTitle _ = "Автори"
  tableName _   = "authors"
  dataColumns _ = ["full_name", "position", "academic_degree", "email"]
  toRow x       = [ toValue (auFullName x), toValue (auPosition x)
                  , toValue (auDegree x), toValue (auEmail x) ]
  getKey        = auId

instance Display Author where
  display x = "[" ++ show (auId x) ++ "] " ++ txt (auFullName x)
           ++ " — " ++ txt (auPosition x)
           ++ maybe "" (\d -> ", " ++ txt d) (auDegree x)
  displayFull x = unlines
    [ "[" ++ show (auId x) ++ "] " ++ txt (auFullName x)
    , "   Посада:           " ++ txt (auPosition x)
    , "   Науковий ступінь: " ++ orDash (auDegree x)
    , "   E-mail:           " ++ orDash (auEmail x)
    ]

instance Editable Author where
  inputNew _ = Author 0
    <$> ask "ПІБ автора"
    <*> ask "Посада (напр. доцент)"
    <*> ask "Науковий ступінь (- якщо немає)"
    <*> ask "E-mail (- якщо немає)"
  inputEdit _ x = Author (auId x)
    <$> askDef "ПІБ автора" (auFullName x)
    <*> askDef "Посада" (auPosition x)
    <*> askDef "Науковий ступінь" (auDegree x)
    <*> askDef "E-mail" (auEmail x)

---------------------------------------------------------------------
-- 4. Інформаційний ресурс (resources)
---------------------------------------------------------------------

data Resource = Resource
  { reId         :: Int
  , reTitle      :: Text
  , reAnnotation :: Text
  , reTypeId     :: Int
  , rePurposeId  :: Int
  , reOpenDate   :: Day
  , reTermMonths :: Maybe Int   -- ^ Nothing = безстроково
  , reConditions :: Text
  } deriving (Show, Eq)

instance FromRow Resource where
  fromRow [i, t, a, ty, pu, d, tm, c] =
    Resource (fromValue i) (fromValue t) (fromValue a) (fromValue ty)
             (fromValue pu) (fromValue d) (fromValue tm) (fromValue c)
  fromRow r = badRow "resources" r

instance Entity Resource where
  entityTitle _ = "Інформаційні ресурси"
  tableName _   = "resources"
  dataColumns _ = [ "title", "annotation", "type_id", "purpose_id"
                  , "open_date", "usage_term_months", "usage_conditions" ]
  toRow x       = [ toValue (reTitle x), toValue (reAnnotation x)
                  , toValue (reTypeId x), toValue (rePurposeId x)
                  , toValue (reOpenDate x), toValue (reTermMonths x)
                  , toValue (reConditions x) ]
  getKey        = reId

-- | Текст терміну використання з датою завершення.
termText :: Day -> Maybe Int -> String
termText _ Nothing  = "безстроково"
termText d (Just n) = show n ++ " міс. (до "
                   ++ renderField (addGregorianMonthsClip (fromIntegral n) d) ++ ")"

instance Display Resource where
  display x = "[" ++ show (reId x) ++ "] " ++ txt (reTitle x)
           ++ " | відкрито " ++ renderField (reOpenDate x)
           ++ " | термін: " ++ termText (reOpenDate x) (reTermMonths x)
  displayFull x = unlines
    [ "[" ++ show (reId x) ++ "] " ++ txt (reTitle x)
    , "   Анотація:      " ++ txt (reAnnotation x)
    , "   Вид (id):      " ++ show (reTypeId x)
    , "   Призначення:   " ++ show (rePurposeId x)
    , "   Відкрито:      " ++ renderField (reOpenDate x)
    , "   Термін:        " ++ termText (reOpenDate x) (reTermMonths x)
    , "   Умови:         " ++ txt (reConditions x)
    ]

instance Editable Resource where
  inputNew conn = do
    title <- ask "Назва ресурсу"
    ann   <- ask "Текст анотації"
    ty    <- askRef conn (Proxy :: Proxy ResourceType) "Вид ресурсу (id)" Nothing
    pu    <- askRef conn (Proxy :: Proxy Purpose) "Призначення (id)" Nothing
    d     <- ask "Дата відкриття для використання (РРРР-ММ-ДД)"
    term  <- ask "Термін використання, міс. (- = безстроково)"
    cond  <- ask "Умови використання"
    pure (Resource 0 title ann ty pu d term cond)
  inputEdit conn x = do
    title <- askDef "Назва ресурсу" (reTitle x)
    ann   <- askDef "Текст анотації" (reAnnotation x)
    ty    <- askRef conn (Proxy :: Proxy ResourceType) "Вид ресурсу (id)" (Just (reTypeId x))
    pu    <- askRef conn (Proxy :: Proxy Purpose) "Призначення (id)" (Just (rePurposeId x))
    d     <- askDef "Дата відкриття (РРРР-ММ-ДД)" (reOpenDate x)
    term  <- askDef "Термін використання, міс. (- = безстроково)" (reTermMonths x)
    cond  <- askDef "Умови використання" (reConditions x)
    pure x { reTitle = title, reAnnotation = ann, reTypeId = ty, rePurposeId = pu
           , reOpenDate = d, reTermMonths = term, reConditions = cond }

---------------------------------------------------------------------
-- 5. Зв'язок ресурс — автор (resource_authors), багато-до-багатьох
---------------------------------------------------------------------

data ResourceAuthor = ResourceAuthor
  { raId         :: Int
  , raResourceId :: Int
  , raAuthorId   :: Int
  , raRole       :: Text
  } deriving (Show, Eq)

instance FromRow ResourceAuthor where
  fromRow [i, r, a, role] =
    ResourceAuthor (fromValue i) (fromValue r) (fromValue a) (fromValue role)
  fromRow r = badRow "resource_authors" r

instance Entity ResourceAuthor where
  entityTitle _ = "Автори ресурсів (зв'язки)"
  tableName _   = "resource_authors"
  dataColumns _ = ["resource_id", "author_id", "role"]
  toRow x       = [toValue (raResourceId x), toValue (raAuthorId x), toValue (raRole x)]
  getKey        = raId

instance Display ResourceAuthor where
  display x = "[" ++ show (raId x) ++ "] ресурс #" ++ show (raResourceId x)
           ++ " — автор #" ++ show (raAuthorId x) ++ " (" ++ txt (raRole x) ++ ")"

instance Editable ResourceAuthor where
  inputNew conn = do
    r    <- askRef conn (Proxy :: Proxy Resource) "id ресурсу" Nothing
    a    <- askRef conn (Proxy :: Proxy Author) "id автора" Nothing
    role <- askDef "Роль" (T.pack "автор")
    pure (ResourceAuthor 0 r a role)
  inputEdit conn x = do
    r    <- askRef conn (Proxy :: Proxy Resource) "id ресурсу" (Just (raResourceId x))
    a    <- askRef conn (Proxy :: Proxy Author) "id автора" (Just (raAuthorId x))
    role <- askDef "Роль" (raRole x)
    pure x { raResourceId = r, raAuthorId = a, raRole = role }

---------------------------------------------------------------------
-- 6. Адреса в мережі (resource_urls)
---------------------------------------------------------------------

data ResourceUrl = ResourceUrl
  { ruId          :: Int
  , ruResourceId  :: Int
  , ruUrl         :: Text
  , ruDescription :: Maybe Text
  } deriving (Show, Eq)

instance FromRow ResourceUrl where
  fromRow [i, r, u, d] = ResourceUrl (fromValue i) (fromValue r) (fromValue u) (fromValue d)
  fromRow r            = badRow "resource_urls" r

instance Entity ResourceUrl where
  entityTitle _ = "Адреси ресурсів у мережі"
  tableName _   = "resource_urls"
  dataColumns _ = ["resource_id", "url", "description"]
  toRow x       = [toValue (ruResourceId x), toValue (ruUrl x), toValue (ruDescription x)]
  getKey        = ruId

instance Display ResourceUrl where
  display x = "[" ++ show (ruId x) ++ "] ресурс #" ++ show (ruResourceId x)
           ++ ": " ++ txt (ruUrl x) ++ maybe "" (\d -> " (" ++ txt d ++ ")") (ruDescription x)

askUrl :: String -> Maybe Text -> IO Text
askUrl label mOld = do
  u <- maybe (ask label) (askDef label) mOld
  if any (`T.isPrefixOf` u) (map T.pack ["http://", "https://", "ftp://"])
    then pure u
    else do
      putStrLn "  ! Адреса має починатися з http://, https:// або ftp://"
      askUrl label mOld

instance Editable ResourceUrl where
  inputNew conn = do
    r <- askRef conn (Proxy :: Proxy Resource) "id ресурсу" Nothing
    u <- askUrl "Адреса (URL)" Nothing
    d <- ask "Опис (- якщо немає)"
    pure (ResourceUrl 0 r u d)
  inputEdit conn x = do
    r <- askRef conn (Proxy :: Proxy Resource) "id ресурсу" (Just (ruResourceId x))
    u <- askUrl "Адреса (URL)" (Just (ruUrl x))
    d <- askDef "Опис" (ruDescription x)
    pure x { ruResourceId = r, ruUrl = u, ruDescription = d }

---------------------------------------------------------------------
-- 7. Користувач (users)
---------------------------------------------------------------------

data User = User
  { usId         :: Int
  , usFullName   :: Text
  , usCategory   :: Text
  , usGroup      :: Maybe Text
  , usEmail      :: Maybe Text
  , usRegistered :: Day
  } deriving (Show, Eq)

userCategories :: [Text]
userCategories = map T.pack
  ["студент", "аспірант", "викладач", "співробітник кафедри", "зовнішній користувач"]

instance FromRow User where
  fromRow [i, n, c, g, e, d] =
    User (fromValue i) (fromValue n) (fromValue c) (fromValue g) (fromValue e) (fromValue d)
  fromRow r = badRow "users" r

instance Entity User where
  entityTitle _ = "Користувачі"
  tableName _   = "users"
  dataColumns _ = ["full_name", "category", "study_group", "email", "registered_on"]
  toRow x       = [ toValue (usFullName x), toValue (usCategory x), toValue (usGroup x)
                  , toValue (usEmail x), toValue (usRegistered x) ]
  getKey        = usId

instance Display User where
  display x = "[" ++ show (usId x) ++ "] " ++ txt (usFullName x)
           ++ " — " ++ txt (usCategory x)
           ++ maybe "" (\g -> ", група " ++ txt g) (usGroup x)
  displayFull x = unlines
    [ "[" ++ show (usId x) ++ "] " ++ txt (usFullName x)
    , "   Категорія:     " ++ txt (usCategory x)
    , "   Група:         " ++ orDash (usGroup x)
    , "   E-mail:        " ++ orDash (usEmail x)
    , "   Зареєстровано: " ++ renderField (usRegistered x)
    ]

instance Editable User where
  inputNew _ = do
    name <- ask "ПІБ користувача"
    cat  <- askChoice "Категорія" userCategories Nothing
    grp  <- ask "Група (- якщо немає)"
    mail <- ask "E-mail (- якщо немає)"
    reg  <- askDef "Дата реєстрації" =<< today
    pure (User 0 name cat grp mail reg)
  inputEdit _ x = do
    name <- askDef "ПІБ користувача" (usFullName x)
    cat  <- askChoice "Категорія" userCategories (Just (usCategory x))
    grp  <- askDef "Група" (usGroup x)
    mail <- askDef "E-mail" (usEmail x)
    reg  <- askDef "Дата реєстрації" (usRegistered x)
    pure x { usFullName = name, usCategory = cat, usGroup = grp
           , usEmail = mail, usRegistered = reg }

---------------------------------------------------------------------
-- 8. Статистика використання — журнал звернень (usage_stats)
---------------------------------------------------------------------

data UsageRecord = UsageRecord
  { urId          :: Int
  , urResourceId  :: Int
  , urUserId      :: Int
  , urAccessTime  :: LocalTime
  , urAction      :: Text
  , urDurationMin :: Int
  } deriving (Show, Eq)

usageActions :: [Text]
usageActions = map T.pack ["перегляд", "завантаження", "онлайн-робота", "друк"]

instance FromRow UsageRecord where
  fromRow [i, r, u, t, a, d] =
    UsageRecord (fromValue i) (fromValue r) (fromValue u) (fromValue t) (fromValue a) (fromValue d)
  fromRow r = badRow "usage_stats" r

instance Entity UsageRecord where
  entityTitle _ = "Статистика використання (журнал звернень)"
  tableName _   = "usage_stats"
  dataColumns _ = ["resource_id", "user_id", "access_time", "action", "duration_min"]
  toRow x       = [ toValue (urResourceId x), toValue (urUserId x), toValue (urAccessTime x)
                  , toValue (urAction x), toValue (urDurationMin x) ]
  getKey        = urId

instance Display UsageRecord where
  display x = "[" ++ show (urId x) ++ "] " ++ renderField (urAccessTime x)
           ++ " | ресурс #" ++ show (urResourceId x)
           ++ " | користувач #" ++ show (urUserId x)
           ++ " | " ++ txt (urAction x)
           ++ " | " ++ show (urDurationMin x) ++ " хв"

instance Editable UsageRecord where
  inputNew conn = do
    r <- askRef conn (Proxy :: Proxy Resource) "id ресурсу" Nothing
    u <- askRef conn (Proxy :: Proxy User) "id користувача" Nothing
    t <- askDef "Дата і час (РРРР-ММ-ДД ГГ:ХХ)" =<< nowLocal
    a <- askChoice "Дія" usageActions Nothing
    d <- ask "Тривалість, хв"
    pure (UsageRecord 0 r u t a d)
  inputEdit conn x = do
    r <- askRef conn (Proxy :: Proxy Resource) "id ресурсу" (Just (urResourceId x))
    u <- askRef conn (Proxy :: Proxy User) "id користувача" (Just (urUserId x))
    t <- askDef "Дата і час (РРРР-ММ-ДД ГГ:ХХ)" (urAccessTime x)
    a <- askChoice "Дія" usageActions (Just (urAction x))
    d <- askDef "Тривалість, хв" (urDurationMin x)
    pure x { urResourceId = r, urUserId = u, urAccessTime = t
           , urAction = a, urDurationMin = d }
