-- =====================================================================
--  Інформаційна система «Інформаційні ресурси кафедри»
--  Схема БД MySQL (8 таблиць) + тестові дані
-- =====================================================================

SET NAMES utf8mb4;

CREATE DATABASE IF NOT EXISTS dept_resources
  CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE dept_resources;

SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXISTS usage_stats;
DROP TABLE IF EXISTS resource_urls;
DROP TABLE IF EXISTS resource_authors;
DROP TABLE IF EXISTS resources;
DROP TABLE IF EXISTS users;
DROP TABLE IF EXISTS authors;
DROP TABLE IF EXISTS purposes;
DROP TABLE IF EXISTS resource_types;
SET FOREIGN_KEY_CHECKS = 1;

-- 1. Види ресурсів (довідник)
CREATE TABLE resource_types (
  id          INT AUTO_INCREMENT PRIMARY KEY,
  name        VARCHAR(100) NOT NULL UNIQUE,
  description VARCHAR(255) NULL
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 COLLATE = utf8mb4_unicode_ci;

-- 2. Призначення ресурсів (довідник)
CREATE TABLE purposes (
  id          INT AUTO_INCREMENT PRIMARY KEY,
  name        VARCHAR(100) NOT NULL UNIQUE,
  description VARCHAR(255) NULL
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 COLLATE = utf8mb4_unicode_ci;

-- 3. Автори
CREATE TABLE authors (
  id              INT AUTO_INCREMENT PRIMARY KEY,
  full_name       VARCHAR(150) NOT NULL,
  position        VARCHAR(100) NOT NULL,
  academic_degree VARCHAR(100) NULL,
  email           VARCHAR(150) NULL,
  INDEX idx_authors_name (full_name)
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 COLLATE = utf8mb4_unicode_ci;

-- 4. Інформаційні ресурси
CREATE TABLE resources (
  id                INT AUTO_INCREMENT PRIMARY KEY,
  title             VARCHAR(255) NOT NULL,
  annotation        TEXT NOT NULL,
  type_id           INT NOT NULL,
  purpose_id        INT NOT NULL,
  open_date         DATE NOT NULL,                 -- дата відкриття для використання
  usage_term_months INT NULL,                      -- термін використання; NULL = безстроково
  usage_conditions  VARCHAR(255) NOT NULL,         -- умови використання
  CONSTRAINT chk_term CHECK (usage_term_months IS NULL OR usage_term_months > 0),
  CONSTRAINT fk_res_type FOREIGN KEY (type_id) REFERENCES resource_types (id)
    ON UPDATE CASCADE ON DELETE RESTRICT,
  CONSTRAINT fk_res_purpose FOREIGN KEY (purpose_id) REFERENCES purposes (id)
    ON UPDATE CASCADE ON DELETE RESTRICT,
  INDEX idx_res_title (title),
  INDEX idx_res_open (open_date)
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 COLLATE = utf8mb4_unicode_ci;

-- 5. Автори ресурсів (зв'язок багато-до-багатьох)
CREATE TABLE resource_authors (
  id          INT AUTO_INCREMENT PRIMARY KEY,
  resource_id INT NOT NULL,
  author_id   INT NOT NULL,
  role        VARCHAR(50) NOT NULL DEFAULT 'автор',
  CONSTRAINT uq_res_author UNIQUE (resource_id, author_id),
  CONSTRAINT fk_ra_resource FOREIGN KEY (resource_id) REFERENCES resources (id)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT fk_ra_author FOREIGN KEY (author_id) REFERENCES authors (id)
    ON UPDATE CASCADE ON DELETE RESTRICT
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 COLLATE = utf8mb4_unicode_ci;

-- 6. Адреси ресурсів у мережі
CREATE TABLE resource_urls (
  id          INT AUTO_INCREMENT PRIMARY KEY,
  resource_id INT NOT NULL,
  url         VARCHAR(500) NOT NULL,
  description VARCHAR(255) NULL,
  CONSTRAINT fk_url_resource FOREIGN KEY (resource_id) REFERENCES resources (id)
    ON UPDATE CASCADE ON DELETE CASCADE
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 COLLATE = utf8mb4_unicode_ci;

-- 7. Користувачі
CREATE TABLE users (
  id            INT AUTO_INCREMENT PRIMARY KEY,
  full_name     VARCHAR(150) NOT NULL,
  category      VARCHAR(50) NOT NULL,       -- студент / аспірант / викладач / ...
  study_group   VARCHAR(20) NULL,
  email         VARCHAR(150) NULL,
  registered_on DATE NOT NULL
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 COLLATE = utf8mb4_unicode_ci;

-- 8. Статистика використання (журнал звернень)
CREATE TABLE usage_stats (
  id           INT AUTO_INCREMENT PRIMARY KEY,
  resource_id  INT NOT NULL,
  user_id      INT NOT NULL,
  access_time  DATETIME NOT NULL,
  action       VARCHAR(50) NOT NULL,        -- перегляд / завантаження / ...
  duration_min INT NOT NULL DEFAULT 0,
  CONSTRAINT chk_duration CHECK (duration_min >= 0),
  CONSTRAINT fk_us_resource FOREIGN KEY (resource_id) REFERENCES resources (id)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT fk_us_user FOREIGN KEY (user_id) REFERENCES users (id)
    ON UPDATE CASCADE ON DELETE CASCADE,
  INDEX idx_us_time (access_time)
) ENGINE = InnoDB DEFAULT CHARSET = utf8mb4 COLLATE = utf8mb4_unicode_ci;

-- =====================================================================
--  Тестові дані
-- =====================================================================

INSERT INTO resource_types (id, name, description) VALUES
  (1, 'Електронний підручник', 'Підручники та навчальні посібники в електронному вигляді'),
  (2, 'Методичні вказівки', 'Вказівки до лабораторних, практичних і курсових робіт'),
  (3, 'Відеолекція', 'Записи лекцій і відеокурси'),
  (4, 'Програмне забезпечення', 'Ліцензійне та вільне ПЗ для навчального процесу'),
  (5, 'База даних', 'Бази даних і електронні каталоги'),
  (6, 'Онлайн-курс', 'Дистанційні курси'),
  (7, 'Репозиторій коду', 'Приклади програм і шаблони проєктів'),
  (8, 'Веб-сервіс', 'Інформаційні сервіси кафедри');

INSERT INTO purposes (id, name, description) VALUES
  (1, 'Навчальний процес', 'Аудиторні заняття та лабораторні роботи'),
  (2, 'Наукова робота', 'Дослідження викладачів та аспірантів'),
  (3, 'Самостійна робота студентів', NULL),
  (4, 'Дипломне проєктування', 'Підготовка кваліфікаційних робіт'),
  (5, 'Адміністративні потреби', 'Організація роботи кафедри');

INSERT INTO authors (id, full_name, position, academic_degree, email) VALUES
  (1, 'Петренко Олег Іванович', 'доцент', 'кандидат технічних наук', 'o.petrenko@dept.example.edu.ua'),
  (2, 'Коваленко Ірина Василівна', 'професор', 'доктор фізико-математичних наук', 'i.kovalenko@dept.example.edu.ua'),
  (3, 'Шевчук Андрій Миколайович', 'старший викладач', NULL, 'a.shevchuk@dept.example.edu.ua'),
  (4, 'Бондар Наталія Петрівна', 'асистент', NULL, NULL),
  (5, 'Мельник Сергій Олександрович', 'доцент', 'кандидат фізико-математичних наук', 's.melnyk@dept.example.edu.ua'),
  (6, 'Гнатюк Тарас Романович', 'завідувач лабораторії', NULL, 't.hnatiuk@dept.example.edu.ua');

INSERT INTO resources (id, title, annotation, type_id, purpose_id, open_date, usage_term_months, usage_conditions) VALUES
  (1, 'Функціональне програмування мовою Haskell',
      'Навчальний посібник: типи і класи типів, функції вищого порядку, монади, паралельні обчислення. Містить задачі для самоперевірки.',
      1, 1, '2023-09-01', NULL, 'Вільний доступ для студентів і викладачів кафедри'),
  (2, 'Методичні вказівки до лабораторних робіт з баз даних',
      'Вказівки до 8 лабораторних робіт: проєктування схеми, SQL-запити, індекси, транзакції, робота з MySQL.',
      2, 1, '2024-02-01', 36, 'Доступ після авторизації в системі дистанційного навчання'),
  (3, 'Відеокурс «Алгоритми та структури даних»',
      'Цикл із 24 відеолекцій: сортування, дерева пошуку, хешування, алгоритми на графах.',
      3, 3, '2024-09-01', 24, 'Лише перегляд онлайн, завантаження заборонене'),
  (4, 'Ліцензійний пакет MATLAB для навчальних лабораторій',
      'Мережева ліцензія MATLAB із пакетами Statistics та Optimization для 25 робочих місць.',
      4, 1, '2025-01-15', 12, 'Використання лише в комп''ютерних класах кафедри'),
  (5, 'Репозиторій прикладів коду з курсу «Розподілені системи»',
      'Приклади клієнт-серверних застосунків, черг повідомлень і мікросервісів з інструкціями до запуску.',
      7, 3, '2025-09-01', NULL, 'Ліцензія MIT, використання з посиланням на авторів'),
  (6, 'База даних наукових публікацій кафедри',
      'Каталог статей, тез і монографій співробітників кафедри з 2010 року з пошуком за ключовими словами.',
      5, 2, '2022-03-01', NULL, 'Доступ для викладачів і аспірантів'),
  (7, 'Онлайн-курс «Машинне навчання: вступ»',
      'Дистанційний курс: регресія, класифікація, кластеризація, оцінювання моделей. Практичні завдання на Python.',
      6, 3, '2025-02-10', 18, 'Реєстрація за корпоративною поштою університету'),
  (8, 'Шаблони та вимоги до оформлення кваліфікаційних робіт',
      'Вимоги до структури та оформлення бакалаврських і магістерських робіт, шаблон титульної сторінки.',
      2, 4, '2024-10-01', 24, 'Вільний доступ'),
  (9, 'Розклад занять і навчальні плани кафедри',
      'Веб-сервіс з актуальним розкладом, навчальними планами та графіком консультацій.',
      8, 5, '2023-09-01', NULL, 'Відкритий доступ');

INSERT INTO resource_authors (resource_id, author_id, role) VALUES
  (1, 1, 'автор'), (1, 5, 'співавтор'),
  (2, 3, 'автор'), (2, 4, 'співавтор'),
  (3, 2, 'лектор'),
  (4, 6, 'відповідальний'),
  (5, 1, 'автор'), (5, 3, 'співавтор'),
  (6, 2, 'укладач'),
  (7, 5, 'автор'),
  (8, 2, 'укладач'), (8, 1, 'рецензент'),
  (9, 6, 'відповідальний');

INSERT INTO resource_urls (resource_id, url, description) VALUES
  (1, 'https://dept.example.edu.ua/library/haskell-textbook.pdf', 'PDF-версія'),
  (1, 'https://moodle.example.edu.ua/course/view.php?id=101', 'Курс у Moodle'),
  (2, 'https://moodle.example.edu.ua/course/view.php?id=214', 'Сторінка в Moodle'),
  (3, 'https://video.example.edu.ua/playlist/algorithms', 'Плейлист лекцій'),
  (4, 'https://dept.example.edu.ua/software/matlab', 'Інструкція з підключення до сервера ліцензій'),
  (5, 'https://github.com/example-dept/distributed-systems-examples', 'GitHub-репозиторій'),
  (6, 'https://dept.example.edu.ua/publications', 'Веб-інтерфейс бази'),
  (7, 'https://courses.example.edu.ua/ml-intro', 'Сторінка курсу'),
  (8, 'https://dept.example.edu.ua/docs/thesis-requirements.pdf', 'Вимоги (PDF)'),
  (8, 'https://dept.example.edu.ua/docs/thesis-template.docx', 'Шаблон (DOCX)'),
  (9, 'https://dept.example.edu.ua/schedule', 'Розклад');

INSERT INTO users (id, full_name, category, study_group, email, registered_on) VALUES
  (1, 'Іваненко Дмитро Сергійович', 'студент', 'ІПС-42', 'd.ivanenko@student.example.edu.ua', '2023-09-05'),
  (2, 'Литвиненко Олена Ігорівна', 'студент', 'ІПС-41', 'o.lytvynenko@student.example.edu.ua', '2023-09-06'),
  (3, 'Ткаченко Артем Вікторович', 'студент', 'ТТП-32', NULL, '2024-09-02'),
  (4, 'Кравчук Марія Олегівна', 'аспірант', NULL, 'm.kravchuk@example.edu.ua', '2024-10-01'),
  (5, 'Савченко Віктор Петрович', 'викладач', NULL, 'v.savchenko@dept.example.edu.ua', '2022-09-01'),
  (6, 'Романюк Анна Юріївна', 'співробітник кафедри', NULL, 'a.romaniuk@dept.example.edu.ua', '2022-09-01'),
  (7, 'Демченко Ігор Васильович', 'зовнішній користувач', NULL, NULL, '2025-11-20');

INSERT INTO usage_stats (resource_id, user_id, access_time, action, duration_min) VALUES
  (1, 1, '2026-02-10 10:15:00', 'перегляд', 45),
  (1, 2, '2026-02-11 18:40:00', 'завантаження', 2),
  (1, 3, '2026-03-02 09:05:00', 'перегляд', 30),
  (1, 1, '2026-09-15 20:10:00', 'перегляд', 60),
  (1, 5, '2026-09-16 08:30:00', 'перегляд', 15),
  (2, 1, '2026-03-12 14:00:00', 'завантаження', 1),
  (2, 2, '2026-03-13 16:20:00', 'перегляд', 25),
  (2, 3, '2026-10-01 11:45:00', 'друк', 5),
  (3, 1, '2026-04-05 19:00:00', 'онлайн-робота', 90),
  (3, 2, '2026-04-06 21:30:00', 'онлайн-робота', 75),
  (3, 3, '2026-10-20 17:10:00', 'онлайн-робота', 50),
  (4, 5, '2026-02-20 10:00:00', 'онлайн-робота', 120),
  (4, 3, '2026-03-21 12:00:00', 'онлайн-робота', 80),
  (5, 1, '2026-09-25 15:30:00', 'завантаження', 3),
  (5, 4, '2026-10-02 13:15:00', 'перегляд', 40),
  (6, 4, '2026-05-14 10:30:00', 'перегляд', 35),
  (6, 5, '2026-05-15 09:00:00', 'перегляд', 20),
  (6, 4, '2026-11-03 11:00:00', 'завантаження', 2),
  (7, 2, '2026-06-01 18:00:00', 'онлайн-робота', 110),
  (7, 1, '2026-06-03 19:20:00', 'онлайн-робота', 95),
  (8, 1, '2026-10-10 12:40:00', 'завантаження', 1),
  (8, 2, '2026-10-11 13:00:00', 'перегляд', 10),
  (9, 6, '2026-09-01 08:00:00', 'перегляд', 5),
  (9, 1, '2026-09-01 08:20:00', 'перегляд', 3),
  (9, 7, '2026-11-12 16:45:00', 'перегляд', 4);
