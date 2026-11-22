-- Створення користувача для програми (виконати від root ПІСЛЯ schema.sql).
-- Потрібно лише при ручному встановленні MySQL / XAMPP; у Docker користувач створюється автоматично.
CREATE USER IF NOT EXISTS 'lab'@'%' IDENTIFIED BY 'lab';
CREATE USER IF NOT EXISTS 'lab'@'localhost' IDENTIFIED BY 'lab';
GRANT ALL PRIVILEGES ON dept_resources.* TO 'lab'@'%';
GRANT ALL PRIVILEGES ON dept_resources.* TO 'lab'@'localhost';
FLUSH PRIVILEGES;
