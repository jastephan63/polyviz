-- Demo schema + seed data executed by pv_run_sql_file().
CREATE TABLE IF NOT EXISTS product_dim (
  product TEXT PRIMARY KEY,
  category TEXT NOT NULL,
  unit_price REAL NOT NULL
);

DELETE FROM product_dim;

INSERT INTO product_dim (product, category, unit_price) VALUES
  ('Aster',   'Analytics',      129.00),
  ('Betula',  'Analytics',       89.50),
  ('Cedrus',  'Infrastructure', 210.00),
  ('Dahlia',  'Infrastructure', 159.99),
  ('Erica',   'Devices',         49.00);
