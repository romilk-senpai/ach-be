CREATE TABLE categories (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL
);

INSERT INTO categories (name)
VALUES
  ('Japanese Culture'),
  ('Video Games'),
  ('Interests');

CREATE TABLE boards (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  category_id INT NOT NULL REFERENCES categories(id) ON DELETE CASCADE,
  slug TEXT NOT NULL,
  description TEXT NOT NULL
);

INSERT INTO boards (name, category_id, slug, description)
VALUES
  ('Anime & Manga', 1, 'a', 'Anime & Manga'),
  ('Anime/Cute', 1, 'c', 'Anime/Cute'),
  ('Video Games', 2, 'v', 'Video Games'),
  ('Science & Math', 3, 'vg', 'Science & Math'),
  ('Toys', 3, 'toy', 'Toys');

CREATE TABLE threads (
  id SERIAL PRIMARY KEY,
  board_id INT NOT NULL REFERENCES boards(id) ON DELETE CASCADE
);

INSERT INTO threads (board_id)
VALUES
  (1),
  (5);

CREATE TABLE posts (
  id SERIAL PRIMARY KEY,
  thread_id INT NOT NULL REFERENCES threads(id) ON DELETE CASCADE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  subject TEXT,
  author TEXT,
  content TEXT NOT NULL
);

INSERT INTO posts (thread_id, subject, author, content)
VALUES
  (1, 'subject 1', 'ayanokojimode', 'sample post 1'),
  (2, 'subject 2', 'thorns-1', 'sample post 2');

CREATE TABLE media (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  file_name TEXT NOT NULL,
  file_path TEXT NOT NULL
);
