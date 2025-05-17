CREATE TABLE categories (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL
);

INSERT INTO categories (name)
VALUES
  ('Japanese Culture'),
  ('Video Games'),
  ('Interests')

CREATE TABLE boards (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  category_id INT NOT NULL REFERENCES categories(id) ON DELETE CASCADE,
  slug TEXT NOT NULL
);

INSERT INTO boards (name, category_id, slug)
VALUES
  ('Anime & Manga', 1, 'a'),
  ('Anime/Cute', 1, 'c'),
  ('Video Games', 2, 'v'),
  ('Science & Math', 3, 'vg'),
  ('Toys', 3, 'toy');

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
  topic TEXT,
  author TEXT,
  content TEXT NOT NULL
);

INSERT INTO posts (thread_id, topic, author, content)
VALUES
  (1, 'topic 1', 'ayanokojimode', 'sample post 1'),
  (2, 'topic 2', 'thorns-1', 'sample post 2');

