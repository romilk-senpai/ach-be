CREATE TABLE boards (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  category INT NOT NULL
);

INSERT INTO boards (name, category)
VALUES
  ('Anime & Manga', 1),
  ('Anime/Cute', 1),
  ('Hentai', 1),
  ('Science & Math', 2),
  ('Toys', 2);

CREATE TABLE threads (
  id SERIAL PRIMARY KEY,
  board_id INT NOT NULL REFERENCES boards(id) ON DELETE CASCADE,
  title TEXT NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  author TEXT
);

INSERT INTO threads (board_id, title, author)
VALUES
  (1, 'Sample Anime & Manga topic', 'ayanokojimode'),
  (5, 'Sample Toys topic', 'thorns-1');

CREATE TABLE posts (
  id SERIAL PRIMARY KEY,
  thread_id INT NOT NULL REFERENCES threads(id) ON DELETE CASCADE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  author TEXT,
  content TEXT NOT NULL
);

INSERT INTO posts (thread_id, author, content)
VALUES
  (1, 'ayanokojimode', 'sample post 1'),
  (2, 'thorns-1', 'sample post 2');

