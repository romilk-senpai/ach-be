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

