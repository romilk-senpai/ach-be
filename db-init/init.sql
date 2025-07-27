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
  local_id INT NOT NULL,
  thread_id INT NOT NULL REFERENCES threads(id) ON DELETE CASCADE,
  board_id INT NOT NULL REFERENCES boards(id),
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  subject TEXT,
  author TEXT,
  content TEXT NOT NULL
);

CREATE UNIQUE INDEX uniq_local_id_per_board ON posts(board_id, local_id);

CREATE OR REPLACE FUNCTION assign_local_post_id()
RETURNS TRIGGER AS $$
DECLARE
  thread_board_id INT;
  max_local_id INT;
BEGIN
  SELECT board_id INTO thread_board_id
  FROM threads
  WHERE id = NEW.thread_id;

  NEW.board_id := thread_board_id;

  SELECT COALESCE(MAX(local_id), 0) INTO max_local_id
  FROM posts WHERE board_id = thread_board_id;

  NEW.local_id := max_local_id + 1;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER set_local_post_id
BEFORE INSERT ON posts
FOR EACH ROW
EXECUTE FUNCTION assign_local_post_id();

INSERT INTO posts (thread_id, subject, author, content)
VALUES
  (1, 'subject 1', 'ayanokojimode', 'sample post 1'),
  (2, 'subject 2', 'thorns-1', 'sample post 2');

CREATE TABLE media (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  file_name TEXT NOT NULL,
  file_path TEXT NOT NULL,
  content_type TEXT NOT NULL
);
