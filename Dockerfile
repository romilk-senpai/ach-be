FROM haskell:9-slim-bullseye

ENV DOCKERIZE_VERSION v0.9.3

WORKDIR /app

RUN echo "deb http://apt.postgresql.org/pub/repos/apt bullseye-pgdg main" > /etc/apt/sources.list.d/pgdg.list && \
    curl -sSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - && \
    apt-get update && \
    apt-get install -y libpq-dev wget && \
    wget -O - https://github.com/jwilder/dockerize/releases/download/$DOCKERIZE_VERSION/dockerize-linux-amd64-$DOCKERIZE_VERSION.tar.gz | tar xzf - -C /usr/local/bin && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

COPY . .

RUN cabal update && cabal build --only-dependencies -j

RUN cabal install

CMD dockerize -wait tcp://db:5432 -timeout 30s ach
