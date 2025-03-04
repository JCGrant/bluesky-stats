FROM haskell

ENV TARGET=

WORKDIR /app
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
  wget \
  gnupg \
  lsb-release \
  sudo \
  && rm -rf /var/lib/apt/lists/*

RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | gpg --dearmor -o /usr/share/keyrings/postgresql-archive-keyring.gpg

RUN echo "deb [signed-by=/usr/share/keyrings/postgresql-archive-keyring.gpg] http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list

RUN apt-get update && apt-get install -y \
  libpq-dev \
  && rm -rf /var/lib/apt/lists/*

COPY bluesky-stats.cabal .
RUN cabal update
RUN cabal build --only-dependencies

COPY app/ ./app
COPY agent/ ./agent
COPY src/ ./src
COPY static/ ./static
COPY Makefile .

RUN cabal build

ENTRYPOINT ["make", "run"]
