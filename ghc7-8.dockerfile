FROM haskell:7.8
RUN ["cabal", "update"]
ADD . /app
WORKDIR /app
RUN ["cabal", "sandbox", "init"]
RUN ["cabal", "install", "--only-dep", "-j4", "--constraint=Cabal installed"]
RUN ["cabal", "build"]
