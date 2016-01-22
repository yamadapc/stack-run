FROM haskell:7.10
RUN ["cabal", "update"]
ADD . /app
WORKDIR /app
RUN ["cabal", "sandbox", "init"]
RUN ["cabal", "install", "--only-dep", "-j4"]
RUN ["cabal", "build"]
