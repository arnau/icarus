FROM haskell:7.10
MAINTAINER Arnau Siches <asiches@gmail.com>

ENV TERM screen-256color

COPY . /source
WORKDIR /source

RUN echo ":set prompt \"\\ESC[35mghci> \\ESC[m\"" > /root/.ghci

RUN cabal update \
 && cabal install --only-dependencies --enable-tests

CMD ["ghci"]
