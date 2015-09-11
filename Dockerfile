FROM ubuntu
MAINTAINER Benjamin Streb <bstreb@gmail.com>
RUN mkdir /clike
RUN apt-get -y update && apt-get -y install libpcre3 llvm-3.4 ghc cabal-install
RUN cabal install ghc-mod
VOLUME /clike
