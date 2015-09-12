FROM ubuntu
MAINTAINER Benjamin Streb <bstreb@gmail.com>
RUN mkdir /clike
RUN apt-get -y update && apt-get -y install libpcre3 llvm-3.4 ghc cabal-install happy g++
RUN cabal update && cabal install ghc-mod
ENV PATH /root/.cabal/bin/:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
WORKDIR /clike
VOLUME /clike
