FROM haskell

RUN apt-get update && apt-get install -y apt-utils xz-utils make # sudo

RUN useradd -ms /bin/bash hs 

EXPOSE 3000

USER hs

VOLUME /home/hs/.ghc
VOLUME /home/hs/.stack
VOLUME /home/hs/.local
VOLUME /HOME/hs/Projects

WORKDIR /home/hs

