FROM haskell

RUN apt-get update && apt-get install -y apt-utils xz-utils make # sudo

RUN useradd -ms /bin/bash hs 

USER hs

VOLUME /home/hs/.ghc
VOLUME /home/hs/.stack
VOLUME /HOME/hs/Projects

WORKDIR /home/hs

