FROM haskell

RUN apt-get update && apt-get install -y apt-utils xz-utils make

RUN useradd -ms /bin/bash hs
USER hs
WORKDIR /home/hs

