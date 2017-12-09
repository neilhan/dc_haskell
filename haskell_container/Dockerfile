FROM haskell

RUN apt-get update && apt-get install -y apt-utils xz-utils make # sudo

RUN useradd -ms /bin/bash hs # && adduser hs sudo

USER hs
WORKDIR /home/hs

