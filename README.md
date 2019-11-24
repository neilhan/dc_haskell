# dc_haskell - Docker Compose and Docker files for a haskell container

```
docker-compose build
docker-compose run haskell bash

cd Projects/my-project
stack install hpack
stack setup
stack build
stack exec my-project
```

To run scotty-hello:
```
docker-compose run --rm haskell bash
```

This will publish the 3000 port. When running not as host mode for its network, will need to do this:
```
docker-compose run -p 3000 --rm haskell bash
```

2017 12 08 Started experiementing with Haskell. It's interesting, powerful, mind bending. Its purity is satisfying.


# Start a new project
Referce: https://docs.haskellstack.org/en/stable/README/
```
cd Projects
stack new real-world-haskell
cd real-world-haskell
stack setup
stack build
stack exec real-world-haskell

# run ghci
stack ghci

# run a haskell file
stack exec -- runghc Main.hs
```
