# dc_haskell - Docker Compose and Docker files for a haskell container

```
docker-compose build
docker-compose run haskell bash

cd Projects/my-project
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

