#!/bin/bash

cd ./utility && git checkout 708d3eb44fc231608b87ac1242b28a7445fe6637 && cd ..

cd ./coupledL2 && git checkout 7c2062903b9a1afda5bb1191081e2fc38b1ccc1e

cd utility && git checkout 708d3eb44fc231608b87ac1242b28a7445fe6637 && cd ..
cd Huancun && git checkout 6e2322ec08b0c02d81d69330c74e146d8bdb41aa && cd ..
cd rocket-chip && git checkout 175dfe096e3b7c630f93ef328df1cf0b2ed55de1 && cd ..

cd ..