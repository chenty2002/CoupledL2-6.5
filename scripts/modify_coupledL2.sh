#!/bin/bash

sed -i 's/\r$//' coupledL2/src/main/scala/coupledL2/prefetch/Prefetcher.scala
cd coupledL2 && git apply ../scripts/coupledL2.diff
cd huancun && git apply ../../scripts/huancun.diff