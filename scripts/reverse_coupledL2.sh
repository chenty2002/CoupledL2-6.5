#!/bin/bash

cd coupledL2 && git apply -R ../scripts/coupledL2.diff
cd huancun && git apply -R ../../scripts/huancun.diff
sed -i 's/$/\r/' ./src/main/scala/coupledL2/prefetch/Prefetcher.scala