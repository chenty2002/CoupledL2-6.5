#!/bin/bash

sed -i 's/\r$//' coupledL2/src/main/scala/coupledL2/prefetch/Prefetcher.scala
cd coupledL2 && git apply -R ../scripts/coupledL2.diff
cd HuanCun && git apply -R ../../scripts/huancun.diff