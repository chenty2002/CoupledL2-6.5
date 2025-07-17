#!/bin/bash

cd coupledL2 && git apply -R ../scripts/coupledL2.diff
cd HuanCun && git apply -R ../../scripts/huancun.diff