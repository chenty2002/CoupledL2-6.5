init:
	git clone https://github.com/OpenXiangShan/CoupledL2.git coupledL2
	git clone https://github.com/OpenXiangShan/rocket-chip.git
	git clone https://github.com/OpenXiangShan/Utility.git utility
	cd utility && git checkout 708d3eb44fc231608b87ac1242b28a7445fe6637 && cd ..
	cd coupledL2 && git checkout 7c2062903b9a1afda5bb1191081e2fc38b1ccc1e && cd ..
	cd rocket-chip && git checkout 175dfe096e3b7c630f93ef328df1cf0b2ed55de1 && cd ..
	cd rocket-chip && git submodule update --init hardfloat cde && cd ..
	cd coupledL2 && make init && cd ..
	./scripts/modify_coupledL2.sh

compile:
	mill -i CoupledL2Assume.compile

verify:
	mill -i CoupledL2Assume.test.runMain coupledL2Assume.VerifyTop -td build

auto:
	mill -i CoupledL2Assume.test.runMain coupledL2Assume.AutoVerify -td build

clean:
	rm -rf ./build

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.idea.GenIdea/idea

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat
