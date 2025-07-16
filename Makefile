init:
	git clone https://github.com/OpenXiangShan/CoupledL2.git coupledL2
	git clone https://github.com/OpenXiangShan/rocket-chip.git
	git clone https://github.com/OpenXiangShan/Utility.git utility
	git submodule update --init hardfloat cde && cd ..
	cd coupledL2 && make init
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
