init:
	git submodule update --init
	cd rocket-chip && git submodule update --init hardfloat cde
	cd coupledL2 && make init

compile:
	mill -i CoupledL2Assume.compile

verify:
	./scripts/modify_coupledL2.sh
	mill -i CoupledL2Assume.test.runMain coupledL2Assume.VerifyTop -td build

auto:
	./scripts/modify_coupledL2.sh
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
