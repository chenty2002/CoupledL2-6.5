init:
	git clone https://github.com/OpenXiangShan/CoupledL2.git coupledL2
	git clone https://github.com/OpenXiangShan/rocket-chip.git
	git clone https://github.com/OpenXiangShan/Utility.git utility
	cd utility && git checkout baacebea4bf173b2b676e99f5bb0f9abf10a5f48 && cd ..
	cd coupledL2 && git checkout f6e0ce9cd54b8a0eba40a8dc82f7893b3db5800f && cd ..
	cd rocket-chip && git checkout 90ae9a9314d444ade097fd5a471916869559b4fd && cd ..
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
