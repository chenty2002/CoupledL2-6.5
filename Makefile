init:
	git submodule update --init
	cd coupledL2 && git submodule update --init HuanCun rocket-chip utility
	cd coupledL2/rocket-chip && git submodule update --init hardfloat cde
	./scripts/modify_coupledL2.sh

MILL_VER := $(subst $(newline),,$(shell cat .mill-version))
MILL ?= mill-$(MILL_VER)

compile:
	$(MILL) -i CoupledL2Assume.compile

verify:
	$(MILL) -i CoupledL2Assume.test.runMain coupledL2Assume.VerifyTop -td build

auto:
	$(MILL) -i CoupledL2Assume.test.runMain coupledL2Assume.AutoVerify -td build

clean:
	rm -rf ./build

bsp:
	$(MILL) -i mill.bsp.BSP/install

idea:
	$(MILL) -i mill.idea.GenIdea/idea

reformat:
	$(MILL) -i __.reformat

checkformat:
	$(MILL) -i __.checkFormat
