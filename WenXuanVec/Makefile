BUILD_DIR = ./build
WENXUANVEC_HOME = $(shell pwd)

VERILATOR_FLAGS = -cc --exe -Os -x-assign 0 \
	--assert --trace

default: verilog

verilog:
	mkdir -p $(BUILD_DIR)
	mill -i wenxuanvec.runMain TopMain -td $(BUILD_DIR)

emu: verilog
	cd $(WENXUANVEC_HOME)/difftest && $(MAKE) emu

help:
	mill -i wenxuanvec.runMain TopMain --help

idea:
	mill -i mill.scalalib.GenIdea/idea
	
clean:
	-rm -rf $(BUILD_DIR)
	-rm -rf obj_dir
	-rm -rf logs

.PHONY: verilog emu help clean
