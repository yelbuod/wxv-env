BUILD_DIR = ./build

TOP = XSTop
SIM_TOP = SimTop

FPGATOP = top.TopMain
SIMTOP  = top.SimTop

TOP_V = $(BUILD_DIR)/$(TOP).v
SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).v

SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')

VERILATOR_FLAGS = -cc --exe -Os -x-assign 0 \
	--assert --trace

TIMELOG = $(BUILD_DIR)/time.log
TIME_CMD = time -a -o $(TIMELOG)

default: sim-verilog

# verilog:
# 	mkdir -p $(BUILD_DIR)
# 	mill -i wenxuanvec.test.runMain $(SIMTOP) -td $(BUILD_DIR)

$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	@echo "\n[mill] Generating Verilog files..." > $(TIMELOG)
	@date -R | tee -a $(TIMELOG)
	$(TIME_CMD) mill -i wenxuanvec.test.runMain $(SIMTOP) -td $(@D)
	
sim-verilog: $(SIM_TOP_V)

# verilator simulation
emu: sim-verilog
	$(MAKE) -C ./difftest emu SIM_TOP=$(SIM_TOP) DESIGN_DIR=$(NOOP_HOME)

emu-run: emu
	$(MAKE) -C ./difftest emu-run SIM_TOP=$(SIM_TOP) DESIGN_DIR=$(NOOP_HOME)

help:
	mill -i wenxuanvec.test.runMain $(SIMTOP) --help

idea:
	mill -i mill.scalalib.GenIdea/idea
	
clean:
	-rm -rf $(BUILD_DIR)
	-rm -rf obj_dir
	-rm -rf logs

.PHONY: verilog emu help clean
