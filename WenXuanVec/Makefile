BUILD_DIR = ./build

TOP = XSTop
SIM_TOP = SimTop

FPGATOP = top.TopMain
SIMTOP  = top.SimTop

TOP_V = $(BUILD_DIR)/$(TOP).v
SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).v

SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')

MFC ?= 0

# firtool check and download
FIRTOOL_VERSION = 1.61.0
FIRTOOL_URL = https://github.com/llvm/circt/releases/download/firtool-$(FIRTOOL_VERSION)/firrtl-bin-linux-x64.tar.gz
FIRTOOL_PATH = $(shell which firtool 2>/dev/null)
CACHE_FIRTOOL_PATH = $(HOME)/.cache/xiangshan/firtool-$(FIRTOOL_VERSION)/bin/firtool
ifeq ($(MFC),1)
ifeq ($(FIRTOOL_PATH),)
ifeq ($(wildcard $(CACHE_FIRTOOL_PATH)),)
$(info [INFO] Firtool not found in your PATH.)
$(info [INFO] Downloading from $(FIRTOOL_URL))
$(shell mkdir -p $(HOME)/.cache/xiangshan && curl -L $(FIRTOOL_URL) | tar -xzC $(HOME)/.cache/xiangshan)
endif
FIRTOOL_ARGS = --firtool-binary-path $(CACHE_FIRTOOL_PATH)
endif
endif

# common chisel args
ifeq ($(MFC),1)
CHISEL_VERSION = chisel
FPGA_MEM_ARGS = --firtool-opt "--repl-seq-mem --repl-seq-mem-file=$(TOP).v.conf"
SIM_MEM_ARGS = --firtool-opt "--repl-seq-mem --repl-seq-mem-file=$(SIM_TOP).v.conf"
MFC_ARGS = --dump-fir $(FIRTOOL_ARGS) \
           --firtool-opt "-O=release --disable-annotation-unknown --lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing"
RELEASE_ARGS += $(MFC_ARGS)
DEBUG_ARGS += $(MFC_ARGS)
PLDM_ARGS += $(MFC_ARGS)
else
CHISEL_VERSION = chisel3
FPGA_MEM_ARGS = --infer-rw --repl-seq-mem -c:$(FPGATOP):-o:$(@D)/$(@F).conf --gen-mem-verilog full
SIM_MEM_ARGS = --infer-rw --repl-seq-mem -c:$(SIMTOP):-o:$(@D)/$(@F).conf --gen-mem-verilog full
endif

VERILATOR_FLAGS = -cc --exe -Os -x-assign 0 \
	--assert --trace

TIMELOG = $(BUILD_DIR)/time.log
TIME_CMD = time -a -o $(TIMELOG)

default: sim-verilog

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	$(TIME_CMD) mill -i wenxuanvec[$(CHISEL_VERSION)].runMain $(FPGATOP) -td $(@D)

verilog: $(TOP_V)

$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	@echo "\n[mill] Generating Verilog files..." > $(TIMELOG)
	@date -R | tee -a $(TIMELOG)
	$(TIME_CMD) mill -i wenxuanvec[$(CHISEL_VERSION)].test.runMain $(SIMTOP) -td $(@D)

sim-verilog: $(SIM_TOP_V)

# verilator simulation
emu: sim-verilog
	$(MAKE) -C ./difftest emu SIM_TOP=$(SIM_TOP) DESIGN_DIR=$(NOOP_HOME)

emu-run: emu
	$(MAKE) -C ./difftest emu-run SIM_TOP=$(SIM_TOP) DESIGN_DIR=$(NOOP_HOME)

help:
	mill -i wenxuanvec[$(CHISEL_VERSION)].test.runMain $(SIMTOP) --help

idea:
	mill -i mill.scalalib.GenIdea/idea

clean:
	-rm -rf $(BUILD_DIR)
	-rm -rf obj_dir
	-rm -rf logs

.PHONY: verilog emu help clean
