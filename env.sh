# This script will setup XiangShan environment variables

export WXV_PROJECT_ROOT=$(pwd)
export NEMU_HOME=$(pwd)/NEMU
export AM_HOME=$(pwd)/abstract-machine
export NOOP_HOME=$(pwd)/WenXuanVec
# export DRAMSIM3_HOME=$(pwd)/DRAMsim3

echo SET WXV_PROJECT_ROOT: ${WXV_PROJECT_ROOT}
echo SET NEMU_HOME: ${NEMU_HOME}
echo SET AM_HOME: ${AM_HOME}
echo SET NOOP_HOME \(RTL Home\): ${NOOP_HOME}
# echo SET DRAMSIM3_HOME: ${DRAMSIM3_HOME}
