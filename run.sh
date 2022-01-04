#!/bin/bash
source /opt/intel/inteloneapi/setvars.sh > /dev/null 2>&1
#make run_dpcpp
#make run_omp
#./tbp.exe

#export LIBOMPTARGET_DEBUG=1
#export LIBOMPTARGET_DEBUG=2
#export LIBOMPTARGET_INFO=4

./tb.exe

