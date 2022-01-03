#FC = ifort -g -qopenmp -qopenmp-offload=host
#FC = xlf2008_r -qsmp=omp -qoffload -Wx,-nvvm-compile-options=-opt=0
#FC = xlf_r -qsmp=omp -qoffload -qtgtarch=auto  -qcuda  -Wx,-nvvm-compile-options=-opt=0
#FC = xlf2008_r -qsmp=omp 
#FC = xlf2008_r -qsmp=omp -qoffload -qtgtarch=auto  -qcuda  -Wx,-nvvm-compile-options=-opt=0

# see https://www.intel.com/content/www/us/en/develop/documentation/get-started-with-cpp-fortran-compiler-openmp/top.html
# ifx -qopenmp -fopenmp-targets=spir64 matmul_offload.f90
# to run: export OMP_TARGET_OFFLOAD=MANDATORY
#
FC = ifx -g -traceback
FC = ifx -qopenmp -fopenmp-targets=spir64 -g -traceback

all : tbp.exe

tbp_submod.o : tbp.o tbp_submod.F90
	$(FC) $(WORKAROUND) -c tbp_submod.F90

tbp.o : tbp.F90
	$(FC) $(WORKAROUND) -c tbp.F90

tbp.exe : tbp_submod.o
	$(FC) -o tbp.exe tbp.o tbp_submod.o

clean:
	/bin/rm -f *.o *.mod *.smod tbp.exe

