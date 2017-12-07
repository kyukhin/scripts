GCC-BUILD-DIR=/export/build/gnu/gcc-avx-internal/build-x86_64-linux

RUNTESTFLAGS=$(TEST) --target_board 'emx-avx-sim{-m32,}'

# We will run all supported tests in libgomp and libmudflap.
libraries=libgomp libmudflap

CHECKS=c c++ fortran
COMPILERS=check-gcc check-c++ check-fortran
RUNTIMES=libgcc libc++ libfortran

libgcc-LIBS=
libc++-LIBS=libstdc++-v3
libfortran-LIBS=

TIME=/usr/bin/time

-include config.mk

all:

libs: $(libraries)

check: c c++ fortran

c: check-gcc libgcc libs
	echo $@ is finished!

c++: check-c++ libc++
	echo $@ is finished!

fortran: check-fortran libfortran
	echo $@ is finished!

check-gcc check-c++ check-fortran: $(GCC-BUILD-DIR)/gcc/site.exp
	-$(TIME) $(MAKE) -k -C $(GCC-BUILD-DIR)/gcc $@ \
	  RUNTESTFLAGS="$(RUNTESTFLAGS)"
	echo $@ is finished!

$(GCC-BUILD-DIR)/gcc/site.exp:
	$(MAKE) -C $(GCC-BUILD-DIR)/gcc site.exp

libgcc libc++ libfortran:
	-for l in $($@-LIBS); do \
	  $(TIME) $(MAKE) -k -C $(GCC-BUILD-DIR) check-target-$$l \
	    RUNTESTFLAGS="$(RUNTESTFLAGS)"; \
	done
	echo $@ is finished!

$(libraries):
	-$(TIME) $(MAKE) -k -C $(GCC-BUILD-DIR) check-target-$@ \
	  RUNTESTFLAGS="$(RUNTESTFLAGS)"
	echo $@ is finished!
