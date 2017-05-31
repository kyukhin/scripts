# To build and test gcc, the following rpms are required:
# 	1. glibc-devel
# 	2. zlib-devel
# 	3. zlib-static
# 	4. gmp-static
# 	5. gmp-devel
# 	6. mpfr-devel
# 	7. libmpc-devel
# 	8. elfutils-libelf-devel
# 	9. ppl-devel
# 	10. cloog-ppl-devel
# 	11. dejagnu
# 	12. bison
# 	13. flex
# 	14. texinfo
# 	15. binutils
# 	16. gettext
# 	17. m4
# 	18. make
# On Linux/Intel64, the additional rpms are
# 	1. glibc-devel.i686
# 	2. zlib-devel.i686
# You can use "http_proxy= yum install" to install those required
# rpms.
#
# Copy this Makefile in an empty build directory and use "nohup make one"
# to configure, build and test gcc as well as send test results with the
# default languages. The output from gcc build will be in nohup.out.

# You can use it to share the same Makefile on different machines.
# include ../../common.mk

# Define it to the gcc source tree directory.
GCC-SOURCE-DIR=/export/users/kyukhin/gcc/git/gcc

ifndef GCC-SOURCE-DIR
.DEFAULT:
	@echo Please define GCC-SOURCE-DIR.
	@exit 1
else
# Which gcc branch we are building for.
BRANCH-NAME=
#haswell

# Add '-' before the name of branch.
ifdef BRANCH-NAME
BRANCH=-$(BRANCH-NAME)
endif

VERSION=$(shell cat $(GCC-SOURCE-DIR)/gcc/BASE-VER)

# Where gcc will be installed.
GCC-PREFIX=/usr/gcc-$(VERSION)$(BRANCH)
LOCAL-PREFIX=/usr/local

BUILD-ARCH:=$(shell arch)

# What arch gcc is configured for.
ifndef CPU
CPU=$(BUILD-ARCH)
endif

ARCH=$(CPU)

ifeq (i586,$(CPU))
ARCH=i386
endif

ifeq (i686,$(CPU))
ARCH=i386
endif

ifndef OS
OS=linux
endif

comma:= ,
empty:=
space:= $(empty) $(empty)

FLAGS-TO-PASS=

TARGET=$(CPU)-$(OS)

CC=gcc
#-O0 -g3
CXX=g++
#-O0 -g3

ifeq (i386,$(ARCH))
ifneq ($(BUILD-ARCH),$(CPU))
TARGET-FLAGS=$(TARGET)
CC+= -m32
CXX+= -m32
# Need 32bit linker for LTO.  */
PATH:=/usr/local32/bin:$(PATH)
endif
endif

FLAGS-TO-PASS+=CC="$(CC)"
FLAGS-TO-PASS+=CXX="$(CXX)"

# Number of procesors on line.  For parallel build.
NUM-CPUS:=$(shell /usr/bin/getconf _NPROCESSORS_ONLN)
# Limit NUM-CPUS to 12
NUM-CPUS:=$(shell if [ $(NUM-CPUS) -ge 12 ]; then echo 12; else echo $(NUM-CPUS);fi)
PARALLELMFLAGS:=-j $(NUM-CPUS)

ifeq (linux,$(OS))
ENABLE-FLAGS=--enable-clocale=gnu --with-system-zlib
endif
#ENABLE-FLAGS+=--disable-libgcj
#ENABLE-FLAGS+=--disable-bootstrap
#ENABLE-FLAGS+=--enable-checking=assert

ENABLE-FLAGS+=--with-demangler-in-ld

# Select languages to build.
#LANG-FLAGS=--enable-languages=c
#LANG-FLAGS=--enable-languages=c,go
#LANG-FLAGS=--enable-languages=c,ada
#LANG-FLAGS=--enable-languages=c,fortran
#LANG-FLAGS=--enable-languages=c,c++,fortran
#LANG-FLAGS=--enable-languages=c,c++
#LANG-FLAGS=--enable-languages=c,c++,lto
#LANG-FLAGS=--enable-languages=c,c++,fortran,java,lto,objc,ada,obj-c++,go

CONFIG-FLAGS+=$(ENABLE-FLAGS)
CONFIG-FLAGS+=$(LANG-FLAGS)

# We can disable bootstrap for debug purpose.
#CONFIG-FLAGS=--enable-languages=c,c++,fortran,objc --disable-bootstrap
#CONFIG-FLAGS=--enable-languages=c,c++,fortran --disable-bootstrap
CONFIG-FLAGS=--enable-languages=c,c++ --disable-bootstrap
#CONFIG-FLAGS=--enable-languages=c,c++,fortran
#CONFIG-FLAGS=--enable-languages=c,c++,java
#CONFIG-FLAGS=--enable-languages=c,c++

#CONFIG-FLAGS+=--with-arch=native --with-cpu=native

CONFIG-FLAGS+=$(TARGET-FLAGS)
#CONFIG-FLAGS+=--prefix=$(GCC-PREFIX)
#CONFIG-FLAGS+=--with-local-prefix=$(LOCAL-PREFIX)
#CONFIG-FLAGS+=--enable-gnu-indirect-function
CONFIG-FLAGS+=--with-as=/export/users/kyukhin/binutils/build/release/bin/as

# msticlxl28
#CONFIG-FLAGS+=--with-gmp=/usr/local/lib --with-mpfr=/usr/local/lib --with-mpc=/usr/local/lib LDFLAGS="-static -static-libgcc"

ifeq (atom,$(BRANCH-NAME))
# Use "-march=atom -mtune=atom" on Atom branch.
CONFIG-FLAGS+=--with-arch=atom --with-cpu=atom
endif

ifeq (corei7,$(BRANCH-NAME))
# Use "-march=corei7 -mtune=corei7" on Core i7 branch.
CONFIG-FLAGS+=--with-arch=corei7 --with-cpu=corei7
endif

ifeq (corei7-avx,$(BRANCH-NAME))
# Use "-march=corei7-avx -mtune=corei7-avx" on Core i7 AVX branch.
CONFIG-FLAGS+=--with-arch=corei7-avx --with-cpu=corei7-avx
endif

ifeq (haswell,$(BRANCH-NAME))
# Use "-march=corei7-avx -mtune=corei7-avx" on Haswell branch.
CONFIG-FLAGS+=--with-arch=corei7-avx --with-cpu=corei7-avx
endif


ifeq (native,$(BRANCH-NAME))
# Use "-march=native -mtune=native" on native branch.
CONFIG-FLAGS+=--with-arch=native --with-cpu=native
endif

BUILD-DIR=build-$(TARGET)

ifeq (,$(findstring --disable-bootstrap, $(CONFIG-FLAGS)))
BUILD=bootstrap-lean
ifeq (yes,$(PROFILEDBOOTSTRAP))
BUILD=profiledbootstrap
else
BUILD=bootstrap
endif
else
BUILD=all
BOOT_CFLAGS=-g
FLAGS-TO-PASS+=BOOT_CFLAGS="$(BOOT_CFLAGS)"
FLAGS-TO-PASS+=CFLAGS="$(BOOT_CFLAGS)"
endif

ifeq (yes,$(LTO))
CONFIG-FLAGS+=--with-build-config=bootstrap-lto
endif

#FLAGS-TO-PASS+=CXXFLAGS="$(BOOT_CFLAGS)"

# Use SDE emulator to run test.
BOARD=sde-sim

# Normal test.
#BOARD=unix

#GNU-PREFIX=/opt/gnu
ifdef GNU-PREFIX
CONFIG-FLAGS+=--enable-cloog-backend=isl
ifeq (x86_64,$(ARCH))
CONFIG-FLAGS+=--with-ppl-include=$(GNU-PREFIX)/include
CONFIG-FLAGS+=--with-ppl-lib=$(GNU-PREFIX)/lib64
CONFIG-FLAGS+=--with-cloog-include=$(GNU-PREFIX)/include
CONFIG-FLAGS+=--with-cloog-lib=$(GNU-PREFIX)/lib64
else
CONFIG-FLAGS+=--with-ppl=$(GNU-PREFIX)
CONFIG-FLAGS+=--with-cloog=$(GNU-PREFIX)
endif
endif

# RUNTESTFLAGS="--target_board='unix{-m32,}'"

ifeq (ia64,$(ARCH))
PRCTL=prctl --unaligned=always-signal
else
CONFIG-FLAGS+=--with-fpmath=sse
endif

ifeq (x86_64,$(ARCH))
RUNTESTFLAGS=--target_board='$(BOARD){-m32,}'
FLAGS-TO-PASS+=RUNTESTFLAGS=i386.exp="$(RUNTESTFLAGS)"
endif

ifeq (i386,$(ARCH))
RUNTESTFLAGS=--target_board='$(BOARD)'
FLAGS-TO-PASS+=RUNTESTFLAGS="$(RUNTESTFLAGS)"
endif

SANITY-RUNTESTFLAGS=vect.exp i386-costmodel-vect.exp \
		    x86_64-costmodel-vect.exp $(RUNTESTFLAGS)
SANITY-FLAGS-TO-PASS=check RUNTESTFLAGS="$(SANITY-RUNTESTFLAGS)"

#X86-RUNTESTFLAGS=i386.exp=hle-* $(RUNTESTFLAGS)

#X86-RUNTESTFLAGS=i386.exp=avx3_1-vpcmpgtd-1* $(RUNTESTFLAGS)

X86-RUNTESTFLAGS=i386.exp=avx* $(RUNTESTFLAGS)

X86-FLAGS-TO-PASS=check RUNTESTFLAGS="$(X86-RUNTESTFLAGS)"

# Don't report test results. Use it on internal branches.
REPORT=true

# Report test results.  Use it on FSF checkout trees.
REPORT=$(MAKE) report

TIME=/usr/bin/time

# Use to create binary tar ball with
# make install
# make tar
RELEASE-DIR=$(PWD)/release

# Configure and build
tools: config $(BUILD)

#.DEFAULT:
#	$(TIME) $(MAKE) $(PARALLELMFLAGS) -C $(BUILD-DIR) $@ \
#		$(FLAGS-TO-PASS)

r:
	$(TIME) $(MAKE) $(PARALLELMFLAGS) -C $(BUILD-DIR)/gcc \
		$(FLAGS-TO-PASS)
rr:
	$(TIME) $(MAKE) $(PARALLELMFLAGS) -C $(BUILD-DIR) \
		$(FLAGS-TO-PASS)

# Run gcc test
check:
	$(TIME) $(PRCTL) $(MAKE) $(PARALLELMFLAGS) -C $(BUILD-DIR) -k $@ \
		$(FLAGS-TO-PASS)

sanity-check:
	$(TIME) $(MAKE) $(PARALLELMFLAGS) -C $(BUILD-DIR)/gcc -k \
		$(SANITY-FLAGS-TO-PASS)

c:
	$(TIME) $(MAKE) $(PARALLELMFLAGS) -C $(BUILD-DIR)/gcc -k \
		$(X86-FLAGS-TO-PASS)

x86-check:
	$(TIME) $(MAKE) $(PARALLELMFLAGS) -C $(BUILD-DIR)/gcc -k \
		$(X86-FLAGS-TO-PASS)

# Report test results.
report:
	cd $(BUILD-DIR) && \
	  $(GCC-SOURCE-DIR)/contrib/test_summary | sh

# Configure, build, run test and report test results.
one:
	if $(MAKE) tools; then \
	  $(MAKE) check; \
	  $(REPORT); \
	else \
	  exit 1; \
	fi

# Continue to build, run test and report test results.
cont:
	if $(MAKE) $(BUILD); then \
	  $(MAKE) check; \
	  $(REPORT); \
	else \
	  exit 1; \
	fi

# Install
install:
	rm -rf $(RELEASE-DIR)
	$(MAKE) -C $(BUILD-DIR) DESTDIR=$(RELEASE-DIR) $@

# Create a binary tar ball
tar:
	cd $(RELEASE-DIR); \
	VERSION=$$(cat $(GCC-SOURCE-DIR)/gcc/BASE-VER); \
	PHASE=$$(cat $(GCC-SOURCE-DIR)/gcc/DEV-PHASE); \
	if [ -n "$$PHASE" ]; then \
	  PHASE="-$$(cat $(GCC-SOURCE-DIR)/gcc/DATESTAMP)"; \
	fi; \
	tar --owner=root --group=root -cj \
	  -f gcc-$(VERSION)$(BRANCH)$$PHASE.$(ARCH).tar.bz2 .$(GCC-PREFIX)

# Configure gcc
config: clobber
	mkdir -p $(BUILD-DIR)
	cd $(BUILD-DIR); \
	$(FLAGS-TO-PASS) $(GCC-SOURCE-DIR)/configure $(CONFIG-FLAGS)

# Remove build directory.
clobber:
	rm -rf $(BUILD-DIR)
endif
