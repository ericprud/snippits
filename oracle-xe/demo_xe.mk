#/bin/make
###############################################################################
#                       Make file for OCI,OCCI and ODBC demos
###############################################################################
#  Usage :
# For compiling & linking the cdemo81.c file
#    make -f demo_xe.mk buildoci CDEMOEXE=cdemo81 CDEMOOBJS=cdemo81.o 
#
# For compiling & linking the occidml.cpp
#    make -f demo_xe.mk buildocci EXE=occidml OBJS=occidml.o
#
# For compiling & linking the occiobj.cpp
#    make -f demo_xe.mk occiobj 
#
# For compiling & linking ODBCDEMOS
#     make -f demo_xe.mk buildodbcdemo 
#     ODBCDM_HOME=<path where unix driver manager is installed> 
#     ODBCDEMOOBJS=odbcdemo.o ODBCDEMOEXE=odbcdemo 
#
# In general, for any occi program
#    make -f demo_xe.mk buildocci EXE=<exename> OBJS="<list of dependent objs>"
#
# For compiling all demos
#    make -f demo_xe.mk
#
# NOTE: Please change cc and CC to point to the appropiate location on your
#       machine.
#
###############################################################################

.SUFFIXES: .o .c .cpp

ICINCHOME=$(ORACLE_HOME)/rdbms/public/
ICLIBHOME=$(ORACLE_HOME)/lib/
ICLIBPATH=-L$(ICLIBHOME)
THREADLIBS=-lthread
CCLIB=$(ICLIBPATH) -lclntsh $(THREADLIBS)
OCCILIB=$(ICLIBPATH) -locci -lclntsh $(THREADLIBS)

CCINCLUDES = -I$(ICINCHOME)

CCFLAGS=$(CCINCLUDES) -D_REENTRANT -g -xs 
LDFLAGS=
SO_EXT=.so
EXTLDFLAGS=$(LDFLAGS) -shared

REMOVE=rm -rf
MKLINK=ln
MAKE=make
MAKEFILE=demo_xe.mk

CDEMOEXE=cdemo81
CDEMOOBJS=cdemo81.o
OCCIDEMO=occidml
OCCIOBJDEMO=occiobj
OTT=$(ORACLE_HOME)/bin/ott
OCCIOTTUSR=scott
OCCIOTTPWD=tiger
ODBCDEMOEXE=odbcdemo
ODBCDEMOOBJS=odbcdemo.o
AQDEMOEXE=ociaqdemo00
AQDEMOOBJS=ociaqdemo00.o 
EXTDEMO=extdemo2.so

.cpp.o:
	$(CCC) -c -I$(ICINCHOME) $(CCFLAGS) $<

.c.o:
	$(cc) -c -I$(ICINCHOME) $(ODBCDEMO_INCLUDE) $(CCFLAGS) $<

all: clean buildoci $(OCCIDEMO) $(OCCIOBJDEMO) $(ODBCDEMOEXE)

buildoci: $(LIBCLNT) $(CDEMOOBJS)
	$(CC) -o $(CDEMOEXE) $(LDFLAGS) $(CDEMOOBJS) $(CCLIB)

buildextc: $(LIBCLNT) $(CDEMOOBJS)
	$(CC) -o $(EXTDEMO) $(EXTLDFLAGS) $(CDEMOOBJS)

buildocci: $(LIBCLNT) $(OBJS)
	$(CCC) -o $(EXE) $(LDFLAGS) $(OBJS) $(OCCILIB)

buildaq: $(LIBCLNT) $(AQDEMOOBJS)
	$(CC) -o $(AQDEMOEXE) $(LDFLAGS) $(AQDEMOOBJS) $(CCLIB)

$(CDEMOEXE):
	$(MAKE) -f $(MAKEFILE) buildoci OBJS=$@.o EXE=$@

$(EXTDEMO):
	$(MAKE) -f $(MAKEFILE) buildextc OBJS=$@.o SHARED_LIBNAME=$@

$(OCCIDEMO):
	$(MAKE) -f $(MAKEFILE) buildocci OBJS=$@.o EXE=$@

$(AQDEMOEXE):
	$(MAKE) -f $(MAKEFILE) buildaq OBJS=$@.o EXE=$@

$(OCCIOBJDEMO):
	$(OTT) userid=$(OCCIOTTUSR)/$(OCCIOTTPWD) \
                intype=$@.typ \
                outtype=$@out.type \
                code=cpp \
                hfile=$@.h \
                cppfile=$@o.cpp \
                attraccess=private \
                unicode=none
	$(MAKE) -f $(MAKEFILE) buildocci OBJS="$@.o $@m.o $@o.o" EXE=$@

ODBCDM_HOME=$(ORACLE_HOME)/odbc/public/osds/unixODBC
ODBCDM_INCLUDE=$(ODBCDM_HOME)/include
ODBCDM_LIBHOME=$(ODBCDM_HOME)/lib
ODBCDEMO_INCLUDE=-I$(ODBCDM_INCLUDE) -I. 
ODBCDMLIB=-lodbc

buildodbcdemo: $(ODBCDEMOOBJS) 
	$(cc) -o $(ODBCDEMOEXE) $(ODBCDEMOOBJS) -L$(ODBCDM_LIBHOME) $(ODBCDMLIB)

$(ODBCDEMOEXE):
	$(MAKE) -f $(MAKEFILE) buildodbcdemo ODBCDEMOOBJS=$@.o ODBCDEMOEXE=$@

clean:
	$(REMOVE) cdemo81 cdemo81.o occidml occidml.o occiobj occiobj.o occiobjo* occiobjm* occiobj.h occiobjout.type odbcdemo odbcdemo.o extdemo2.o extdemo2.so


#
# This port-specific file is currently empty on Solaris. Product
# lines may use this file to override compiler definitions and
# flags used in occi.mk.
#

# Linux compiler definitions
CC=/usr/bin/gcc
cc=/usr/bin/gcc
CCC=/usr/bin/g++

CCFLAGS=$(CCINCLUDES) -DLINUX -D_GNU_SOURCE -D_REENTRANT -g
LDFLAGS=-g

# This macro CCINCLUDES has to be redefined on Linux because of
# the existence of the 'new' directory in t_work. The name new
# clashes with a system header file.
CCINCLUDES = -idirafter .

THREADLIBS=-lpthread
