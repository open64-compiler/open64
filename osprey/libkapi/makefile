
##
## Copyright (c) 2000, Intel Corporation
## All rights reserved.
##
## WARRANTY DISCLAIMER
##
## THESE MATERIALS ARE PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
## "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
## LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
## A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL INTEL OR ITS 
## CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
## EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
## PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
## PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
## OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY OR TORT (INCLUDING
## NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THESE
## MATERIALS, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
## Intel Corporation is the author of the Materials, and requests that all
## problem reports or change requests be submitted to it directly at
## http://developer.intel.com/opensource.
##

#
# Get the current directory
#

include Makefile.inc


CWD=$(shell pwd)

ifdef TST_KAPI
   MAIN_SRC     = kapi_main
else
   MAIN_SRC     = 
endif

ifeq ($(HOSTTYPE), WINNT)
	CFLAGS += /I $(SERVER_AREA)/include 
	OBJSUF       = obj
	ASMSUF       = asm
	EXESUF		 = exe
	END_O		 =
	LEX_BIN		 = flex.exe
	YACC_BIN	 = bison.exe
	TAB_NAME	 = kapi_parser.tab
	LIBSUF		 = lib
	AR_OUT	 	 = /out:
	ifndef REL_DIR
		REL_DIR 	 = w:/$(WORKING)/x86sw/src/release/kapi/$(RELEASE)
	endif
	NAME_FLAG	 = /o
	LINK_OPTIONS = /MD libkapi.lib libkapi_ia64.lib

	ifeq ($(MACHINE_ARCH),IA64_NT)
		CC=ecl		
		AR=ilib /MACHINE:IA64 
		GCC_BIN=ecl
		CC_BIN=ecl
		LINK32_FLAGS+=-nodefaultlib -debug:notmapped,MINIMAL -debugtype:coff -machine:IA64 -base:0x10000000
		LINKER=ilink.exe
		EXTERNAL_LIBS=$(IA64SDK_REF)\\lib\\libcEM.lib
		BTRANS_FLAGS=-X -O3 -D_NTSDK                                           \
	          /MLd /W2 /D "WIN32" /D "_CONSOLE" /D "_MBCS"                                \
	          -D_IA64_=1 -DIA64=1 -D_GAMBIT_=1 -DNO_HW_DETECT -D_MSC_EXTENSIONS           \
	          -DSTD_CALL -DCONDITION_HANDLING=1   -DNT_INST=0 -DWIN32=100                 \
	          -D_NT1X_=100 -D_WIN32_WINNT=0x0500 -DWINVER=0x0500                          \
	          -D_WIN32_IE=0x0500 -DWIN32_LEAN_AND_MEAN=1 -DDEVL=1 -DFPO=1                 \
	          -D_IDWBUILD -DRDRDBG -DSRVDBG                                               

		USER_CFLAGS=$(BTRANS_FLAGS) -X /ML -Qapp_mode -DWINNT_APP -DU_64=__int64 -DYY_NEVER_INTERACTIVE=1 -I. -Iinclude -I$(IA64SDK_REF)\include \
		 -I$(BTRANS_TOOL_DIR)\\include.eas -I$(IA64SDK_REF)\include\sys -I$(IA64SDK_REF)\include\crt -U_MSC_VER \
		 -I$(INSTALLINC_SEARCH_PATH) /Fa 
		LOCALLIB=$(LOCALDIR)/lib.ia64
		LOCALBIN=$(LOCALDIR)/bin.ia64
		COPT = /c
		LINK_OPTIONS = /MD libkapi.lib libkapi_ia64.lib libcem.lib
	endif
else
	OBJSUF       = o
	ASMSUF       = s
	EXESUF		 =
	END_O		 = /opt/langtools/lib/end.o 
	MAKEDEPEND	 = /opt/imake/bin/makedepend
	LEX_BIN	     = lex
	YACC_BIN	 = yacc
	TAB_NAME	 = y.tab
	LIBSUF		 = a
	AR 			 = /usr/bin/ar
	AR_OUT		 = r 
	ifndef REL_DIR
		REL_DIR 	 = ~/p7/$(WORKING)/x86sw/src/release/kapi/$(RELEASE)
	endif
	NAME_FLAG	 = -o 
	LINK_OPTIONS = -l:kapi -l:kapi_ia64
	ifeq ($(OS),HPUX)
		CC=cc
	endif


	#CFLAGS += -I $(SERVER_AREA)/include
endif

ifdef SCHED
	CFLAGS += -DSCHED 
endif

CSUF         = .c
HSUF         = .h

GENFILES        = kapi_y.tab.c kapi_lex.yy.c kapi_parse.new.c \
                  y.output $(TAB_NAME).c $(TAB_NAME).h kapi_y.tab.h

OLD_KAPI_LINKOBJSr       = kapi_general kapi_generic kapi_ia64_bundle \
                  kapi_ia64_instruction kapi_ia64_ports kapi_ia64_latency \
                  $(MAIN_SRC) kapi_debug kapi_util kapi_y.tab kapi_lex.yy \
                  kapi_error kapi_symbol kapi_parse.new kapi_bv kapi_ia64 \
		  		  kapi_ia64_cache kapi_save_source_ia64 \
	  	  		  kapi_version kapi_ia64_version
OLD_KAPI_LINKOBJS        = $(addsuffix .$(OBJSUF),$(LINKOBJSr))

KAPI_LINKOBJSr = \
	kapi_bv \
	kapi_error \
	kapi_general \
	kapi_generic \
	kapi_lex.yy \
	kapi_parse.new \
	kapi_symbol \
	kapi_util \
	kapi_version \
	kapi_y.tab

SCHED_KAPI_LINKOBJSr = \
	kapi_bv \
	kapi_error \
	kapi_general \
	kapi_generic \
	kapi_symbol \
	kapi_util


KAPI_ia64_LINKOBJSr = \
	kapi_debug \
	kapi_ia64 \
	kapi_ia64_bundle \
	kapi_ia64_cache \
	kapi_ia64_instruction \
	kapi_ia64_latency \
	kapi_ia64_ports \
	kapi_ia64_version \
	kapi_save_source_ia64


SCHED_KAPI_ia64_LINKOBJSr = \
	kapi_ia64 \
	kapi_ia64_bundle \
	kapi_ia64_cache \
	kapi_ia64_instruction \
	kapi_ia64_latency \
	kapi_ia64_ports \
	kapi_ia64_version


KMAPI_LINKOBJSr = \
	kmapi_error \
	kmapi_map	\
	kmapi_split_issue	\
	kmapi_save_source	\
	kmapi_version

SCHED_KMAPI_LINKOBJSr = \
	kmapi_error \
	kmapi_map	\
	kmapi_split_issue	


ifndef WITH_STRINGS
SCHED_LINKOBJSr = \
    kapi_med_init \
    merced_no_strings
else
SCHED_LINKOBJSr = \
    kapi_med_init \
    merced_with_strings
endif
# merced_no_strings for secure version of schedlib.

ifdef SCHED_SPECIAL_OBJS
  KAPI_LINKOBJS = $(addsuffix .$(OBJSUF),$(SCHED_KAPI_LINKOBJSr))
  KAPI_ia64_LINKOBJS = $(addsuffix .$(OBJSUF),$(SCHED_KAPI_ia64_LINKOBJSr))
  KMAPI_LINKOBJS = $(addsuffix .$(OBJSUF),$(SCHED_KMAPI_LINKOBJSr))
else
  KAPI_LINKOBJS = $(addsuffix .$(OBJSUF),$(KAPI_LINKOBJSr))
  KAPI_ia64_LINKOBJS = $(addsuffix .$(OBJSUF),$(KAPI_ia64_LINKOBJSr))
  KMAPI_LINKOBJS = $(addsuffix .$(OBJSUF),$(KMAPI_LINKOBJSr))
endif  

SCHED_LINKOBJS = $(addsuffix .$(OBJSUF),$(SCHED_LINKOBJSr))
SCHED_OBJS = $(KAPI_LINKOBJS) $(KAPI_ia64_LINKOBJS) $(KMAPI_LINKOBJS) $(SCHED_LINKOBJS)

DIST_CFILES = \
	kapi_bv.c \
	kapi_error.c \
	kapi_fmain.c \
	kapi_general.c \
	kapi_generic.c \
	kapi_ia64.c \
	kapi_ia64_bundle.c \
	kapi_ia64_cache.c \
	kapi_ia64_instruction.c \
	kapi_ia64_latency.c \
	kapi_ia64_ports.c \
	kapi_ia64_version.c \
	kapi_lex.yy.c \
	kapi_main.c \
	kapi_parse.c \
	kapi_parse.new.c \
	kapi_parser.tab.c \
	kapi_symbol.c \
	kapi_util.c \
	kapi_version.c \
	kapi_y.tab.c \
	kmapi_error.c \
	kmapi_map.c \
	kmapi_split_issue.c \
	kmapi_version.c 

NON_DIST_CFILES = \
	btrans_tst.c \
	kmapi_save_source.c \
	kapi_save_source_ia64.c \
	kapi_debug.c \
	kapi_dumpbin.c 

DIST_HFILES = \
	kapi.h \
	kapi_bv.h \
	kapi_debug.h \
	kapi_error.h \
	kapi_ia64.h \
	kapi_internal.h \
	kapi_parse.h \
	kapi_parser.tab.h \
	kapi_symbol.h \
	kapi_util.h \
	kapi_y.tab.h \
	kmapi.h \
	kmapi_error.h \
	kmapi_internal.h \

NON_DIST_HFILES= \
	kapi_saver_utils.h \
	kapi_save_source_ia64.h \
	kapi_dumpbin.h \

DIST_MISCFILES = \
	Makefile \
	kapi_parser.y	\
	kapi_scanner.l	\
	rename.sed 

DISTSRC = $(DIST_CFILES) $(DIST_HFILES) $(DIST_MISCFILES) 
FULL_DISTSRC = $(DIST_CFILES) $(DIST_HFILES) $(DIST_MISCFILES) $(NON_DIST_CFILES) $(NON_DIST_HFILES)

ifndef RELEASE
	RELEASE = volatile
endif


ifeq ($(OPT_TYPE),OPTIMIZE)
	OPTIMIZE=1
else
	DEBUG=1
endif

ifdef OPTIMIZE
    NODEBUG=1
    DEBUG_FLAGS = -DNDEBUG
else
    DEBUG_FLAGS = 
endif

ifndef RELEASE
	RELEASE = volatile
endif


ifeq ($(OPT_TYPE),OPTIMIZE)
	OPTIMIZE=1
else
	DEBUG=1
endif

ifdef OPTIMIZE
    NODEBUG=1
    DEBUG_FLAGS = -DNDEBUG
else
    DEBUG_FLAGS = -D_DEBUG -g
endif



ifdef PROF
   LDFLAGS += -G
   DEBUG_FLAGS += -G
endif

VERSION_FLAGS=-D XVER_MINOR=$(XVERSION_MINOR) -D XVER_MAJOR=$(XVERSION_MAJOR)

CFLAGS+= -MD $(DEBUG_FLAGS) $(VERSION_FLAGS)

#
#  Tools versions/paths
#


# EAS_REL = eas2.4

.SUFFIXES: $(ASMSUF) $(OBJSUF)

help:
	@echo Options:
	@echo -lib_sched_kapi.lib
	@echo -libkapi.$(LIBSUF)- for kapi layer 1.
	@echo -libkapi_ia64.$(LIBSUF)- for kapi layer 2.
	@echo -libkmapi.$(LIBSUF)- for kapi layer 3.
	@echo -old_libkapi.$(LIBSUF)- for combined library 
	@echo 	will not be supported in future releases.
	@echo -ksaver.$(EXESUF)- for saver tool.
	@echo -release-, will create all needed files in 
	@echo	$(REL_DIR) 
	@echo	can be defined using environment variable REL_DIR.
	@echo -clean- for clean.


lib_sched_kapi.$(LIBSUF) : $(SCHED_OBJS)
	rm -f lib_sched_kapi.$(LIBSUF)
	$(AR) $(AR_OUT)lib_sched_kapi.lib $(SCHED_OBJS)

old_libkapi.$(LIBSUF): $(LINKOBJS)
	rm -f libkapi.$(LIBSUF)
	$(AR) $(AR_OUT)libkapi.$(LIBSUF) $(LINKOBJS)

libkapi.$(LIBSUF): $(KAPI_LINKOBJS)
	rm -f libkapi.$(LIBSUF)
	$(AR) $(AR_OUT)libkapi.$(LIBSUF) $(KAPI_LINKOBJS)

libkapi_ia64.$(LIBSUF): libkapi.$(LIBSUF) $(KAPI_ia64_LINKOBJS)
	rm -f libkapi_ia64.$(LIBSUF)
	$(AR) $(AR_OUT)libkapi_ia64.$(LIBSUF) $(KAPI_ia64_LINKOBJS)

libkmapi.$(LIBSUF): libkapi_ia64.$(LIBSUF) $(KMAPI_LINKOBJS)
	rm -f libkmapi.$(LIBSUF)
	$(AR) $(AR_OUT)libkmapi.$(LIBSUF) $(KMAPI_LINKOBJS)

sched: $(SCHED_OBJS)
	cp $(SCHED_OBJS) $(SCHED_OBJ_DIR)

echo_SCHED_OBJS:
	+@echo KAPI_SCHED_OBJS = "$(SCHED_OBJS)"

ksaver.$(EXESUF): ksaver.c libkapi.$(LIBSUF) libkapi_ia64.$(LIBSUF)
	$(CC) $(LINK_OPTIONS) ksaver.c

btst.$(EXESUF): btrans_tst.c merced.c libkapi.$(LIBSUF) libkapi_ia64.$(LIBSUF)
	$(CC) $(LINK_OPTIONS) btrans_tst.c merced.c

kapi_y.tab.c: kapi_parser.y
	$(YACC_BIN) -dv kapi_parser.y
	cat $(TAB_NAME).c | sed -f rename.sed > kapi_y.tab.c

kapi_y.tab.h: kapi_parser.y kapi_y.tab.c
	cat $(TAB_NAME).h | sed -f rename.sed > kapi_y.tab.h

kapi_y.tab.$(OBJSUF): kapi_y.tab.c

kapi_parse.new.$(OBJSUF): kapi_parse.c
	cat kapi_parse.c | sed -f rename.sed > kapi_parse.new.c
	$(CC) $(CFLAGS) kapi_parse.new.c 

kapi_lex.yy.c: kapi_scanner.l kapi_y.tab.h
	$(LEX_BIN) -t -n kapi_scanner.l | sed -f rename.sed > kapi_lex.yy.c

.c.$(ASMSUF):
	$(CC) -c -S $(CFLAGS) $<



release:  libkapi.$(LIBSUF) libkapi_ia64.$(LIBSUF) ksaver.$(EXESUF) libkmapi.$(LIBSUF)
	tar cvf src.tar $(DISTSRC)
	rm -f $(REL_DIR)/src.tar
	cp src.tar $(REL_DIR)
	chmod 600 $(REL_DIR)/src.tar
	rm -f $(REL_DIR)/libkapi.$(LIBSUF)
	cp libkapi.$(LIBSUF) $(REL_DIR)
	rm -f $(REL_DIR)/kapi_bv.h $(REL_DIR)/kapi.h $(REL_DIR)/kapi_ia64.h $(REL_DIR)/kapi_internal.h
	cp kapi_ia64.h kapi_bv.h kapi.h kapi_internal.h $(REL_DIR)
	cp kapi.doc $(REL_DIR)
	cp ksaver.c ksaver.$(EXESUF) $(REL_DIR)

full_release: libkapi.$(LIBSUF) libkapi_ia64.$(LIBSUF) libkmapi.$(LIBSUF)
	tar zcvf all_src.tgz $(FULL_DISTSRC)
	rm -f $(REL_DIR)/all_src.tgz
	cp all_src.tgz $(REL_DIR)
	rm -f $(REL_DIR)/libkapi.$(LIBSUF)
	cp libkapi.$(LIBSUF) $(REL_DIR)
	cp libkapi_ia64.$(LIBSUF) $(REL_DIR)
	cp libkmapi.$(LIBSUF) $(REL_DIR)
	rm -f $(REL_DIR)/kapi_bv.h $(REL_DIR)/kapi.h $(REL_DIR)/kapi_ia64.h $(REL_DIR)/kmapi.h
	cp kapi_ia64.h kapi_bv.h kapi.h kmapi.h $(REL_DIR)


depend:
	$(MAKEDEPEND) -o".o" $(INCLUDE_PATH) *.c

P:
	make PURIFY=1

clean: 
	rm -f $(KAPI_LINKOBJS) $(KAPI_ia64_LINKOBJS) $(BINARY) $(GENFILES) libkapi.$(LIBSUF) libkapi_ia64.$(LIBSUF) src.tar *.d

wc:
	wc.exe $(FULL_DISTSRC)


# DO NOT DELETE THIS LINE -- make depend depends on it.

kapi_bv.o: kapi_bv.h
kapi_debug.o: kapi_internal.h kapi.h kapi_bv.h
kapi_debug.o: kapi_ia64.h kapi_parse.h kapi_debug.h
kapi_error.o: kapi_error.h
kapi_general.o: kapi_internal.h kapi.h kapi_bv.h kapi_ia64.h kapi_util.h
kapi_general.o: kapi_parse.h kapi_error.h
kapi_generic.o: kapi_internal.h kapi.h kapi_bv.h 
kapi_generic.o: kapi_ia64.h kapi_parse.h kapi_util.h
kapi_ia64.o: kapi_internal.h kapi.h kapi_bv.h
kapi_ia64.o: kapi_ia64.h kapi_util.h kapi_error.h
kapi_ia64_bundle.o: kapi_internal.h kapi.h kapi_bv.h
kapi_ia64_bundle.o: kapi_ia64.h
kapi_ia64_instruction.o: kapi_internal.h kapi.h
kapi_ia64_instruction.o: kapi_bv.h kapi_ia64.h
kapi_ia64_instruction.o: kapi_util.h kapi_error.h
kapi_ia64_latency.o: kapi_internal.h kapi.h kapi_bv.h
kapi_ia64_latency.o: kapi_ia64.h kapi_error.h
kapi_ia64_ports.o: kapi_internal.h kapi.h kapi_bv.h
kapi_ia64_ports.o: kapi_ia64.h kapi_util.h kapi_error.h
kapi_lex.yy.o: kapi_internal.h kapi.h kapi_bv.h
kapi_lex.yy.o: kapi_ia64.h kapi_parse.h kapi_error.h kapi_util.h kapi_y.tab.h
kapi_parse.o: kapi_parse.h
kapi_parse.o: kapi_bv.h kapi_ia64.h kapi.h kapi_internal.h kapi_error.h
kapi_parse.o: kapi_util.h
kapi_parse.new.o: kapi_parse.h kapi_bv.h kapi_ia64.h kapi.h kapi_internal.h
kapi_parse.new.o: kapi_error.h kapi_util.h
kapi_symbol.o: kapi_parse.h
kapi_symbol.o: kapi_bv.h kapi_ia64.h kapi.h kapi_internal.h kapi_util.h
kapi_util.o: kapi_internal.h kapi.h kapi_bv.h
kapi_util.o: kapi_ia64.h
kapi_y.tab.o: kapi_internal.h
kapi_y.tab.o: kapi.h kapi_bv.h kapi_ia64.h kapi_util.h kapi_error.h
kapi_y.tab.o: kapi_parse.h
y.tab.o: kapi_internal.h kapi.h
y.tab.o: kapi_bv.h kapi_ia64.h kapi_util.h kapi_error.h kapi_parse.h
