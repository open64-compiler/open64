#  Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
#
#  This program is free software; you can redistribute it and/or modify it
#  under the terms of version 2 of the GNU General Public License as
#  published by the Free Software Foundation.
#
#  This program is distributed in the hope that it would be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
#  Further, this software is distributed without any warranty that it is
#  free of the rightful claim of any third person regarding infringement
#  or the like.  Any license provided herein, whether implied or
#  otherwise, applies only to this software file.  Patent licenses, if
#  any, provided herein do not apply to combinations of this program with
#  other software, or any other product whatsoever.
#
#  You should have received a copy of the GNU General Public License along
#  with this program; if not, write the Free Software Foundation, Inc., 59
#  Temple Place - Suite 330, Boston MA 02111-1307, USA.
#

SOURCES = gspin-tree.c \
  gspin-assert.c \
  gspin-alloc.c \
  gspin-mempool.c \
  gspin-io.c \
  gspin-list.c \
  gspin-tel.c

HEADERS = gspin-tree.h \
  gspin-assert.h \
  gspin-base-types.h \
  gspin-alloc.h \
  gspin-mempool.h \
  gspin-io.h \
  gspin-list.h \
  gspin-tel.h

ifeq ($(FE),GNU_4_2_0)
  FE_VERSION=-DFE_GNU_4_2_0
  NAME_SUFFIX=42
else
  FE_VERSION=
  NAME_SUFFIX=
endif

OBJECTS = $(SOURCES:.c=$(NAME_SUFFIX).o)

CC = gcc
LIBSPIN = libgspin$(NAME_SUFFIX).a
GSPIN = gspin$(NAME_SUFFIX)

ifeq ($(BUILD_ARCH), MIPS64)
  CFLAGS = -mabi=n32 -O3 -ffloat-store -DARCH_MIPS
endif

ifeq ($(BUILD_ARCH), IA32)
CFLAGS += -m32
endif

ifeq ($(BUILD_ARCH), X8664)
CFLAGS += -m64
endif

ifeq ($(BUILD_TARGET), X8664)
CFLAGS += -DTARG_X8664
endif

ifeq ($(BUILD_TARGET), IA64)
CFLAGS += -DTARG_IA64
endif

all: $(LIBSPIN) $(GSPIN)

devbuild: CFLAGS = -DIs_True_On -g -Wall
devbuild: $(LIBSPIN) $(GSPIN)

$(OBJECTS): $(HEADERS)

gspin-test-driver$(NAME_SUFFIX).o: $(HEADERS)

%$(NAME_SUFFIX).o : %.c
	$(CC) $(CFLAGS) $(FE_VERSION) -Wformat -Wswitch -c -Werror-implicit-function-declaration $< -o $@

$(LIBSPIN): $(OBJECTS)
	$(AR) rc $@ $(OBJECTS)

$(GSPIN): gspin-test-driver$(NAME_SUFFIX).o $(LIBSPIN)
	$(CC) $(CFLAGS) $(FE_VERSION) gspin-test-driver.c $(LIBSPIN) -o $@

clean:
	/bin/rm -f $(OBJECTS) gspin-test-driver$(NAME_SUFFIX).o $(LIBSPIN) $(GSPIN)
