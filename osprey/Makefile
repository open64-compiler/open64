# Copyright 2005 PathScale, Inc.  All Rights Reserved.
#
# This file is free software; you can redistribute it and/or modify it
# under the terms of version 2 of the GNU General Public License, as
# published by the Free Software Foundation.
#
# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this file; see the file COPYING.  If not, write to the
# Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

export OPEN64_MAJOR_VERSION := 0
export OPEN64_MINOR_VERSION := 0
export OPEN64_PATCH_LEVEL := 0
export OPEN64_EXTRA_VERSION := -custom
export OPEN64_FULL_VERSION := $(OPEN64_MAJOR_VERSION).$(OPEN64_MINOR_VERSION).$(OPEN64_PATCH_LEVEL)$(OPEN64_EXTRA_VERSION)
export OPEN64_FULL_VERSION := 3.0
export OPEN64_GCC_VERSION := 3.3.1
export OPEN64_GCC40_VERSION := 4.0.2
export OPEN64_GCC42_VERSION := 4.2.0
export OPEN64_ARCH_64 := x86_64
export OPEN64_ARCH_32 := i386
export OPEN64_ARCH := $(OPEN64_ARCH_64)
export OPEN64_TARGET_64 := $(OPEN64_ARCH_64)-open64-linux
export OPEN64_TARGET_32 := $(OPEN64_ARCH_32)-open64-linux
export OPEN64_TARGET := $(OPEN64_TARGET_64)
export OPEN64_ROOT_PREFIX := /opt/open64
export OPEN64_PREFIX := $(OPEN64_ROOT_PREFIX)/$(OPEN64_TARGET)
export OPEN64_INSTALL_PREFIX := $(OPEN64_PREFIX)
export OPEN64_NAME_PREFIX := open
export OPEN64_INCLUDE_DIR := $(OPEN64_INSTALL_PREFIX)/lib/gcc-lib/$(OPEN64_TARGET)/open64-$(OPEN64_FULL_VERSION)/include/
export OPEN64_LIB_DIR := $(OPEN64_ROOT_PREFIX)/lib/$(OPEN64_FULL_VERSION)

top_dir := .
compiler_dir := $(top_dir)/targia32_x8664
cygnus_dir := $(top_dir)/targcygnus_ia32_x8664
lib32_dir := $(top_dir)/targia32_builtonia32
lib64_dir := $(top_dir)/targx8664_builtonia32

all: compiler messages cygnus libs

compiler:
	$(MAKE) -C $(compiler_dir)
	$(MAKE) -C $(compiler_dir)/crayf90/sgi mfef95

messages: compiler
	$(MAKE) -C $(compiler_dir)/crayf90/sgi cf95.cat
#	$(MAKE) -C $(compiler_dir)/arith arith.cat
	$(MAKE) -C $(compiler_dir)/libu lib.cat

cygnus:
	cd $(cygnus_dir) && ./CONFIGURE
	$(MAKE) -C $(cygnus_dir)

libs: lib32 lib64

lib32:
	$(MAKE) -C $(lib32_dir)

lib64:
	$(MAKE) -C $(lib64_dir)

clean: compiler-clean cygnus-clean libs-clean
	find $(compiler_dir) $(cygnus_dir) $(lib32_dir) $(lib64_dir) \
	  -name Makedepend -print0 | xargs -r0 rm -f

compiler-clean:
	$(MAKE) -C $(compiler_dir) $@

cygnus-clean:
	-[ -f $(cygnus_dir)/Makefile ] && $(MAKE) -C $(cygnus_dir) $@

libs-clean: lib32-clean lib64-clean

lib32-clean:
	$(MAKE) -C $(lib32_dir) $@

lib64-clean:
	$(MAKE) -C $(lib64_dir) $@
