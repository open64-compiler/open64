# Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
#
# Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of version 2 of the GNU General Public License as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it would be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
#
# Further, this software is distributed without any warranty that it is
# free of the rightful claim of any third person regarding infringement 
# or the like.  Any license provided herein, whether implied or 
# otherwise, applies only to this software file.  Patent licenses, if 
# any, provided herein do not apply to combinations of this program with 
# other software, or any other product whatsoever.  
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write the Free Software Foundation, Inc., 59
# Temple Place - Suite 330, Boston MA 02111-1307, USA.

hg_root := $(shell hg root 2>/dev/null || echo unknown)
ifeq ($(BUILD_OS), LINUX)
hg_hostname := $(shell hostname -f)
else
# -f not accepted on cygwin
hg_hostname := $(shell hostname)
endif

version-hg.c: version.c
	@echo 'GEN    $@'
	@echo '#define HG_CSET_ID "$(shell hg parents --template "{node}")"' > $@
	@sed	-e 's/^\(.*version_string.*= "[0-9. ]*\).*/\1 (PathScale " HG_CSET_ID ")";/' \
		-e 's!http://bugzilla.redhat.com/bugzilla!http://www.pathscale.com/support!' $< >> $@
	@echo >> $@
	@echo 'const char *const cset_id = HG_CSET_ID;' >> $@
	@echo 'const char *const build_root = "$(hg_root)";' >> $@
	@echo 'const char *const build_host = "$(hg_hostname)";' >> $@
	@echo 'const char *const build_user = "$(shell id -un)";' >> $@
	@echo 'const char *const build_date = "$(shell date +'%Y-%m-%d %H:%M:%S %z')";' >> $@

.PHONY: version-hg.c

