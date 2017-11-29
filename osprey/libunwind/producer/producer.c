/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/



#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <sys/unwindP.h>
#include "unwind_producer.h"



/* unwind table space */
__unw_table_entry_t *_unwind_table = NULL;
__uint64_t _unwind_table_total_size = 0L;
__uint64_t _unwind_table_size = 0L;
/* current unwind table entry space */
__unw_table_entry_t _current_unwind_table_entry = { 0L, 0L, 0L };

/* unwind info space */
__unw_info_t *_unwind_info = NULL;
__uint64_t _unwind_info_total_size = 0L;
__uint64_t _unwind_info_size = 0L;
/* current unwind info space */
__unw_info_t *_current_unwind_info = NULL;
__uint64_t _current_unwind_info_total_size = 0L;
__uint64_t _current_unwind_info_size = 0L;

/* current procedure and current frame info */
__uint64_t _current_procedure_size = 0L;
__uint64_t _current_procedure_total_size = 0L;
__uint64_t _current_region_total_size = 0L;
__uint32_t _current_region_id = __UNW_UNDEF;

/* uhandler and ehandler flags */
__uint32_t _ehandler = 0;
__uint32_t _uhandler = 0;

/* personality routine */
__unw_addr_t _personality = 0L;

/* language-specific data */
void *_lang_spec_data = NULL;
__uint64_t _lang_spec_data_size = 0L;
__uint64_t _lang_spec_data_total_size = 0L;

/* imask data */
void *_imask = NULL;
__uint64_t _imask_size = 0L;
__uint64_t _imask_total_size = 0L;

/* static var to hold an array of zeros (to be used for padding) */
static const char _null_array[] = {
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };



/* function to add a descriptor to the current unwind info */
__unw_error_t unwind_info_add_desc(__uint64_t size, char *buf) {

#ifdef DEBUG
	__uint64_t i;
	fprintf(stderr, "unwind_info_add_desc(): size=%llu \"",
		(unsigned long long)size);
	for (i = 0; i < size; i++) {
		fprintf(stderr, "%02x", (unsigned char)buf[i]);
	}
	fprintf(stderr, "\"\n");
#endif
	/* update current unwind info */
	_current_unwind_info_size += size;
	while (_current_unwind_info_size >= _current_unwind_info_total_size) {
		_current_unwind_info_total_size *= 2;
		if (NULL == (_current_unwind_info =
				(__unw_info_t *)realloc((void *)_current_unwind_info,
				(size_t)_current_unwind_info_total_size))) {
			unwind_cleanup();
			return __UNW_REALLOC_ERROR;
		}
#ifdef DEBUG
		fprintf(stderr, "unwind_info_add_desc(): ");
		fprintf(stderr, "reallocated _current_unwind_info @ 0x%llx, ",
			(unsigned long long)_current_unwind_info);
		fprintf(stderr, "total_size=%llu\n",
			(unsigned long long)_current_unwind_info_total_size);
#endif
	}
	bcopy((const void *)buf, (void *)((char *)_current_unwind_info +
		_current_unwind_info_size - size), (size_t)size);
#ifdef DEBUG
	fprintf(stderr, "unwind_info_add_desc(): ");
	fprintf(stderr, "updated _current_unwind_info @ 0x%llx, ",
		(unsigned long long)_current_unwind_info);
	fprintf(stderr, "size=%llu+%u, total_size=%llu\n",
		(unsigned long long)_current_unwind_info_size -
			(unsigned long long)size,
		size,
		(unsigned long long)_current_unwind_info_total_size);
#endif

	return __UNW_OK;
}



/* producer function to initialize the unwind info */
__unw_error_t unwind_info_initialize(__unw_info_t **info,
					__unw_addr_t start, __unw_addr_t end) {

	/* set up initial return argument value */
	*info = NULL;

	/* allocate unwind table */
	if (NULL == _unwind_table) {
		_unwind_table_total_size = __UNW_TABLE_ENTRIES_SIZE;
		if (NULL == (_unwind_table =
				(__unw_table_entry_t *)malloc((size_t)
				(_unwind_table_total_size *
				sizeof(__unw_table_entry_t))))) {
			unwind_cleanup(); 
			return __UNW_MALLOC_ERROR;
		}
#ifdef DEBUG
		fprintf(stderr, "unwind_info_initialize(): ");
		fprintf(stderr, "allocated _unwind_table @ 0x%llx, ",
			(unsigned long long)_unwind_table);
		fprintf(stderr, "total_size=%llu*%u\n",
			(unsigned long long)_unwind_table_total_size,
			sizeof(__unw_table_entry_t));
#endif
	}

	/* allocate unwind info */
	if (NULL == _unwind_info) {
		_unwind_info_total_size = __UNW_INFO_SIZE;
		if (NULL == (_unwind_info =
				(__unw_info_t *)malloc((size_t)
				_unwind_info_total_size))) {
			unwind_cleanup(); 
			return __UNW_MALLOC_ERROR;
		}
#ifdef DEBUG
		fprintf(stderr, "unwind_info_initialize(): ");
		fprintf(stderr, "allocated _unwind_info @ 0x%llx, total_size=%llu\n",
			(unsigned long long)_unwind_info,
			(unsigned long long)_unwind_info_total_size);
#endif
	}

	/* allocate current unwind info */
	if (NULL == _current_unwind_info) {
		_current_unwind_info_total_size = __UNW_CURRENT_INFO_SIZE;
		if (NULL == (_current_unwind_info =
				(__unw_info_t *)malloc((size_t)
				_current_unwind_info_total_size))) {
			unwind_cleanup(); 
			return __UNW_MALLOC_ERROR;
		}
#ifdef DEBUG
		fprintf(stderr, "unwind_info_initialize(): ");
		fprintf(stderr, "allocated _current_unwind_info @ 0x%llx, ",
			(unsigned long long)_current_unwind_info);
		fprintf(stderr, "total_size=%llu\n",
			(unsigned long long)_current_unwind_info_total_size);
#endif
	}

	/* set up current unwind table entry */
	_current_unwind_table_entry._start = start;
	_current_unwind_table_entry._end = end;
	_current_unwind_table_entry._info = (__unw_addr_t)_unwind_info_size;

	/* clean up current unwind info */
	_current_unwind_info->_header = 0x0000000000000000L;
	_current_unwind_info_size = sizeof(__unw_dbl_word_t);
	
	/* set up and clean up current procedure/region sizes */
	_current_procedure_size = 0L;
	_current_procedure_total_size = (__uint64_t)(start - end) * 3;
	_current_region_total_size = 0L;

	/* clean ehandler/uhandler flags and personality routine */
	_ehandler = 0;
	_uhandler = 0;
	_personality = 0L;

	/* clean language-specific data size */
	_lang_spec_data_size = 0L;

	/* set up real return argument value */
	*info = _unwind_info + _unwind_info_size;

	return __UNW_OK;
}



/* producer function to finalize the unwind info */
/* and add its entry to the unwind table */
__unw_error_t unwind_info_finalize(__unw_info_t *info) {
	__uint64_t real_size = 0L;
	__unw_error_t ret;

	/* check valid info argument */
	if (NULL == info) {
		return __UNW_NULL_ERROR;
	} else if (_unwind_info + _unwind_info_size != info) {
		return __UNW_INV_ARG_ERROR;
	}

	/* don't forget to add the imask if this is a prologue and there's an imask */
	/* (imask gets set in overall.c) */
	if ((__UNW_PROLOGUE == _current_region_id) && (0L != _imask_size)) {
		unwind_info_add_imask(info);
	}

	/* check current unwind info size */
	if (0L == _current_unwind_info_size) {
#ifdef DEBUG
		fprintf(stderr, "unwind_info_finalize(): ");
		fprintf(stderr, "_current_unwind_info @ 0x%llx has 0 size\n",
			(unsigned long long)_current_unwind_info);
#endif
		return __UNW_OK;
	}

	/* update unwind table */
	if (_unwind_table_size + 1 >= _unwind_table_total_size) {
		_unwind_table_total_size *= 2;
		if (NULL == (_unwind_table =
				(__unw_table_entry_t *)realloc((void *)_unwind_table,
				(size_t)(_unwind_table_total_size *
				sizeof(__unw_table_entry_t))))) {
			unwind_cleanup(); 
			return __UNW_REALLOC_ERROR;
		}
#ifdef DEBUG
		fprintf(stderr, "unwind_info_finalize(): ");
		fprintf(stderr, "reallocated _unwind_table @ 0x%llx, ",
			(unsigned long long)_unwind_table);
		fprintf(stderr, "total_size=%llu*%u\n",
			(unsigned long long)_unwind_table_total_size,
			sizeof(__unw_table_entry_t));
#endif
	}
	_unwind_table[_unwind_table_size++] = _current_unwind_table_entry;
#ifdef DEBUG
	fprintf(stderr, "unwind_info_finalize(): ");
	fprintf(stderr, "updated _unwind_table @ 0x%llx, ",
		(unsigned long long)_unwind_table);
	fprintf(stderr, "size=(%llu+1)*%u, total_size=%llu*%u\n",
		(unsigned long long)_unwind_table_size-1,
		sizeof(__unw_table_entry_t),
		(unsigned long long)_unwind_table_total_size,
		sizeof(__unw_table_entry_t));
#endif

	/* set up real size */
	real_size = (_current_unwind_info_size + sizeof(__unw_dbl_word_t) - 1) /
							sizeof(__unw_dbl_word_t);

	/* set up current unwind info header */
	_current_unwind_info->_header = __UNW_VERSION;
	_current_unwind_info->_header <<= 48;
	if (_ehandler) {
		_current_unwind_info->_header |= 0x0000000100000000LL;
	}
	if (_uhandler) {
		_current_unwind_info->_header |= 0x0000000200000000LL;
	}
	_current_unwind_info->_header |= (__uint32_t)(real_size - 1);

	/* padding */
	real_size *= sizeof(__unw_dbl_word_t);
	if (real_size != _current_unwind_info_size) {
		unwind_info_add_desc((__uint64_t)(real_size -
				_current_unwind_info_size), (char *)_null_array);
	}

	/* set up personality routine */
	/* after the size has been set up in the header */
	if (_personality) {
		ret = unwind_info_add_desc((__uint64_t)sizeof(__unw_addr_t),
							(char *)&_personality);

		/* padding */
		real_size = (sizeof(__unw_addr_t) + sizeof(__unw_dbl_word_t) - 1) /
							sizeof(__unw_dbl_word_t);
		real_size *= sizeof(__unw_dbl_word_t);
		if (real_size != sizeof(__unw_addr_t)) {
			unwind_info_add_desc((__uint64_t)(real_size -
				sizeof(__unw_addr_t)), (char *)_null_array);
		}

		/* check return value */
		if (__UNW_OK != ret) {
			return ret;
		}
	}

	/* set up language-specific data */
	/* after the size has been set up in the header */
	if (_lang_spec_data_size) {
		ret = unwind_info_add_desc(_lang_spec_data_size,
							(char *)_lang_spec_data);

		/* padding */
		real_size = (_lang_spec_data_size + sizeof(__unw_dbl_word_t) - 1) /
							sizeof(__unw_dbl_word_t);
		real_size *= sizeof(__unw_dbl_word_t);
		if (real_size != _lang_spec_data_size) {
			unwind_info_add_desc((__uint64_t)(real_size -
				_lang_spec_data_size), (char *)_null_array);
		}

		/* check return value */
		if (__UNW_OK != ret) {
			return ret;
		}
	}

	/* update unwind info */
	if (_current_unwind_info_size % sizeof(__unw_dbl_word_t) != 0) {
		return __UNW_INV_ALIGNMENT_ERROR;
	}
	_unwind_info_size += _current_unwind_info_size;
	while (_unwind_info_size >= _unwind_info_total_size) {
		_unwind_info_total_size *= 2;
		if (NULL == (_unwind_info =
				(__unw_info_t *)realloc((void *)_unwind_info,
				(size_t)_unwind_info_total_size))) {
			unwind_cleanup();
			return __UNW_REALLOC_ERROR;
		}
#ifdef DEBUG
		fprintf(stderr, "unwind_info_finalize(): ");
		fprintf(stderr, "reallocated _unwind_info @ 0x%llx, ",
			(unsigned long long)_unwind_info);
		fprintf(stderr, "total_size=%llu\n",
			(unsigned long long)_unwind_info_total_size);
#endif
	}
	bcopy((const void *)_current_unwind_info,
		(void *)((char *)_unwind_info + _unwind_info_size - _current_unwind_info_size),
		(size_t)_current_unwind_info_size);
#ifdef DEBUG
	fprintf(stderr, "unwind_info_finalize(): ");
	fprintf(stderr, "updated _unwind_info @ 0x%llx, ",
		(unsigned long long)_unwind_info);
	fprintf(stderr, "size=%llu+%llu total_size=%llu\n",
		(unsigned long long)_unwind_info_size -
			(unsigned long long)_current_unwind_info_size,
		(unsigned long long)_current_unwind_info_size,
		(unsigned long long)_unwind_info_total_size);
#endif

	return __UNW_OK;
}



/* producer function to clean up the unwind data structures */
__unw_error_t unwind_cleanup(void) {

	/* deallocate unwind table space */
	if (_unwind_table) {
		free(_unwind_table);
#ifdef DEBUG
		fprintf(stderr, "unwind_cleanup(): ");
		fprintf(stderr, "deallocated _unwind_table @ 0x%llx\n",
			(unsigned long long)_unwind_table);
#endif
	}

	/* deallocate unwind info space */
	if (_unwind_info) {
		free(_unwind_info);
#ifdef DEBUG
		fprintf(stderr, "unwind_cleanup(): ");
		fprintf(stderr, "deallocated _unwind_info @ 0x%llx\n",
			(unsigned long long)_unwind_info);
#endif
	}

	/* deallocate current unwind info space */
	if (_current_unwind_info) {
		free(_current_unwind_info);
#ifdef DEBUG
		fprintf(stderr, "unwind_cleanup(): ");
		fprintf(stderr, "deallocated _current_unwind_info @ 0x%llx\n",
			(unsigned long long)_current_unwind_info);
#endif
	}

	/* deallocate language-specific data space */
	if (_lang_spec_data) {
		free(_lang_spec_data);
#ifdef DEBUG
		fprintf(stderr, "unwind_cleanup(): ");
		fprintf(stderr, "deallocated _lang_spec_data @ 0x%llx\n",
			(unsigned long long)_lang_spec_data);
#endif
	}

	/* deallocate imask space */
	if (_imask) {
		free(_imask);
#ifdef DEBUG
		fprintf(stderr, "unwind_cleanup(): ");
		fprintf(stderr, "deallocated _imask @ 0x%llx\n",
			(unsigned long long)_imask);
#endif
	}

	return __UNW_OK;
}



/* producer function to add a personality routine */
__unw_error_t unwind_info_add_personality_routine_info(__unw_info_t *info,
		__unw_addr_t personality, __uint32_t ehandler, __uint32_t uhandler) {

        /* check valid info argument */
        if (NULL == info) {
                return __UNW_NULL_ERROR;
        } else if (_unwind_info + _unwind_info_size != info) {
                return __UNW_INV_ARG_ERROR;
        }

	/* set personality */
	_personality = personality;

	/* set flags */
	_ehandler = ehandler;
	_uhandler = uhandler;

	return __UNW_OK;
}



/* producer function to add language-specific data */
__unw_error_t unwind_info_add_language_specific_info(__unw_info_t *info,
			void *ptr, __uint64_t size) {

        /* check valid info argument */
        if (NULL == info) {
                return __UNW_NULL_ERROR;
        } else if (_unwind_info + _unwind_info_size != info) {
                return __UNW_INV_ARG_ERROR;
        }

	/* allocate language-specific data space */
	if (NULL == _lang_spec_data) {
		_lang_spec_data_total_size = __UNW_LANG_SPEC_SIZE;
		if (NULL == (_lang_spec_data =
				(void *)malloc((size_t)
				_lang_spec_data_total_size))) {
			unwind_cleanup(); 
			return __UNW_MALLOC_ERROR;
		}
#ifdef DEBUG
		fprintf(stderr, "unwind_info_add_language_specific_info(): ");
		fprintf(stderr, "allocated _lang_spec_data @ 0x%llx, ",
			(unsigned long long)_lang_spec_data);
		fprintf(stderr, "total_size=%llu\n",
			(unsigned long long)_lang_spec_data_total_size);
#endif
	}

	/* update language-specific data space */
	_lang_spec_data_size += size;
	while (_lang_spec_data_size >= _lang_spec_data_total_size) {
		_lang_spec_data_total_size *= 2;
		if (NULL == (_lang_spec_data =
				(void *)realloc((void *)_lang_spec_data,
				(size_t)_lang_spec_data_total_size))) {
			unwind_cleanup();
			return __UNW_REALLOC_ERROR;
		}
#ifdef DEBUG
		fprintf(stderr, "unwind_info_add_language_specific_info(): ");
		fprintf(stderr, "reallocated _lang_spec_data @ 0x%llx, ",
			(unsigned long long)_lang_spec_data);
		fprintf(stderr, "total_size=%llu\n",
			(unsigned long long)_lang_spec_data_total_size);
#endif
	}
	bcopy((const void *)ptr, (void *)((char *)_lang_spec_data +
		_lang_spec_data_size - size), (size_t)size);

	return __UNW_OK;
}
