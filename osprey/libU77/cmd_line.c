/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/* Code to implement F2003 intrinsics related to command line */

#include <string.h>
#include <alloca.h>
#include <stdlib.h>

#include "pathf90_libU_intrin.h"
#include "externals.h"

#define C_IFY(name, name_len, notrim) \
	c_ify(alloca((name_len) + 1), (name), (name_len), (notrim))
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

/*
 * Convert Fortran-style string value into C-style null-terminated value
 * temp		Temporary allocated by caller
 * value	Fortran-style string value to copy into temporary
 * value_len	Length of value
 * notrim	If true, don't trim trailing blanks
 * return	temp
 */
static char *
c_ify(char *temp, char *value, int value_len, int notrim) {
  int i = value_len;
  if (!notrim) {
    for (i = value_len; i > 0 && ' ' == value[i - 1]; i -= 1)
      ;
    }
  i ? memcpy(temp, value, i) : 0;
  temp[i] = '\0';
  return temp;
  }

/*
 * Copy C-style null-terminated string to Fortran-style blank-padded string
 * dest		Fortran-style blank-padded string
 * dest_len	length of dest
 * src		C-style null terminated string
 * src_len	length of src
 * return	dest
 */
static char *
f_ify(char *dest, int dest_len, char *src, int src_len) {
  int copy_len = MIN(src_len, dest_len);
  memcpy(dest, src, copy_len);
  int pad_len = MAX(0, dest_len - copy_len);
  if (pad_len) {
    memset(dest + copy_len, ' ', pad_len);
    }
  return dest;
  }

/*
 * Help intrinsics set their value, length, and status arguments
 * error	true if there's an error
 * dest		Fortran-style value to be set
 * src		C-style value to use in setting dest
 * length	length argument to be set
 * status	status argument to be set
 * dest_len	length of dest
 */
static void
helper(int error, char *dest, char *src, pathf90_i4 *length, pathf90_i4 *status,
  int dest_len) {
  if (error) {
    dest ?  memset(dest, ' ', dest_len) : 0;
    length ? (*length = 0) : 0;
    status ? (*status = 1) : 0;
    return;
    }
  int src_len = strlen(src);
  status ? (*status = (-(src_len > dest_len))) : 0;
  dest ? f_ify(dest, dest_len, src, src_len) : 0;
  length ? (*length = src_len) : 0;
  }

/* Implement F2003 intrinsic "command_argument_count" */
pathf90_i4
_Command_argument_count() {
  return ARGC - 1;
  }

/* Implement F2003 intrinsic "get_command" */
void
_Get_command(char *command, pathf90_i4 *length, pathf90_i4 *status,
  pathf90_i4 command_len) {
  unsigned c_value_len = 0;
  int i;
  for (i = 0; i < ARGC; i += 1) {
    c_value_len += strlen(ARGV[i]) + (i > 0);
    }
  status ? (*status = (command ? (-(c_value_len > command_len)): 0)) : 0;
  length ? (*length = c_value_len) : 0;
  if (command) {
    char *tmp = command;
    int tmp_len = command_len;
    int i;
    for (i = 0; i < ARGC; i += 1) {
      int src_len = strlen(ARGV[i]);
      int copy_len = MIN(src_len, tmp_len);
      if (i > 0) {
	*tmp++ = ' ';
	tmp_len--;
        }
      memcpy(tmp, ARGV[i], copy_len);
      tmp += copy_len;
      tmp_len -= copy_len;
      if (tmp_len <= 0) {
        return;
	}
      }
    tmp_len ? memset(tmp, ' ', tmp_len) : 0;
    }
  }

/* Implement F2003 intrinsic "get_command_argument" */
void
_Get_command_argument(pathf90_i4 *number, char *value,
  pathf90_i4 *length, pathf90_i4 *status, pathf90_i4 value_len) {
  char *c_value = ARGV[*number];
  helper((*number < 0 || *number >= ARGC), value, c_value, length, status,
    value_len);
  }

/* Implement F2003 intrinsic "get_environment_variable" */
void
_Get_environment_variable(char *name, char *value, pathf90_i4 *length,
  pathf90_i4 *status, pathf90_i4 *trim_name, pathf90_i4 name_len,
  pathf90_i4 value_len) {
  char *c_name =  C_IFY(name, name_len, (trim_name && !*trim_name));
  char *c_value = getenv(c_name);
  helper(!c_value, value, c_value, length, status, value_len);
  }

#include "liberrno.h"

/* Implement F2003 intrinsic "is_iostat_end" */
pathf90_i4
_Is_iostat_end(pathf90_i4 *iostat) {
  return *iostat == FERDPEOF;
  }

/* Implement F2003 intrinsic "is_iostat_eor" */
pathf90_i4
_Is_iostat_eor(pathf90_i4 *iostat) {
  return *iostat == FEEORCND;
  }

