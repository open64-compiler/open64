/*
 * Copyright (C) 2020-2021 Xcalibyte Limited, Inc.  All Rights Reserved.
 */

//-*-c++-*-

#include "errors.h"
#include "inline_skip.h"

void
INL_SKIPLST::Build_skiplst(const char *skiplst_file)
{
  if (skiplst_file  == NULL)
    return;
  FILE *input = fopen(skiplst_file, "r");
  FmtAssert(input != NULL, ("%s does not exist\n", skiplst_file));

  char *line = NULL;
  size_t line_len = 0;
  ssize_t ret;
  while ((ret = getline(&line, &line_len, input)) > 0) {
    // remove tail '\n'
    if (line[ret - 1] == '\n') {
      line[ret - 1] = '\0';
      -- ret;
    }
    // remove head ' '
    char *str = line;
    while (*str == ' ') {
      ++ str;
    }
    // check if empty string
    if (*str == '\0')
      continue;
    if (_skiplst.find(str) == _skiplst.end()) {
      // make a copy and insert into _skiplst
      _skiplst.insert(strdup(str));
    }
  }
  // free line buffer
  if (line)
    free(line);
  // close file
  fclose(input);
}

void
INL_SKIPLST::Print(FILE *fp) const {
  fprintf(fp, "INLINE skiplist:\n");
  STR_MAP::const_iterator end = _skiplst.end();
  INT i = 0;
  for (STR_MAP::const_iterator it = _skiplst.begin(); it != end; ++it) {
    fprintf(fp, " [%d]: %s\n", i++, *it);
  }
}

