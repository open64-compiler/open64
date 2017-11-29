/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

/* Type information for cp/spew.c.
   Copyright (C) 2002 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* This file is machine generated.  Do not edit.  */

void
gt_ggc_mx_feed (x_p)
      void *x_p;
{
  struct feed * const x = (struct feed *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_13unparsed_text ((*x).input);
      switch (((*x)).yychar)
        {
        case NSNAME:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case PTYPENAME_DEFN:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case TYPENAME_DEFN:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case IDENTIFIER_DEFN:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case PRE_PARSED_FUNCTION_DECL:
          gt_ggc_m_13unparsed_text ((*x).yylval.pi);
          break;
        case DEFARG_MARKER:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case DEFARG:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case PRE_PARSED_CLASS_DECL:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case ALL:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case EXTERN_LANG_STRING:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case PTYPENAME:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case CV_QUALIFIER:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case TYPESPEC:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case SCSPEC:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case STRING:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case SELFNAME:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case PFUNCNAME:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case CONSTANT:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case tTYPENAME:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case IDENTIFIER:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case LEFT_RIGHT:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case VISSPEC:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case AGGR:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        case VAR_FUNC_NAME:
          gt_ggc_m_9tree_node ((*x).yylval.ttype);
          break;
        default:
          break;
        }
      gt_ggc_m_4feed ((*x).next);
  }
}

void
gt_ggc_mx_token_chunk (x_p)
      void *x_p;
{
  struct token_chunk * const x = (struct token_chunk *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_11token_chunk ((*x).next);
      {
        size_t i1_0;
        const size_t ilimit1_0 = (TOKEN_CHUNK_SIZE);
        for (i1_0 = 0; i1_0 < ilimit1_0; i1_0++) {
          switch (((*x).toks[i1_0]).yychar)
            {
            case NSNAME:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case PTYPENAME_DEFN:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case TYPENAME_DEFN:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case IDENTIFIER_DEFN:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case PRE_PARSED_FUNCTION_DECL:
              gt_ggc_m_13unparsed_text ((*x).toks[i1_0].yylval.pi);
              break;
            case DEFARG_MARKER:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case DEFARG:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case PRE_PARSED_CLASS_DECL:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case ALL:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case EXTERN_LANG_STRING:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case PTYPENAME:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case CV_QUALIFIER:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case TYPESPEC:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case SCSPEC:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case STRING:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case SELFNAME:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case PFUNCNAME:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case CONSTANT:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case tTYPENAME:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case IDENTIFIER:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case LEFT_RIGHT:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case VISSPEC:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case AGGR:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            case VAR_FUNC_NAME:
              gt_ggc_m_9tree_node ((*x).toks[i1_0].yylval.ttype);
              break;
            default:
              break;
            }
        }
      }
  }
}

void
gt_ggc_mx_unparsed_text (x_p)
      void *x_p;
{
  struct unparsed_text * const x = (struct unparsed_text *)x_p;
  if (ggc_test_and_set_mark (x))
    {
      gt_ggc_m_13unparsed_text ((*x).next);
      gt_ggc_m_9tree_node ((*x).decl);
      gt_ggc_m_11token_chunk ((*x).tokens);
      gt_ggc_m_11token_chunk ((*x).last_chunk);
      gt_ggc_m_11token_chunk ((*x).cur_chunk);
  }
}

/* GC roots.  */

const struct ggc_root_tab gt_ggc_r_gt_cp_spew_h[] = {
  {
    &defarg_fnsdone,
    1,
    sizeof (defarg_fnsdone),
    &gt_ggc_mx_tree_node

  },
  {
    &defarg_depfns,
    1,
    sizeof (defarg_depfns),
    &gt_ggc_mx_tree_node

  },
  {
    &defarg_parm,
    1,
    sizeof (defarg_parm),
    &gt_ggc_mx_tree_node

  },
  {
    &defarg_fns,
    1,
    sizeof (defarg_fns),
    &gt_ggc_mx_tree_node

  },
  {
    &processing_these_inlines,
    1,
    sizeof (processing_these_inlines),
    &gt_ggc_mx_unparsed_text

  },
  {
    &pending_inlines_tail,
    1,
    sizeof (pending_inlines_tail),
    &gt_ggc_mx_unparsed_text

  },
  {
    &pending_inlines,
    1,
    sizeof (pending_inlines),
    &gt_ggc_mx_unparsed_text

  },
  {
    &feed,
    1,
    sizeof (feed),
    &gt_ggc_mx_feed

  },
  LAST_GGC_ROOT_TAB
};

