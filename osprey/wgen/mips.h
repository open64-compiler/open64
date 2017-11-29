/*
 * Copyright 2007 (C) PathScale, LLC.  All Rights Reserved.
 */

/* Definitions of target machine for GNU compiler.  MIPS version.
   Copyright (C) 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998
   1999, 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by A. Lichnewsky (lich@inria.inria.fr).
   Changed by Michael Meissner	(meissner@osf.org).
   64 bit r4000 support by Ian Lance Taylor (ian@cygnus.com) and
   Brendan Eich (brendan@microunity.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#define FIRST_PSEUDO_REGISTER 188

#define REGISTER_NAMES							   \
{ "$0",   "$1",   "$2",   "$3",   "$4",   "$5",   "$6",   "$7",		   \
  "$8",   "$9",   "$10",  "$11",  "$12",  "$13",  "$14",  "$15",	   \
  "$16",  "$17",  "$18",  "$19",  "$20",  "$21",  "$22",  "$23",	   \
  "$24",  "$25",  "$26",  "$27",  "$28",  "$sp",  "$fp",  "$31",	   \
  "$f0",  "$f1",  "$f2",  "$f3",  "$f4",  "$f5",  "$f6",  "$f7",	   \
  "$f8",  "$f9",  "$f10", "$f11", "$f12", "$f13", "$f14", "$f15",	   \
  "$f16", "$f17", "$f18", "$f19", "$f20", "$f21", "$f22", "$f23",	   \
  "$f24", "$f25", "$f26", "$f27", "$f28", "$f29", "$f30", "$f31",	   \
  "hi",   "lo",   "",     "$fcc0","$fcc1","$fcc2","$fcc3","$fcc4",	   \
  "$fcc5","$fcc6","$fcc7","", "", "$arg", "$frame", "$fakec",		   \
  "$c0r0", "$c0r1", "$c0r2", "$c0r3", "$c0r4", "$c0r5", "$c0r6", "$c0r7",  \
  "$c0r8", "$c0r9", "$c0r10","$c0r11","$c0r12","$c0r13","$c0r14","$c0r15", \
  "$c0r16","$c0r17","$c0r18","$c0r19","$c0r20","$c0r21","$c0r22","$c0r23", \
  "$c0r24","$c0r25","$c0r26","$c0r27","$c0r28","$c0r29","$c0r30","$c0r31", \
  "$c2r0", "$c2r1", "$c2r2", "$c2r3", "$c2r4", "$c2r5", "$c2r6", "$c2r7",  \
  "$c2r8", "$c2r9", "$c2r10","$c2r11","$c2r12","$c2r13","$c2r14","$c2r15", \
  "$c2r16","$c2r17","$c2r18","$c2r19","$c2r20","$c2r21","$c2r22","$c2r23", \
  "$c2r24","$c2r25","$c2r26","$c2r27","$c2r28","$c2r29","$c2r30","$c2r31", \
  "$c3r0", "$c3r1", "$c3r2", "$c3r3", "$c3r4", "$c3r5", "$c3r6", "$c3r7",  \
  "$c3r8", "$c3r9", "$c3r10","$c3r11","$c3r12","$c3r13","$c3r14","$c3r15", \
  "$c3r16","$c3r17","$c3r18","$c3r19","$c3r20","$c3r21","$c3r22","$c3r23", \
  "$c3r24","$c3r25","$c3r26","$c3r27","$c3r28","$c3r29","$c3r30","$c3r31", \
  "$ac1hi","$ac1lo","$ac2hi","$ac2lo","$ac3hi","$ac3lo","$dsp_po","$dsp_sc", \
  "$dsp_ca","$dsp_ou","$dsp_cc","$dsp_ef" }

/* List the "software" names for each register.  Also list the numerical
   names for $fp and $sp.  */

#define GP_REG_FIRST 0

#define ADDITIONAL_REGISTER_NAMES					\
{									\
  { "$29",	29 + GP_REG_FIRST },					\
  { "$30",	30 + GP_REG_FIRST },					\
  { "at",	 1 + GP_REG_FIRST },					\
  { "v0",	 2 + GP_REG_FIRST },					\
  { "v1",	 3 + GP_REG_FIRST },					\
  { "a0",	 4 + GP_REG_FIRST },					\
  { "a1",	 5 + GP_REG_FIRST },					\
  { "a2",	 6 + GP_REG_FIRST },					\
  { "a3",	 7 + GP_REG_FIRST },					\
  { "t0",	 8 + GP_REG_FIRST },					\
  { "t1",	 9 + GP_REG_FIRST },					\
  { "t2",	10 + GP_REG_FIRST },					\
  { "t3",	11 + GP_REG_FIRST },					\
  { "t4",	12 + GP_REG_FIRST },					\
  { "t5",	13 + GP_REG_FIRST },					\
  { "t6",	14 + GP_REG_FIRST },					\
  { "t7",	15 + GP_REG_FIRST },					\
  { "s0",	16 + GP_REG_FIRST },					\
  { "s1",	17 + GP_REG_FIRST },					\
  { "s2",	18 + GP_REG_FIRST },					\
  { "s3",	19 + GP_REG_FIRST },					\
  { "s4",	20 + GP_REG_FIRST },					\
  { "s5",	21 + GP_REG_FIRST },					\
  { "s6",	22 + GP_REG_FIRST },					\
  { "s7",	23 + GP_REG_FIRST },					\
  { "t8",	24 + GP_REG_FIRST },					\
  { "t9",	25 + GP_REG_FIRST },					\
  { "k0",	26 + GP_REG_FIRST },					\
  { "k1",	27 + GP_REG_FIRST },					\
  { "gp",	28 + GP_REG_FIRST },					\
  { "sp",	29 + GP_REG_FIRST },					\
  { "fp",	30 + GP_REG_FIRST },					\
  { "ra",	31 + GP_REG_FIRST },					\
  ALL_COP_ADDITIONAL_REGISTER_NAMES					\
}

/* This is meant to be redefined in the host dependent files.  It is a
 *    set of alternative names and regnums for mips coprocessors.  */

#define ALL_COP_ADDITIONAL_REGISTER_NAMES
