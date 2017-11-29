/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */


/* 
   Copyright (C) 2002 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/***************************************************************
 * translatetypes.h
 *
 * a simple header to reduce conflicts among identical types with
 * different names and locations across platforms
 *
 **************************************************************/

#ifndef translate_types_h
#define translate_types_h

#if defined(__sun__) 
/**********************

 The different versions of solaris have different ways and locations
 of defining these various types. Fortunately, <sys/synch.h> exists
 on both platforms, and uses these kinds. So we include synch.h simply
 because it does the right thing on both platforms. (In spite of all the
 warnings.)

 In the event we ever get rid of support for 2.5.1, we will be able to
 replace some of this code with 

#include <sys/int_types.h>

 That would be a good thing.

*/
#include <sys/synch.h>
/*
  if we ever drop support for older versions of solaris we can 
  uncomment the line above and get rid of the next several lines.
*/
typedef char           int8_t;
//typedef unsigned char  uint8_t;
typedef short          int16_t;
typedef unsigned short uint16_t;
typedef int            int32_t;
//typedef unsigned int   uint32_t;
typedef long long      int64_t;
//typedef unsigned long long uint64_t;

/******************************/

typedef uint8_t    __uint8_t;
typedef int8_t     __int8_t;
typedef uint16_t   __uint16_t;
typedef int16_t    __int16_t;

/* some files include sgidefs.h which also defines these types.
   I have no idea why sgidefs.h doesn't rely on the usual
   location of these definitions.

   This problem appears as a warning under linux, but as a fatal
   error under solaris
*/
#if !defined(__SGIDEFS_H__) 
typedef uint32_t   __uint32_t;
typedef int32_t    __int32_t;
typedef uint64_t   __uint64_t;
typedef int64_t    __int64_t;
#endif (!__SGIDEFS_H__)

#endif /* __sun__ */

#if defined(__CYGWIN__) || defined(_WIN32)
#include <sys/types.h>
typedef uint8_t    __uint8_t;
typedef int8_t     __int8_t;
typedef uint16_t   __uint16_t;
typedef int16_t    __int16_t;

/* some files include sgidefs.h which also defines these types.
   I have no idea why sgidefs.h doesn't rely on the usual
   location of these definitions.

   This problem appears as a warning under linux, but as a fatal
   error under solaris
*/
#if !defined(__SGIDEFS_H__) 
typedef uint32_t   __uint32_t;
typedef int32_t    __int32_t;
typedef uint64_t   __uint64_t;
typedef int64_t    __int64_t;
#endif
#endif /* __CYGWIN__ || _WIN32 */

#endif /* translate_types_h */


