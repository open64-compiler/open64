
/*
*  Copyright (C) 2008-2009 Advanced Micro Devices, Inc. All Rights Reserved.
*
*  This file is part of libacml_mv.
*
*  libacml_mv is free software; you can redistribute it and/or
*  modify it under the terms of the GNU Lesser General Public
*  License as published by the Free Software Foundation; either
*  version 2.1 of the License, or (at your option) any later version.
*
*  libacml_mv is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*  Lesser General Public License for more details.
*
*  You should have received a copy of the GNU Lesser General Public
*  License along with libacml_mv.  If not, see
*  <http://www.gnu.org/licenses/>.
*
*/


#ifndef __FN_MACROS_H__
#define __FN_MACROS_H__

#if defined(WINDOWS)
#pragma warning( disable : 4985 )
#define FN_PROTOTYPE(fn_name) fast##fn_name
#else
/* For Linux we prepend function names by a double underscore */
#define _concat(x,y) x##y
#define concat(x,y) _concat(x,y)
/* #define FN_PROTOTYPE(fn_name) concat(__,fn_name) */
#define FN_PROTOTYPE(fn_name) concat(fast,fn_name) /* commenting out previous line for build success, !!!!! REVISIT THIS SOON !!!!! */
#endif


#if defined(WINDOWS)
#define weak_alias(name, aliasname) /* as nothing */
#else
/* Define ALIASNAME as a weak alias for NAME.
   If weak aliases are not available, this defines a strong alias.  */
#define weak_alias(name, aliasname) /* _weak_alias (name, aliasname) */ /* !!!!! REVISIT THIS SOON !!!!! */
#define _weak_alias(name, aliasname) extern __typeof (name) aliasname __attribute__ ((weak, alias (#name))); 
#endif

#endif // __FN_MACROS_H__

