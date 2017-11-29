/* GNU C varargs and stdargs support for the Intel ia64.  */

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
typedef void *__gnuc_va_list;
#endif

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

#if !defined(_STDARG_H)

/* In GCC version 2, we want an ellipsis at the end of the declaration
   of the argument list.  GCC version 1 can't parse it.  */

#if __GNUC__ > 1
#define __va_ellipsis ...
#else
#define __va_ellipsis
#endif

#define va_alist  __builtin_va_alist
/* The ... causes current_function_varargs to be set in cc1.  */
#define va_dcl    long long __builtin_va_alist; __va_ellipsis

#define va_start(AP)							\
 (AP = ((__gnuc_va_list) __builtin_next_arg ()				\
	- (__builtin_args_info (0) >= 8	? 8 : 0)))

#else /* STDARG_H */

#define va_start(AP, LASTARG) 						\
 (AP = ((__gnuc_va_list) __builtin_next_arg (LASTARG)))

#endif /* _STDARG_H */

/* This is essentially the same as stdarg.h, except that int has been replaced
   with long, and we test __BIG_ENDIAN__ to see if this is little endian code
   or not.  */

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (long) - 1) / sizeof (long)) * sizeof (long))

/* Arguments larger than 8 bytes are padded to 16 byte boundaries.  */

#define __va_padded_size(AP, TYPE) \
  (sizeof (TYPE) > 8 && (((char *) AP - (char *) 0) & 15)		\
   ? 16 - (((char *) AP - (char *) 0) & 15) : 0)

#undef va_end
void va_end (__gnuc_va_list);		/* Defined in libgcc.a */
#define va_end(AP)	((void)0)

/* We cast to void * and then to TYPE * because this avoids
   a warning about increasing the alignment requirement.  */

#ifndef __BIG_ENDIAN__
/* This is for little-endian machines; small args are padded upward.  */
#define va_arg(AP, TYPE)						\
 (AP = (__gnuc_va_list) ((char *) (AP) + __va_rounded_size (TYPE)	\
			 + __va_padded_size (AP, TYPE)),		\
  *((TYPE *) (void *) ((char *) (AP) - __va_rounded_size (TYPE))))
#else /* big-endian */
/* This is for big-endian machines; small args are padded downward.  */
#define va_arg(AP, TYPE)						\
 (AP = (__gnuc_va_list) ((char *) (AP) + __va_rounded_size (TYPE)	\
			 + __va_padded_size (AP, TYPE)),		\
  *((TYPE *) (void *) ((char *) (AP)					\
		       - ((sizeof (TYPE) < __va_rounded_size (char)	\
			   ? sizeof (TYPE) : __va_rounded_size (TYPE))))))
#endif /* big-endian */

/* Copy __gnuc_va_list into another variable of this type.  */
#define __va_copy(dest, src) (dest) = (src)

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
