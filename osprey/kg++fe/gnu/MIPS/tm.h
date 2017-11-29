#ifdef SGI_MONGOOSE
// MASK_GNU_AS and MASK_GNU_LD no longer defined
#define TARGET_CPU_DEFAULT (MASK_GAS)
#else
#define TARGET_CPU_DEFAULT (MASK_GNU_AS|MASK_GNU_LD)
#endif /* SGI_MONGOOSE */
#ifdef IN_GCC
#include "gansidecl.h"
#endif
#ifdef IN_GCC
#include "MIPS/linux.h"
#endif
