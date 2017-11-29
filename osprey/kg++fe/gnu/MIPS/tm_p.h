#ifdef IN_GCC
#if defined(TARG_MIPS) && !defined(TARG_SL)
# include "MIPS/mips-protos.h"
#endif
#if defined(TARG_SL)
# include "SL/sl-protos.h"
#endif
#endif
#include "tm-preds.h"
