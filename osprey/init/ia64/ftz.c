#define _FPU_MASK_UM      0x10           /* underflow */
#define _FPU_SF0_MASK_FZ  0x40           /* Status field 0: flush to zero */
#define _FPU_SF1_MASK_FZ  0x80000        /* Status field 1: flush to zero */
#define _FPU_SF2_MASK_FZ  0x100000000    /* Status field 2: flush to zero */
#define _FPU_SF3_MASK_FZ  0x200000000000 /* Status field 3: flush to zero */

static void __attribute__ ((constructor)) fz_ ( void )
{
   unsigned long fpsr_val, new_fpsr;

   __asm__ __volatile__ ("mov %0=ar.fpsr" : "=r"(fpsr_val) :: "memory");

   new_fpsr = fpsr_val | (_FPU_MASK_UM | _FPU_SF0_MASK_FZ |
      _FPU_SF1_MASK_FZ | _FPU_SF2_MASK_FZ | _FPU_SF3_MASK_FZ);

   __asm__ __volatile__ ("mov ar.fpsr=%0" :: "r"(new_fpsr) : "memory");
}
