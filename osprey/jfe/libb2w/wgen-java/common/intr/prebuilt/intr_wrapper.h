/* ACOS                      acosf      r_acos                                    */     float                     intr_wrap_000 (float p1)  { return acosf(p1); }
/* DACOS                     acos       d_acos                                    */     double                    intr_wrap_001 (double p1)  { return acos(p1); }
/* QACOS                     __qacos    __q_acos                                  */     long double               intr_wrap_002 (long double p1)  { return __qacos(p1); }
/* QARCOS                    __qacos    __q_acos                                  */     long double               intr_wrap_003 (long double p1)  { return __qacos(p1); }
/* ACOSD                     __racosd   r_acosd                                   */     float                     intr_wrap_004 (float p1)  { return __racosd(p1); }
/* DACOSD                    __dacosd   d_acosd                                   */     double                    intr_wrap_005 (double p1)  { return __dacosd(p1); }
/* QACOSD                               __q_acosd                                 */     long double               intr_wrap_006 (long double p1)  { return __q_acosd(&p1); }
/* ASIN                      asinf      r_asin                                    */     float                     intr_wrap_007 (float p1)  { return asinf(p1); }
/* DASIN                     asin       d_asin                                    */     double                    intr_wrap_008 (double p1)  { return asin(p1); }
/* QASIN                     __qasin    __q_asin                                  */     long double               intr_wrap_009 (long double p1)  { return __qasin(p1); }
/* QARSIN                    __qasin    __q_asin                                  */     long double               intr_wrap_010 (long double p1)  { return __qasin(p1); }
/* ASIND                     __rasind   r_asind                                   */     float                     intr_wrap_011 (float p1)  { return __rasind(p1); }
/* DASIND                    __dasind   d_asind                                   */     double                    intr_wrap_012 (double p1)  { return __dasind(p1); }
/* QASIND                               __q_asind                                 */     long double               intr_wrap_013 (long double p1)  { return __q_asind(&p1); }
/* ATAN                      atanf      r_atan                                    */     float                     intr_wrap_014 (float p1)  { return atanf(p1); }
/* DATAN                     atan       d_atan                                    */     double                    intr_wrap_015 (double p1)  { return atan(p1); }
/* QATAN                     __qatan    __q_atan                                  */     long double               intr_wrap_016 (long double p1)  { return __qatan(p1); }
/* ATAND                     __ratand   r_atand                                   */     float                     intr_wrap_017 (float p1)  { return __ratand(p1); }
/* DATAND                    __datand   d_atand                                   */     double                    intr_wrap_018 (double p1)  { return __datand(p1); }
/* QATAND                               __q_atand                                 */     long double               intr_wrap_019 (long double p1)  { return __q_atand(&p1); }
/* ATAN2                     atan2f     r_atn2                                    */     float                     intr_wrap_020 (float p1, float p2)  { return atan2f(p1, p2); }
/* DATAN2                    atan2      d_atn2                                    */     double                    intr_wrap_021 (double p1, double p2)  { return atan2(p1, p2); }
/* QATAN2                    __qatan2   __q_atn2                                  */     long double               intr_wrap_022 (long double p1, long double p2)  { return __qatan2(p1, p2); }
/* ATAN2D                    __ratn2d   r_atn2d                                   */     float                     intr_wrap_023 (float p1, float p2)  { return __ratn2d(p1, p2); }
/* DATAN2D                   __datn2d   d_atn2d                                   */     double                    intr_wrap_024 (double p1, double p2)  { return __datn2d(p1, p2); }
/* QATAN2D                   __qatan2d  __q_atn2d                                 */     long double               intr_wrap_025 (long double p1, long double p2)  { return __qatan2d(p1, p2); }
/* COS                       cosf       r_cos                                     */     float                     intr_wrap_026 (float p1)  { return cosf(p1); }
/* DCOS                      cos        d_cos                                     */     double                    intr_wrap_027 (double p1)  { return cos(p1); }
/* QCOS                      __qcos     __q_cos                                   */     long double               intr_wrap_028 (long double p1)  { return __qcos(p1); }
/* CCOS                      __ccos     c_cos_                                    */     struct _cpx_float         intr_wrap_029 (struct _cpx_float p1)  { return __ccos(p1.real, p1.imag); }
/* ZCOS                      __zcos     z_cos_                                    */     struct _cpx_double        intr_wrap_030 (struct _cpx_double p1)  { return __zcos(p1.dreal, p1.dimag); }
/* CDCOS                     __zcos     z_cos_                                    */     struct _cpx_double        intr_wrap_031 (struct _cpx_double p1)  { return __zcos(p1.dreal, p1.dimag); }
/* CQCOS                     __cqcos    __cq_cos                                  */     struct _cpx_long_double   intr_wrap_032 (struct _cpx_long_double p1)  { return __cqcos(p1.qreal, p1.qimag); }
/* COSD                      __rcosd    r_cosd                                    */     float                     intr_wrap_033 (float p1)  { return __rcosd(p1); }
/* DCOSD                     __dcosd    d_cosd                                    */     double                    intr_wrap_034 (double p1)  { return __dcosd(p1); }
/* QCOSD                                __q_cosd                                  */     long double               intr_wrap_035 (long double p1)  { return __q_cosd(&p1); }
/* COSH                      coshf      r_cosh                                    */     float                     intr_wrap_036 (float p1)  { return coshf(p1); }
/* DCOSH                     cosh       d_cosh                                    */     double                    intr_wrap_037 (double p1)  { return cosh(p1); }
/* QCOSH                     __qcosh    __q_cosh                                  */     long double               intr_wrap_038 (long double p1)  { return __qcosh(p1); }
/* EXP                       expf       r_exp                                     */     float                     intr_wrap_039 (float p1)  { return expf(p1); }
/* DEXP                      exp        d_exp                                     */     double                    intr_wrap_040 (double p1)  { return exp(p1); }
/* QEXP                      __qexp     __q_exp                                   */     long double               intr_wrap_041 (long double p1)  { return __qexp(p1); }
/* CEXP                      __cexp     c_exp_                                    */     struct _cpx_float         intr_wrap_042 (struct _cpx_float p1)  { return __cexp(p1.real, p1.imag); }
/* ZEXP                      __zexp     z_exp_                                    */     struct _cpx_double        intr_wrap_043 (struct _cpx_double p1)  { return __zexp(p1.dreal, p1.dimag); }
/* CDEXP                     __zexp     z_exp_                                    */     struct _cpx_double        intr_wrap_044 (struct _cpx_double p1)  { return __zexp(p1.dreal, p1.dimag); }
/* CQEXP                     __cqexp    __cq_exp                                  */     struct _cpx_long_double   intr_wrap_045 (struct _cpx_long_double p1)  { return __cqexp(p1.qreal, p1.qimag); }
/* LOG                                                                            */     /* not specific */
/* ALOG                      logf       r_log                                     */     float                     intr_wrap_047 (float p1)  { return logf(p1); }
/* DLOG                      log        d_log                                     */     double                    intr_wrap_048 (double p1)  { return log(p1); }
/* QLOG                      __qlog     __q_log                                   */     long double               intr_wrap_049 (long double p1)  { return __qlog(p1); }
/* CLOG                      __clog     c_log_                                    */     struct _cpx_float         intr_wrap_050 (struct _cpx_float p1)  { return __clog(p1.real, p1.imag); }
/* ZLOG                      __zlog     z_log_                                    */     struct _cpx_double        intr_wrap_051 (struct _cpx_double p1)  { return __zlog(p1.dreal, p1.dimag); }
/* CDLOG                     __zlog     z_log_                                    */     struct _cpx_double        intr_wrap_052 (struct _cpx_double p1)  { return __zlog(p1.dreal, p1.dimag); }
/* CQLOG                     __cqlog    __cq_log                                  */     struct _cpx_long_double   intr_wrap_053 (struct _cpx_long_double p1)  { return __cqlog(p1.qreal, p1.qimag); }
/* LOG10                                                                          */     /* not specific */
/* ALOG10                    log10f     r_lg10                                    */     float                     intr_wrap_055 (float p1)  { return log10f(p1); }
/* DLOG10                    log10      d_lg10                                    */     double                    intr_wrap_056 (double p1)  { return log10(p1); }
/* QLOG10                    log10l     __q_lg10                                  */     long double               intr_wrap_057 (long double p1)  { return log10l(p1); }
/* SIN                       sinf       r_sin                                     */     float                     intr_wrap_058 (float p1)  { return sinf(p1); }
/* DSIN                      sin        d_sin                                     */     double                    intr_wrap_059 (double p1)  { return sin(p1); }
/* QSIN                      __qsin     __q_sin                                   */     long double               intr_wrap_060 (long double p1)  { return __qsin(p1); }
/* CSIN                      __csin     c_sin_                                    */     struct _cpx_float         intr_wrap_061 (struct _cpx_float p1)  { return __csin(p1.real, p1.imag); }
/* ZSIN                      __zsin     z_sin_                                    */     struct _cpx_double        intr_wrap_062 (struct _cpx_double p1)  { return __zsin(p1.dreal, p1.dimag); }
/* CDSIN                     __zsin     z_sin_                                    */     struct _cpx_double        intr_wrap_063 (struct _cpx_double p1)  { return __zsin(p1.dreal, p1.dimag); }
/* CQSIN                     __cqsin    __cq_sin                                  */     struct _cpx_long_double   intr_wrap_064 (struct _cpx_long_double p1)  { return __cqsin(p1.qreal, p1.qimag); }
/* SIND                      __rsind    r_sind                                    */     float                     intr_wrap_065 (float p1)  { return __rsind(p1); }
/* DSIND                     __dsind    d_sind                                    */     double                    intr_wrap_066 (double p1)  { return __dsind(p1); }
/* QSIND                                __q_sind                                  */     long double               intr_wrap_067 (long double p1)  { return __q_sind(&p1); }
/* SINH                      sinhf      r_sinh                                    */     float                     intr_wrap_068 (float p1)  { return sinhf(p1); }
/* DSINH                     sinh       d_sinh                                    */     double                    intr_wrap_069 (double p1)  { return sinh(p1); }
/* QSINH                     __qsinh    __q_sinh                                  */     long double               intr_wrap_070 (long double p1)  { return __qsinh(p1); }
/* SQRT                      sqrtf      r_sqrt               OPC_F4SQRT           */     float                     intr_wrap_071 (float p1)  { return sqrtf(p1); }
/* DSQRT                     sqrt       d_sqrt               OPC_F8SQRT           */     double                    intr_wrap_072 (double p1)  { return sqrt(p1); }
/* QSQRT                     __qsqrt    __q_sqrt             OPC_FQSQRT           */     long double               intr_wrap_073 (long double p1)  { return __qsqrt(p1); }
/* CSQRT                     __csqrt    c_sqrt_              OPC_C4SQRT           */     struct _cpx_float         intr_wrap_074 (struct _cpx_float p1)  { return __csqrt(p1.real, p1.imag); }
/* ZSQRT                     __zsqrt    z_sqrt_              OPC_C8SQRT           */     struct _cpx_double        intr_wrap_075 (struct _cpx_double p1)  { return __zsqrt(p1.dreal, p1.dimag); }
/* CDSQRT                    __zsqrt    z_sqrt_              OPC_C8SQRT           */     struct _cpx_double        intr_wrap_076 (struct _cpx_double p1)  { return __zsqrt(p1.dreal, p1.dimag); }
/* CQSQRT                    __cqsqrt   __cq_sqrt            OPC_CQSQRT           */     struct _cpx_long_double   intr_wrap_077 (struct _cpx_long_double p1)  { return __cqsqrt(p1.qreal, p1.qimag); }
/* TAN                       tanf       r_tan                                     */     float                     intr_wrap_078 (float p1)  { return tanf(p1); }
/* DTAN                      tan        d_tan                                     */     double                    intr_wrap_079 (double p1)  { return tan(p1); }
/* QTAN                      __qtan     __q_tan                                   */     long double               intr_wrap_080 (long double p1)  { return __qtan(p1); }
/* TAND                      __rtand    r_tand                                    */     float                     intr_wrap_081 (float p1)  { return __rtand(p1); }
/* DTAND                     __dtand    d_tand                                    */     double                    intr_wrap_082 (double p1)  { return __dtand(p1); }
/* QTAND                                __q_tand                                  */     long double               intr_wrap_083 (long double p1)  { return __q_tand(&p1); }
/* TANH                      tanhf      r_tanh                                    */     float                     intr_wrap_084 (float p1)  { return tanhf(p1); }
/* DTANH                     tanh       d_tanh                                    */     double                    intr_wrap_085 (double p1)  { return tanh(p1); }
/* QTANH                     __qtanh    __q_tanh                                  */     long double               intr_wrap_086 (long double p1)  { return __qtanh(p1); }
/* ABS                                  r_abs                OPC_F4ABS            */     float                     intr_wrap_087 (float p1)  { return r_abs(&p1); }
/*                                      b_abs                OPC_I4ABS            */     signed char               intr_wrap_088 (signed char p1)  { return b_abs(&p1); }
/*                                      h_abs                OPC_I4ABS            */     short                     intr_wrap_089 (short p1)  { return h_abs(&p1); }
/*                                      i_abs                OPC_I4ABS            */     int                       intr_wrap_090 (int p1)  { return i_abs(&p1); }
/*                                      l_abs                OPC_I8ABS            */     long long                 intr_wrap_091 (long long p1)  { return l_abs(&p1); }
/* DABS                                 d_abs                OPC_F8ABS            */     double                    intr_wrap_092 (double p1)  { return d_abs(&p1); }
/* QABS                      __qabs     __q_abs              OPC_FQABS            */     long double               intr_wrap_093 (long double p1)  { return __qabs(p1); }
/* CABS                      __c8abs    c_abs_                                    */     float                     intr_wrap_094 (struct _cpx_float p1)  { return __c8abs(p1.real, p1.imag); }
/* ZABS                      __zabs     z_abs_                                    */     double                    intr_wrap_095 (struct _cpx_double p1)  { return __zabs(p1.dreal, p1.dimag); }
/* CDABS                     __zabs     z_abs_                                    */     double                    intr_wrap_096 (struct _cpx_double p1)  { return __zabs(p1.dreal, p1.dimag); }
/* CQABS                     __cqabs    __cq_abs                                  */     long double               intr_wrap_097 (struct _cpx_long_double p1)  { return __cqabs(p1.qreal, p1.qimag); }
/* IABS                                 i_abs                OPC_I4ABS            */     int                       intr_wrap_098 (int p1)  { return i_abs(&p1); }
/*                                      b_abs                OPC_I4ABS            */     signed char               intr_wrap_099 (signed char p1)  { return b_abs(&p1); }
/* IIABS                                h_abs                OPC_I4ABS            */     short                     intr_wrap_100 (short p1)  { return h_abs(&p1); }
/* HABS                                 h_abs                OPC_I4ABS            */     short                     intr_wrap_101 (short p1)  { return h_abs(&p1); }
/* JIABS                                i_abs                OPC_I4ABS            */     int                       intr_wrap_102 (int p1)  { return i_abs(&p1); }
/* KIABS                                l_abs                OPC_I8ABS            */     long long                 intr_wrap_103 (long long p1)  { return l_abs(&p1); }
/* CMPLX                                                                          */     /* not specific */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/* CONJG                     __rconjg   r_cnjg_                                   */     struct _cpx_float         intr_wrap_115 (struct _cpx_float p1)  { return __rconjg(p1.real, p1.imag); }
/* DCONJG                    __dconjg   d_cnjg_                                   */     struct _cpx_double        intr_wrap_116 (struct _cpx_double p1)  { return __dconjg(p1.dreal, p1.dimag); }
/* QCONJG                    __qconjg   __cq_conjg                                */     struct _cpx_long_double   intr_wrap_117 (struct _cpx_long_double p1)  { return __qconjg(p1.qreal, p1.qimag); }
/* DBLE         cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* DBLEQ        cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* DREAL        cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* DCMPLX                                                                         */     /* not specific */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/* QCMPLX                                                                         */     /* not specific */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/*              complex                                                           */     /* complex inlined in IL */
/* DFLOAT                                                                         */     /* not specific */
/*              cast                                                              */     /* cast inlined in IL */
/* DFLOTI       cast                                                              */     /* cast inlined in IL */
/* DFLOTJ       cast                                                              */     /* cast inlined in IL */
/* DFLOTK       cast                                                              */     /* cast inlined in IL */
/* DFLOATK      cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* DIM                                  r_dim                                     */     float                     intr_wrap_162 (float p1, float p2)  { return r_dim(&p1, &p2); }
/*                                      b_dim                                     */     signed char               intr_wrap_163 (signed char p1, signed char p2)  { return b_dim(&p1, &p2); }
/*                                      h_dim                                     */     short                     intr_wrap_164 (short p1, short p2)  { return h_dim(&p1, &p2); }
/*                                      i_dim                                     */     int                       intr_wrap_165 (int p1, int p2)  { return i_dim(&p1, &p2); }
/*                                      l_dim                                     */     long long                 intr_wrap_166 (long long p1, long long p2)  { return l_dim(&p1, &p2); }
/* DDIM                                 d_dim                                     */     double                    intr_wrap_167 (double p1, double p2)  { return d_dim(&p1, &p2); }
/* QDIM                                 __qdim                                    */     long double               intr_wrap_168 (long double p1, long double p2)  { return __qdim(&p1, &p2); }
/* IDIM                                 i_dim                                     */     int                       intr_wrap_169 (int p1, int p2)  { return i_dim(&p1, &p2); }
/*                                      b_dim                                     */     signed char               intr_wrap_170 (signed char p1, signed char p2)  { return b_dim(&p1, &p2); }
/* IIDIM                                h_dim                                     */     short                     intr_wrap_171 (short p1, short p2)  { return h_dim(&p1, &p2); }
/* HDIM                                 h_dim                                     */     short                     intr_wrap_172 (short p1, short p2)  { return h_dim(&p1, &p2); }
/* JIDIM                                i_dim                                     */     int                       intr_wrap_173 (int p1, int p2)  { return i_dim(&p1, &p2); }
/* KIDIM                                l_dim                                     */     long long                 intr_wrap_174 (long long p1, long long p2)  { return l_dim(&p1, &p2); }
/* DPROD                                d_prod                                    */     double                    intr_wrap_175 (float p1, float p2)  { return d_prod(&p1, &p2); }
/* QPROD                     __qprod    __q_prod                                  */     long double               intr_wrap_176 (double p1, double p2)  { return __qprod(p1, p2); }
/* FLOAT        cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* FLOATI       cast                                                              */     /* cast inlined in IL */
/* FLOATJ       cast                                                              */     /* cast inlined in IL */
/* FLOATK       cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* IFIX         cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* IIFIX        cast                                                              */     /* cast inlined in IL */
/* HFIX         cast                                                              */     /* cast inlined in IL */
/* JIFIX        cast                                                              */     /* cast inlined in IL */
/* KIFIX        cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* IMAG                                                                           */     /* not specific */
/*                                      r_imag               OPC_F4IMAGPART       */     float                     intr_wrap_208 (struct _cpx_float p1)  { return r_imag(&p1); }
/*                                      d_imagalt            OPC_F8IMAGPART       */     double                    intr_wrap_209 (struct _cpx_float p1)  { return d_imagalt(&p1); }
/* AIMAG                     invalid    invalid              OPCODE_INVALID       */     /* invalid:  done at runtime */
/* DIMAG                                d_imag               OPC_F8IMAGPART       */     double                    intr_wrap_211 (struct _cpx_double p1)  { return d_imag(&p1); }
/* QIMAG                                __cq_imag            OPC_FQIMAGPART       */     long double               intr_wrap_212 (struct _cpx_long_double p1)  { return __cq_imag(&p1); }
/* INT          cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* IINT         cast                    __iirint                                  */     /* cast inlined in IL */
/* JINT         cast                    __jirint                                  */     /* cast inlined in IL */
/* KINT         cast                    __kirint                                  */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* INT1                                                                           */     /* not specific */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* INT2                                                                           */     /* not specific */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* SHORT                                                                          */     /* not specific */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* INT4                                                                           */     /* not specific */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* LONG                                                                           */     /* not specific */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* INT8                                                                           */     /* not specific */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* AINT                      truncf     r_int                                     */     float                     intr_wrap_327 (float p1)  { return truncf(p1); }
/* DINT                      trunc      d_int                                     */     double                    intr_wrap_328 (double p1)  { return trunc(p1); }
/* QINT                      __qint     __q_int                                   */     long double               intr_wrap_329 (long double p1)  { return __qint(p1); }
/* IDINT        cast                                                              */     /* cast inlined in IL */
/* IIDINT       cast                    __iidint                                  */     /* cast inlined in IL */
/* JIDINT       cast                    __jidint                                  */     /* cast inlined in IL */
/* KIDINT       cast                    __kidint                                  */     /* cast inlined in IL */
/* IQINT        cast                                                              */     /* cast inlined in IL */
/* IIQINT       cast                    __iiqint                                  */     /* cast inlined in IL */
/* JIQINT       cast                    __jiqint                                  */     /* cast inlined in IL */
/* KIQINT       cast                    __kiqint                                  */     /* cast inlined in IL */
/* MAX                                                                            */     /* not specific */
/*                                                           OPC_I4MAX            */     /* min/max handled explicitly */
/*                                                           OPC_I4MAX            */     /* min/max handled explicitly */
/*                                                           OPC_I4MAX            */     /* min/max handled explicitly */
/*                                                           OPC_I8MAX            */     /* min/max handled explicitly */
/* AMAX1                                                     OPC_F4MAX            */     /* min/max handled explicitly */
/* DMAX1                                                     OPC_F8MAX            */     /* min/max handled explicitly */
/* QMAX1                                                     OPC_FQMAX            */     /* min/max handled explicitly */
/* MAX0                                                      OPC_I4MAX            */     /* min/max handled explicitly */
/*                                                           OPC_I4MAX            */     /* min/max handled explicitly */
/* IMAX0                                                     OPC_I4MAX            */     /* min/max handled explicitly */
/* JMAX0                                                     OPC_I4MAX            */     /* min/max handled explicitly */
/* KMAX0                                                     OPC_I8MAX            */     /* min/max handled explicitly */
/* MAX1                                                      OPC_F4MAX            */     /* min/max handled explicitly */
/* IMAX1                                                     OPC_F4MAX            */     /* min/max handled explicitly */
/* JMAX1                                                     OPC_F4MAX            */     /* min/max handled explicitly */
/* KMAX1                                                     OPC_F4MAX            */     /* min/max handled explicitly */
/* AMAX0                                                     OPC_I4MAX            */     /* min/max handled explicitly */
/*                                                           OPC_I4MAX            */     /* min/max handled explicitly */
/* AIMAX0                                                    OPC_I4MAX            */     /* min/max handled explicitly */
/* AJMAX0                                                    OPC_I4MAX            */     /* min/max handled explicitly */
/* AKMAX0                                                    OPC_I8MAX            */     /* min/max handled explicitly */
/* MIN                                                                            */     /* not specific */
/*                                                           OPC_I4MIN            */     /* min/max handled explicitly */
/*                                                           OPC_I4MIN            */     /* min/max handled explicitly */
/*                                                           OPC_I4MIN            */     /* min/max handled explicitly */
/*                                                           OPC_I8MIN            */     /* min/max handled explicitly */
/* AMIN1                                                     OPC_F4MIN            */     /* min/max handled explicitly */
/* DMIN1                                                     OPC_F8MIN            */     /* min/max handled explicitly */
/* QMIN1                                                     OPC_FQMIN            */     /* min/max handled explicitly */
/* MIN0                                                      OPC_I4MIN            */     /* min/max handled explicitly */
/*                                                           OPC_I4MIN            */     /* min/max handled explicitly */
/* IMIN0                                                     OPC_I4MIN            */     /* min/max handled explicitly */
/* JMIN0                                                     OPC_I4MIN            */     /* min/max handled explicitly */
/* KMIN0                                                     OPC_I8MIN            */     /* min/max handled explicitly */
/* MIN1                                                      OPC_F4MIN            */     /* min/max handled explicitly */
/* IMIN1                                                     OPC_F4MIN            */     /* min/max handled explicitly */
/* JMIN1                                                     OPC_F4MIN            */     /* min/max handled explicitly */
/* KMIN1                                                     OPC_F4MIN            */     /* min/max handled explicitly */
/* AMIN0                                                     OPC_I4MIN            */     /* min/max handled explicitly */
/*                                                           OPC_I4MIN            */     /* min/max handled explicitly */
/* AIMIN0                                                    OPC_I4MIN            */     /* min/max handled explicitly */
/* AJMIN0                                                    OPC_I4MIN            */     /* min/max handled explicitly */
/* AKMIN0                                                    OPC_I8MIN            */     /* min/max handled explicitly */
/* MOD                                  i_mod                OPC_I4REM            */     int                       intr_wrap_382 (int p1, int p2)  { return i_mod(&p1, &p2); }
/*                                      b_mod                OPC_I4REM            */     signed char               intr_wrap_383 (signed char p1, signed char p2)  { return b_mod(&p1, &p2); }
/* IMOD                                 h_mod                OPC_I4REM            */     short                     intr_wrap_384 (short p1, short p2)  { return h_mod(&p1, &p2); }
/* HMOD                                 h_mod                OPC_I4REM            */     short                     intr_wrap_385 (short p1, short p2)  { return h_mod(&p1, &p2); }
/* JMOD                                 i_mod                OPC_I4REM            */     int                       intr_wrap_386 (int p1, int p2)  { return i_mod(&p1, &p2); }
/* KMOD                                 l_mod                OPC_I8REM            */     long long                 intr_wrap_387 (long long p1, long long p2)  { return l_mod(&p1, &p2); }
/* AMOD                      __rmod     r_mod                                     */     float                     intr_wrap_388 (float p1, float p2)  { return __rmod(p1, p2); }
/* DMOD                      __dmod     d_mod                                     */     double                    intr_wrap_389 (double p1, double p2)  { return __dmod(p1, p2); }
/* QMOD                      __qmod     __q_mod                                   */     long double               intr_wrap_390 (long double p1, long double p2)  { return __qmod(p1, p2); }
/* NINT                      invalid    invalid                                   */     /* invalid:  done at runtime */
/* ININT                                h_nint                                    */     short                     intr_wrap_392 (float p1)  { return h_nint(&p1); }
/* JNINT                                i_nint                                    */     int                       intr_wrap_393 (float p1)  { return i_nint(&p1); }
/* KNINT                                l_nint                                    */     long long                 intr_wrap_394 (float p1)  { return l_nint(&p1); }
/*                           invalid    invalid                                   */     /* invalid:  done at runtime */
/*                                      h_dnnt                                    */     short                     intr_wrap_396 (double p1)  { return h_dnnt(&p1); }
/*                                      i_dnnt                                    */     int                       intr_wrap_397 (double p1)  { return i_dnnt(&p1); }
/*                                      l_dnnt                                    */     long long                 intr_wrap_398 (double p1)  { return l_dnnt(&p1); }
/*                           invalid    invalid                                   */     /* invalid:  done at runtime */
/*                           __iiqnnt   __ii_qnnt                                 */     short                     intr_wrap_400 (long double p1)  { return __iiqnnt(p1); }
/*                           __jiqnnt   __ji_qnn                                  */     int                       intr_wrap_401 (long double p1)  { return __jiqnnt(p1); }
/*                           __kiqnnt   __ki_qnn                                  */     long long                 intr_wrap_402 (long double p1)  { return __kiqnnt(p1); }
/* ANINT                     __rnint    r_nint                                    */     float                     intr_wrap_403 (float p1)  { return __rnint(p1); }
/* DNINT                     __dnint    d_nint                                    */     double                    intr_wrap_404 (double p1)  { return __dnint(p1); }
/* QNINT                     __qnint    __q_nint                                  */     long double               intr_wrap_405 (long double p1)  { return __qnint(p1); }
/* IDNINT                    invalid    invalid                                   */     /* invalid:  done at runtime */
/* IIDNNT                               h_dnnt                                    */     short                     intr_wrap_407 (double p1)  { return h_dnnt(&p1); }
/* JIDNNT                               i_dnnt                                    */     int                       intr_wrap_408 (double p1)  { return i_dnnt(&p1); }
/* KIDNNT                               l_dnnt                                    */     long long                 intr_wrap_409 (double p1)  { return l_dnnt(&p1); }
/* IQNINT                    invalid    invalid                                   */     /* invalid:  done at runtime */
/* IIQNNT                    __iiqnnt   __ii_qnnt                                 */     short                     intr_wrap_411 (long double p1)  { return __iiqnnt(p1); }
/* JIQNNT                    __jiqnnt   __ji_qnnt                                 */     int                       intr_wrap_412 (long double p1)  { return __jiqnnt(p1); }
/* KIQNNT                    __kiqnnt   __ki_qnnt                                 */     long long                 intr_wrap_413 (long double p1)  { return __kiqnnt(p1); }
/* QEXT         cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* QEXTD        cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* QREAL        cast                                                              */     /* cast inlined in IL */
/* QFLOAT                                                                         */     /* not specific */
/*              cast                                                              */     /* cast inlined in IL */
/* QFLOTI       cast                                                              */     /* cast inlined in IL */
/* QFLOATI      cast                                                              */     /* cast inlined in IL */
/* QFLOTJ       cast                                                              */     /* cast inlined in IL */
/* QFLOATJ      cast                                                              */     /* cast inlined in IL */
/* QFLOTK       cast                                                              */     /* cast inlined in IL */
/* QFLOATK      cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* REAL         cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* SNGL         cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* SNGLQ        cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* SIGN                      __rsign    r_sign                                    */     float                     intr_wrap_458 (float p1, float p2)  { return __rsign(p1, p2); }
/*                           __bsign    b_sign                                    */     signed char               intr_wrap_459 (signed char p1, signed char p2)  { return __bsign(p1, p2); }
/*                           __hsign    h_sign                                    */     short                     intr_wrap_460 (short p1, short p2)  { return __hsign(p1, p2); }
/*                           __isign    i_sign                                    */     int                       intr_wrap_461 (int p1, int p2)  { return __isign(p1, p2); }
/*                           __lsign    l_sign                                    */     long long                 intr_wrap_462 (long long p1, long long p2)  { return __lsign(p1, p2); }
/* DSIGN                     __dsign    d_sign                                    */     double                    intr_wrap_463 (double p1, double p2)  { return __dsign(p1, p2); }
/* QSIGN                     __qsign    __q_sign                                  */     long double               intr_wrap_464 (long double p1, long double p2)  { return __qsign(p1, p2); }
/* ISIGN                     __isign    i_sign                                    */     int                       intr_wrap_465 (int p1, int p2)  { return __isign(p1, p2); }
/*                           __bsign    b_sign                                    */     signed char               intr_wrap_466 (signed char p1, signed char p2)  { return __bsign(p1, p2); }
/* IISIGN                    __hsign    h_sign                                    */     short                     intr_wrap_467 (short p1, short p2)  { return __hsign(p1, p2); }
/* HSIGN                     __hsign    h_sign                                    */     short                     intr_wrap_468 (short p1, short p2)  { return __hsign(p1, p2); }
/* JISIGN                    __isign    i_sign                                    */     int                       intr_wrap_469 (int p1, int p2)  { return __isign(p1, p2); }
/* KISIGN                    __lsign    l_sign                                    */     long long                 intr_wrap_470 (long long p1, long long p2)  { return __lsign(p1, p2); }
/* ZEXT                                                                           */     /* not specific */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/* IZEXT                                                                          */     /* not specific */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/* JZEXT                                                                          */     /* not specific */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/* KZEXT                                                                          */     /* not specific */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/*              zext                                                              */     /* zext inlined in IL */
/* BTEST                                                                          */     /* not specific */
/*                                      btest_b                                   */     signed char               intr_wrap_502 (signed char p1, signed char p2)  { return btest_b(&p1, &p2); }
/* BITEST                               btest_h                                   */     short                     intr_wrap_503 (short p1, short p2)  { return btest_h(&p1, &p2); }
/* BJTEST                               btest_l                                   */     int                       intr_wrap_504 (int p1, int p2)  { return btest_l(&p1, &p2); }
/* BKTEST                               btest_ll                                  */     long long                 intr_wrap_505 (long long p1, long long p2)  { return btest_ll(&p1, &p2); }
/* IAND                                                                           */     /* not specific */
/*                                      and_b                OPC_I4BAND           */     signed char               intr_wrap_507 (signed char p1, signed char p2)  { return and_b(&p1, &p2); }
/* IIAND                                and_h                OPC_I4BAND           */     short                     intr_wrap_508 (short p1, short p2)  { return and_h(&p1, &p2); }
/* JIAND                                and_l                OPC_I4BAND           */     int                       intr_wrap_509 (int p1, int p2)  { return and_l(&p1, &p2); }
/* KIAND                                and_ll               OPC_I8BAND           */     long long                 intr_wrap_510 (long long p1, long long p2)  { return and_ll(&p1, &p2); }
/* AND                                                                            */     /* not specific */
/*                                      and_b                OPC_I4BAND           */     signed char               intr_wrap_512 (signed char p1, signed char p2)  { return and_b(&p1, &p2); }
/*                                      and_h                OPC_I4BAND           */     short                     intr_wrap_513 (short p1, short p2)  { return and_h(&p1, &p2); }
/*                                      and_l                OPC_I4BAND           */     int                       intr_wrap_514 (int p1, int p2)  { return and_l(&p1, &p2); }
/*                                      and_ll               OPC_I8BAND           */     long long                 intr_wrap_515 (long long p1, long long p2)  { return and_ll(&p1, &p2); }
/* IBCLR                                                                          */     /* not specific */
/*                                      bclr_b                                    */     signed char               intr_wrap_517 (signed char p1, signed char p2)  { return bclr_b(&p1, &p2); }
/* IIBCLR                               bclr_h                                    */     short                     intr_wrap_518 (short p1, short p2)  { return bclr_h(&p1, &p2); }
/* JIBCLR                               bclr_l                                    */     int                       intr_wrap_519 (int p1, int p2)  { return bclr_l(&p1, &p2); }
/* KIBCLR                               bclr_ll                                   */     long long                 intr_wrap_520 (long long p1, long long p2)  { return bclr_ll(&p1, &p2); }
/* IBITS                                                                          */     /* not specific */
/*                                      bext_b                                    */     signed char               intr_wrap_522 (signed char p1, signed char p2, signed char p3)  { return bext_b(&p1, &p2, &p3); }
/* IIBITS                               bext_h                                    */     short                     intr_wrap_523 (short p1, short p2, short p3)  { return bext_h(&p1, &p2, &p3); }
/* JIBITS                               bext_l                                    */     int                       intr_wrap_524 (int p1, int p2, int p3)  { return bext_l(&p1, &p2, &p3); }
/* KIBITS                               bext_ll                                   */     long long                 intr_wrap_525 (long long p1, long long p2, long long p3)  { return bext_ll(&p1, &p2, &p3); }
/* IBSET                                                                          */     /* not specific */
/*                                      bset_b                                    */     signed char               intr_wrap_527 (signed char p1, signed char p2)  { return bset_b(&p1, &p2); }
/* IIBSET                               bset_h                                    */     short                     intr_wrap_528 (short p1, short p2)  { return bset_h(&p1, &p2); }
/* JIBSET                               bset_l                                    */     int                       intr_wrap_529 (int p1, int p2)  { return bset_l(&p1, &p2); }
/* KIBSET                               bset_ll                                   */     long long                 intr_wrap_530 (long long p1, long long p2)  { return bset_ll(&p1, &p2); }
/* IEOR                                                                           */     /* not specific */
/*                                      xor_b                OPC_I4BXOR           */     signed char               intr_wrap_532 (signed char p1, signed char p2)  { return xor_b(&p1, &p2); }
/* IIEOR                                xor_h                OPC_I4BXOR           */     short                     intr_wrap_533 (short p1, short p2)  { return xor_h(&p1, &p2); }
/* JIEOR                                xor_l                OPC_I4BXOR           */     int                       intr_wrap_534 (int p1, int p2)  { return xor_l(&p1, &p2); }
/* KIEOR                                xor_ll               OPC_I8BXOR           */     long long                 intr_wrap_535 (long long p1, long long p2)  { return xor_ll(&p1, &p2); }
/* XOR                                                                            */     /* not specific */
/*                                      xor_b                OPC_I4BXOR           */     signed char               intr_wrap_537 (signed char p1, signed char p2)  { return xor_b(&p1, &p2); }
/*                                      xor_h                OPC_I4BXOR           */     short                     intr_wrap_538 (short p1, short p2)  { return xor_h(&p1, &p2); }
/*                                      xor_l                OPC_I4BXOR           */     int                       intr_wrap_539 (int p1, int p2)  { return xor_l(&p1, &p2); }
/*                                      xor_ll               OPC_I8BXOR           */     long long                 intr_wrap_540 (long long p1, long long p2)  { return xor_ll(&p1, &p2); }
/* IOR                                                                            */     /* not specific */
/*                                      or_b                 OPC_I4BIOR           */     signed char               intr_wrap_542 (signed char p1, signed char p2)  { return or_b(&p1, &p2); }
/* IIOR                                 or_h                 OPC_I4BIOR           */     short                     intr_wrap_543 (short p1, short p2)  { return or_h(&p1, &p2); }
/* JIOR                                 or_l                 OPC_I4BIOR           */     int                       intr_wrap_544 (int p1, int p2)  { return or_l(&p1, &p2); }
/* KIOR                                 or_ll                OPC_I8BIOR           */     long long                 intr_wrap_545 (long long p1, long long p2)  { return or_ll(&p1, &p2); }
/* OR                                                                             */     /* not specific */
/*                                      or_b                 OPC_I4BIOR           */     signed char               intr_wrap_547 (signed char p1, signed char p2)  { return or_b(&p1, &p2); }
/*                                      or_h                 OPC_I4BIOR           */     short                     intr_wrap_548 (short p1, short p2)  { return or_h(&p1, &p2); }
/*                                      or_l                 OPC_I4BIOR           */     int                       intr_wrap_549 (int p1, int p2)  { return or_l(&p1, &p2); }
/*                                      or_ll                OPC_I8BIOR           */     long long                 intr_wrap_550 (long long p1, long long p2)  { return or_ll(&p1, &p2); }
/* ISHFT                                                                          */     /* not specific */
/*                                      shft_b                                    */     signed char               intr_wrap_552 (signed char p1, signed char p2)  { return shft_b(&p1, &p2); }
/* IISHFT                               shft_h                                    */     short                     intr_wrap_553 (short p1, short p2)  { return shft_h(&p1, &p2); }
/* JISHFT                               shft_l                                    */     int                       intr_wrap_554 (int p1, int p2)  { return shft_l(&p1, &p2); }
/* KISHFT                               shft_ll                                   */     long long                 intr_wrap_555 (long long p1, long long p2)  { return shft_ll(&p1, &p2); }
/* LSHIFT                                                                         */     /* not specific */
/*                                      shft_b                                    */     signed char               intr_wrap_557 (signed char p1, signed char p2)  { return shft_b(&p1, &p2); }
/*                                      shft_h                                    */     short                     intr_wrap_558 (short p1, short p2)  { return shft_h(&p1, &p2); }
/*                                      shft_l               OPC_I4SHL            */     int                       intr_wrap_559 (int p1, int p2)  { return shft_l(&p1, &p2); }
/*                                      shft_ll              OPC_I8SHL            */     long long                 intr_wrap_560 (long long p1, long long p2)  { return shft_ll(&p1, &p2); }
/* RSHIFT                                                                         */     /* not specific */
/*                                      rshft_b                                   */     signed char               intr_wrap_562 (signed char p1, signed char p2)  { return rshft_b(&p1, &p2); }
/*                                      rshft_h                                   */     short                     intr_wrap_563 (short p1, short p2)  { return rshft_h(&p1, &p2); }
/*                                      rshft_l              OPC_I4LSHR           */     int                       intr_wrap_564 (int p1, int p2)  { return rshft_l(&p1, &p2); }
/*                                      rshft_ll             OPC_I8LSHR           */     long long                 intr_wrap_565 (long long p1, long long p2)  { return rshft_ll(&p1, &p2); }
/* ISHFTC                                                                         */     /* not specific */
/*                                      shftc_b                                   */     signed char               intr_wrap_567 (signed char p1, signed char p2, signed char p3)  { return shftc_b(&p1, &p2, &p3); }
/* IISHFTC                              shftc_h                                   */     short                     intr_wrap_568 (short p1, short p2, short p3)  { return shftc_h(&p1, &p2, &p3); }
/* JISHFTC                              shftc_l                                   */     int                       intr_wrap_569 (int p1, int p2, int p3)  { return shftc_l(&p1, &p2, &p3); }
/* KISHFTC                              shftc_ll                                  */     long long                 intr_wrap_570 (long long p1, long long p2, long long p3)  { return shftc_ll(&p1, &p2, &p3); }
/* IASHR                                                                          */     /* not specific */
/*                                                           OPC_I4ASHR           */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* IIASHR                                                    OPC_I4ASHR           */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* JIASHR                                                    OPC_I4ASHR           */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* KIASHR                                                    OPC_I8ASHR           */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* NOT                                                                            */     /* not specific */
/*                                      not_b                OPC_I4BNOT           */     signed char               intr_wrap_577 (signed char p1)  { return not_b(&p1); }
/* INOT                                 not_h                OPC_I4BNOT           */     short                     intr_wrap_578 (short p1)  { return not_h(&p1); }
/* JNOT                                 not_l                OPC_I4BNOT           */     int                       intr_wrap_579 (int p1)  { return not_l(&p1); }
/* KNOT                                 not_ll               OPC_I8BNOT           */     long long                 intr_wrap_580 (long long p1)  { return not_ll(&p1); }
/* CHAR         cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* ACHAR        cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/*              cast                                                              */     /* cast inlined in IL */
/* ICHAR        cast                                                              */     /* cast inlined in IL */
/* IACHAR       cast                                                              */     /* cast inlined in IL */
/* INDEX                                i_indx                                    */     int                       intr_wrap_599 (char *s1, char *s2, int l1, int l2)  { return i_indx(s1, s2, l1, l2); }
/* LEN          char_length             i_len                                     */     /* char_length inlined in IL */
/* LGE                                  l_ge                                      */     int                       intr_wrap_601 (char *s1, char *s2, int l1, int l2)  { return l_ge(s1, s2, l1, l2); }
/* LGT                                  l_gt                                      */     int                       intr_wrap_602 (char *s1, char *s2, int l1, int l2)  { return l_gt(s1, s2, l1, l2); }
/* LLE                                  l_le                                      */     int                       intr_wrap_603 (char *s1, char *s2, int l1, int l2)  { return l_le(s1, s2, l1, l2); }
/* LLT                                  l_lt                                      */     int                       intr_wrap_604 (char *s1, char *s2, int l1, int l2)  { return l_lt(s1, s2, l1, l2); }
/* SIZEOF       sizeof                                                            */     /* sizeof inlined in IL */
/* LOC          loc                                                               */     /* OMITTED:  % intrinsics are not needed */
/* %LOC         loc                                                               */     /* OMITTED:  % intrinsics are not needed */
/* %VAL         val                                                               */     /* OMITTED:  % intrinsics are not needed */
/* %REF         ref                                                               */     /* OMITTED:  % intrinsics are not needed */
/* %DESCR       descr                                                             */     /* OMITTED:  % intrinsics are not needed */
/* ALLOCA                    alloca                                               */     /* OMITTED:  calls alloca */
/*                           alloca                                               */     /* OMITTED:  calls alloca */
/*                           alloca                                               */     /* OMITTED:  calls alloca */
/*                           alloca                                               */     /* OMITTED:  calls alloca */
/* MALLOC                    malloc                                               */     /* OMITTED:  calls malloc */
/*                           malloc                                               */     /* OMITTED:  calls malloc */
/*                           malloc                                               */     /* OMITTED:  calls malloc */
/*                           malloc                                               */     /* OMITTED:  calls malloc */
/* FREE                      free                                                 */     /* OMITTED:  returns void */
/*                           free                                                 */     /* OMITTED:  returns void */
/*                           free                                                 */     /* OMITTED:  returns void */
/* MVBITS                               mvbits_long                               */     /* OMITTED:  returns void */
/*                                      mvbits_byte                               */     /* OMITTED:  returns void */
/*                                      mvbits_short                              */     /* OMITTED:  returns void */
/*                                      mvbits_long_long                          */     /* OMITTED:  returns void */
/* CIS                       __rcis     r_cis                                     */     struct _cpx_float         intr_wrap_626 (float p1)  { return __rcis(p1); }
/* DCIS                      __dcis     d_cis                                     */     struct _cpx_double        intr_wrap_627 (double p1)  { return __dcis(p1); }
/* QCIS                      __qcis     q_cis                                     */     struct _cpx_long_double   intr_wrap_628 (long double p1)  { return __qcis(p1); }
/* DATE                                                                           */     /* not specific */
/*                                      date_vms                                  */     /* OMITTED:  returns void */
/* IDATE                                                                          */     /* not specific */
/*                                      idate_byte                                */     /* OMITTED:  returns void */
/*                                      idate_short                               */     /* OMITTED:  returns void */
/*                                      idate_long                                */     /* OMITTED:  returns void */
/*                                      idate_long_long                           */     /* OMITTED:  returns void */
/* EXIT                                                                           */     /* not specific */
/*                                      exit_noargs                               */     /* OMITTED:  returns void */
/*                                      exit_byte                                 */     /* OMITTED:  returns void */
/*                                      exit_short                                */     /* OMITTED:  returns void */
/*                                      exit_long                                 */     /* OMITTED:  returns void */
/*                                      exit_long_long                            */     /* OMITTED:  returns void */
/* SECNDS                                                                         */     /* not specific */
/*                                      secnds_vms                                */     /* OMITTED:  time call */
/*                                      dsecnds_vms                               */     /* OMITTED:  time call */
/* TIME                                                                           */     /* not specific */
/*                                      time_vms                                  */     /* OMITTED:  returns void */
/* ERRSNS                                                                         */     /* not specific */
/*                                      errsns_byte                               */     /* OMITTED:  returns void */
/*                                      errsns_short                              */     /* OMITTED:  returns void */
/*                                      errsns_long                               */     /* OMITTED:  returns void */
/*                                      errsns_long_long                          */     /* OMITTED:  returns void */
/* INTRN_DIVFLOOR                                                                   */     /* not specific */
/* INTRN_I4DIVFLOOR                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* INTRN_I8DIVFLOOR                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* INTRN_DIVCEIL                                                                   */     /* not specific */
/* INTRN_I4DIVCEIL                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* INTRN_I8DIVCEIL                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* INTRN_MODFLOOR                                                                   */     /* not specific */
/* INTRN_I4MODFLOOR                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* INTRN_I8MODFLOOR                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* INTRN_MODCEIL                                                                   */     /* not specific */
/* INTRN_I4MODCEIL                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* INTRN_I8MODCEIL                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* INTRN_SETSTACKPOINTER                                                                   */     /* not specific */
/* INTRN_U4I4SETSTACKPOINTER                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* INTRN_U8I8SETSTACKPOINTER                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* INTRN_U4READSTACKPOINTER                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* INTRN_U8READSTACKPOINTER                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* WHIRL_OPR_CEIL                                                                   */     /* not specific */
/* OPC_I4F4CEIL                                              OPC_I4F4CEIL         */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* OPC_I4F8CEIL                                              OPC_I4F8CEIL         */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* OPC_I4FQCEIL                                              OPC_I4FQCEIL         */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* OPC_I8F4CEIL                                              OPC_I8F4CEIL         */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* OPC_I8F8CEIL                                              OPC_I8F8CEIL         */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* OPC_I8FQCEIL                                              OPC_I8FQCEIL         */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* WHIRL_OPR_FLOOR                                                                   */     /* not specific */
/* OPC_I4F4FLOOR                                              OPC_I4F4FLOOR        */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* OPC_I4F8FLOOR                                              OPC_I4F8FLOOR        */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* OPC_I4FQFLOOR                                              OPC_I4FQFLOOR        */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* OPC_I8F4FLOOR                                              OPC_I8F4FLOOR        */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* OPC_I8F8FLOOR                                              OPC_I8F8FLOOR        */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* OPC_I8FQFLOOR                                              OPC_I8FQFLOOR        */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* ADD_AND_FETCH                                                                   */     /* not specific */
/* ADD_AND_FETCH_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* ADD_AND_FETCH_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* ADD_AND_FETCH_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* ADD_AND_FETCH_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* SUB_AND_FETCH                                                                   */     /* not specific */
/* SUB_AND_FETCH_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* SUB_AND_FETCH_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* SUB_AND_FETCH_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* SUB_AND_FETCH_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* OR_AND_FETCH                                                                   */     /* not specific */
/* OR_AND_FETCH_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* OR_AND_FETCH_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* OR_AND_FETCH_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* OR_AND_FETCH_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* XOR_AND_FETCH                                                                   */     /* not specific */
/* XOR_AND_FETCH_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* XOR_AND_FETCH_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* XOR_AND_FETCH_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* XOR_AND_FETCH_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* AND_AND_FETCH                                                                   */     /* not specific */
/* AND_AND_FETCH_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* AND_AND_FETCH_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* AND_AND_FETCH_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* AND_AND_FETCH_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* NAND_AND_FETCH                                                                   */     /* not specific */
/* NAND_AND_FETCH_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* NAND_AND_FETCH_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* NAND_AND_FETCH_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* NAND_AND_FETCH_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* MPY_AND_FETCH                                                                   */     /* not specific */
/* MPY_AND_FETCH_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* MPY_AND_FETCH_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* MPY_AND_FETCH_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* MPY_AND_FETCH_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* MIN_AND_FETCH                                                                   */     /* not specific */
/* MIN_AND_FETCH_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* MIN_AND_FETCH_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* MIN_AND_FETCH_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* MIN_AND_FETCH_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* MAX_AND_FETCH                                                                   */     /* not specific */
/* MAX_AND_FETCH_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* MAX_AND_FETCH_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* MAX_AND_FETCH_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* MAX_AND_FETCH_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_ADD                                                                   */     /* not specific */
/* FETCH_AND_ADD_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_ADD_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_ADD_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_ADD_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_SUB                                                                   */     /* not specific */
/* FETCH_AND_SUB_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_SUB_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_SUB_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_SUB_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_OR                                                                   */     /* not specific */
/* FETCH_AND_OR_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_OR_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_OR_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_OR_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_XOR                                                                   */     /* not specific */
/* FETCH_AND_XOR_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_XOR_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_XOR_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_XOR_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_AND                                                                   */     /* not specific */
/* FETCH_AND_AND_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_AND_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_AND_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_AND_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_NAND                                                                   */     /* not specific */
/* FETCH_AND_NAND_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_NAND_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_NAND_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_NAND_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_MPY                                                                   */     /* not specific */
/* FETCH_AND_MPY_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_MPY_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_MPY_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_MPY_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_MIN                                                                   */     /* not specific */
/* FETCH_AND_MIN_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_MIN_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_MIN_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_MIN_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_MAX                                                                   */     /* not specific */
/* FETCH_AND_MAX_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_MAX_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_MAX_F4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* FETCH_AND_MAX_F8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* LOCK_TEST_AND_SET                                                                   */     /* not specific */
/* LOCK_TEST_AND_SET_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* LOCK_TEST_AND_SET_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* LOCK_RELEASE                                                                   */     /* not specific */
/* LOCK_RELEASE_I4                                                                   */     /* OMITTED:  returns void */
/* LOCK_RELEASE_I8                                                                   */     /* OMITTED:  returns void */
/* COMPARE_AND_SWAP                                                                   */     /* not specific */
/* COMPARE_AND_SWAP_I4                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* COMPARE_AND_SWAP_I8                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* SYNCHRONIZE                                                                    */     /* OMITTED:  returns void */
/* GET_RETURN_ADDRESS                                                                   */     /* IMPOSSIBLE:  not inlined, no library routines given */
/* DSM_NUMTHREADS              dsm_numthreads                                           */     /* NOT DONE:  takes/returns tco_any type */
/* DSM_CHUNKSIZE              dsm_chunksize                                           */     /* NOT DONE:  takes/returns tco_any type */
/* DSM_THIS_CHUNKSIZE              dsm_this_chunksize                                           */     /* NOT DONE:  takes/returns tco_any type */
/* DSM_REM_CHUNKSIZE              dsm_rem_chunksize                                           */     /* NOT DONE:  takes/returns tco_any type */
/* DSM_NUMCHUNKS              dsm_numchunks                                           */     /* NOT DONE:  takes/returns tco_any type */
/* DSM_THIS_THREADNUM              dsm_this_threadnum                                           */     /* NOT DONE:  takes/returns tco_any type */
/* DSM_DISTRIBUTION_BLOCK              dsm_distribution_block                                           */     /* NOT DONE:  takes/returns tco_any type */
/* DSM_DISTRIBUTION_STAR              dsm_distribution_star                                           */     /* NOT DONE:  takes/returns tco_any type */
/* DSM_ISRESHAPED              dsm_isreshaped                                           */     /* NOT DONE:  takes/returns tco_any type */
/* DSM_ISDISTRIBUTED              dsm_isdistributed                                           */     /* NOT DONE:  takes/returns tco_any type */
/* DSM_THIS_STARTINGINDEX              dsm_this_startingindex                                           */     /* NOT DONE:  takes/returns tco_any type */
/* DSM_DISTRIBUTION_CYCLIC              dsm_distribution_cyclic                                           */     /* NOT DONE:  takes/returns tco_any type */
/* LOCK_ACQUIRE                                                                   */     /* not specific */
/* LOCK_ACQUIRE_I4                                                                   */     /* OMITTED:  returns void */
/* LOCK_ACQUIRE_I8                                                                   */     /* OMITTED:  returns void */
/* OMP_TEST_LOCK                         omp_test_lock_                            */     /* NOT DONE:  takes/returns tco_any type */
/* OMP_GET_NUM_THREADS              omp_get_num_threads_                                           */     /* NOT DONE:  takes/returns tco_any type */
/* OMP_GET_MAX_THREADS              omp_get_max_threads_                                           */     /* NOT DONE:  takes/returns tco_any type */
/* OMP_GET_THREAD_NUM              omp_get_thread_num_                                           */     /* NOT DONE:  takes/returns tco_any type */
/* OMP_GET_NUM_PROCS              omp_get_num_procs_                                           */     /* NOT DONE:  takes/returns tco_any type */
/* OMP_IN_PARALLEL              omp_in_parallel_                                           */     /* NOT DONE:  takes/returns tco_any type */
/* OMP_GET_DYNAMIC              omp_get_dynamic_                                           */     /* NOT DONE:  takes/returns tco_any type */
/* OMP_GET_NESTED              omp_get_nested_                                           */     /* NOT DONE:  takes/returns tco_any type */


void *intr_wrap[] = {
  intr_wrap_000,  intr_wrap_001,  intr_wrap_002,  intr_wrap_003,  
  intr_wrap_004,  intr_wrap_005,  intr_wrap_006,  intr_wrap_007,  
  intr_wrap_008,  intr_wrap_009,  intr_wrap_010,  intr_wrap_011,  
  intr_wrap_012,  intr_wrap_013,  intr_wrap_014,  intr_wrap_015,  
  intr_wrap_016,  intr_wrap_017,  intr_wrap_018,  intr_wrap_019,  
  intr_wrap_020,  intr_wrap_021,  intr_wrap_022,  intr_wrap_023,  
  intr_wrap_024,  intr_wrap_025,  intr_wrap_026,  intr_wrap_027,  
  intr_wrap_028,  intr_wrap_029,  intr_wrap_030,  intr_wrap_031,  
  intr_wrap_032,  intr_wrap_033,  intr_wrap_034,  intr_wrap_035,  
  intr_wrap_036,  intr_wrap_037,  intr_wrap_038,  intr_wrap_039,  
  intr_wrap_040,  intr_wrap_041,  intr_wrap_042,  intr_wrap_043,  
  intr_wrap_044,  intr_wrap_045,  NULL,           intr_wrap_047,  
  intr_wrap_048,  intr_wrap_049,  intr_wrap_050,  intr_wrap_051,  
  intr_wrap_052,  intr_wrap_053,  NULL,           intr_wrap_055,  
  intr_wrap_056,  intr_wrap_057,  intr_wrap_058,  intr_wrap_059,  
  intr_wrap_060,  intr_wrap_061,  intr_wrap_062,  intr_wrap_063,  
  intr_wrap_064,  intr_wrap_065,  intr_wrap_066,  intr_wrap_067,  
  intr_wrap_068,  intr_wrap_069,  intr_wrap_070,  intr_wrap_071,  
  intr_wrap_072,  intr_wrap_073,  intr_wrap_074,  intr_wrap_075,  
  intr_wrap_076,  intr_wrap_077,  intr_wrap_078,  intr_wrap_079,  
  intr_wrap_080,  intr_wrap_081,  intr_wrap_082,  intr_wrap_083,  
  intr_wrap_084,  intr_wrap_085,  intr_wrap_086,  intr_wrap_087,  
  intr_wrap_088,  intr_wrap_089,  intr_wrap_090,  intr_wrap_091,  
  intr_wrap_092,  intr_wrap_093,  intr_wrap_094,  intr_wrap_095,  
  intr_wrap_096,  intr_wrap_097,  intr_wrap_098,  intr_wrap_099,  
  intr_wrap_100,  intr_wrap_101,  intr_wrap_102,  intr_wrap_103,  
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           intr_wrap_115,  
  intr_wrap_116,  intr_wrap_117,  NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           intr_wrap_162,  intr_wrap_163,  
  intr_wrap_164,  intr_wrap_165,  intr_wrap_166,  intr_wrap_167,  
  intr_wrap_168,  intr_wrap_169,  intr_wrap_170,  intr_wrap_171,  
  intr_wrap_172,  intr_wrap_173,  intr_wrap_174,  intr_wrap_175,  
  intr_wrap_176,  NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  intr_wrap_208,  intr_wrap_209,  NULL,           intr_wrap_211,  
  intr_wrap_212,  NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           intr_wrap_327,  
  intr_wrap_328,  intr_wrap_329,  NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           intr_wrap_382,  intr_wrap_383,  
  intr_wrap_384,  intr_wrap_385,  intr_wrap_386,  intr_wrap_387,  
  intr_wrap_388,  intr_wrap_389,  intr_wrap_390,  NULL,           
  intr_wrap_392,  intr_wrap_393,  intr_wrap_394,  NULL,           
  intr_wrap_396,  intr_wrap_397,  intr_wrap_398,  NULL,           
  intr_wrap_400,  intr_wrap_401,  intr_wrap_402,  intr_wrap_403,  
  intr_wrap_404,  intr_wrap_405,  NULL,           intr_wrap_407,  
  intr_wrap_408,  intr_wrap_409,  NULL,           intr_wrap_411,  
  intr_wrap_412,  intr_wrap_413,  NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           intr_wrap_458,  intr_wrap_459,  
  intr_wrap_460,  intr_wrap_461,  intr_wrap_462,  intr_wrap_463,  
  intr_wrap_464,  intr_wrap_465,  intr_wrap_466,  intr_wrap_467,  
  intr_wrap_468,  intr_wrap_469,  intr_wrap_470,  NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           intr_wrap_502,  intr_wrap_503,  
  intr_wrap_504,  intr_wrap_505,  NULL,           intr_wrap_507,  
  intr_wrap_508,  intr_wrap_509,  intr_wrap_510,  NULL,           
  intr_wrap_512,  intr_wrap_513,  intr_wrap_514,  intr_wrap_515,  
  NULL,           intr_wrap_517,  intr_wrap_518,  intr_wrap_519,  
  intr_wrap_520,  NULL,           intr_wrap_522,  intr_wrap_523,  
  intr_wrap_524,  intr_wrap_525,  NULL,           intr_wrap_527,  
  intr_wrap_528,  intr_wrap_529,  intr_wrap_530,  NULL,           
  intr_wrap_532,  intr_wrap_533,  intr_wrap_534,  intr_wrap_535,  
  NULL,           intr_wrap_537,  intr_wrap_538,  intr_wrap_539,  
  intr_wrap_540,  NULL,           intr_wrap_542,  intr_wrap_543,  
  intr_wrap_544,  intr_wrap_545,  NULL,           intr_wrap_547,  
  intr_wrap_548,  intr_wrap_549,  intr_wrap_550,  NULL,           
  intr_wrap_552,  intr_wrap_553,  intr_wrap_554,  intr_wrap_555,  
  NULL,           intr_wrap_557,  intr_wrap_558,  intr_wrap_559,  
  intr_wrap_560,  NULL,           intr_wrap_562,  intr_wrap_563,  
  intr_wrap_564,  intr_wrap_565,  NULL,           intr_wrap_567,  
  intr_wrap_568,  intr_wrap_569,  intr_wrap_570,  NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           intr_wrap_577,  intr_wrap_578,  intr_wrap_579,  
  intr_wrap_580,  NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           intr_wrap_599,  
  NULL,           intr_wrap_601,  intr_wrap_602,  intr_wrap_603,  
  intr_wrap_604,  NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           intr_wrap_626,  intr_wrap_627,  
  intr_wrap_628,  NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL,           NULL,           
  NULL,           NULL,           NULL            
};


void convert_constant_EDG_to_targ(a_constant_ptr         EDG_value,
                                  void                   *targ_value,
                                  an_intrinsic_type_code tco)
{
#define CONVERT_INT(type)     *((type *)targ_value) = (type) EDG_value->variant.integer_value
#define CONVERT_REAL(type) \
  (void)memcpy((char *)targ_value, (char *)&EDG_value->variant.float_value.bytes, sizeof(type))
#define CONVERT_COMPLEX(type) \
  (void)memcpy((char *)targ_value, (char *)&EDG_value->variant.complex_value->real.bytes, sizeof(type)); \
  (void)memcpy(((char *)targ_value)+sizeof(type), (char *)&EDG_value->variant.complex_value->imag.bytes, sizeof(type))

  switch (tco) {
    case tco_logical1:
    case tco_integer1:     CONVERT_INT(signed char);      break;
    case tco_logical2:
    case tco_integer2:     CONVERT_INT(short);            break;
    case tco_logical4:
    case tco_integer4:     CONVERT_INT(int);              break;
    case tco_logical8:
    case tco_integer8:     CONVERT_INT(long long);        break;
    case tco_real4:        CONVERT_REAL(float);           break;
    case tco_real8:        CONVERT_REAL(double);          break;
    case tco_real16:       CONVERT_REAL(long double);     break;
    case tco_complex8:     CONVERT_COMPLEX(float);        break;
    case tco_complex16:    CONVERT_COMPLEX(double);       break;
    case tco_complex32:    CONVERT_COMPLEX(long double);  break;
#if CHECKING
    default:
      internal_error("bad case in convert_constant_EDG_to_targ");
      break;
#endif /* if CHECKING */
  }  /* switch */

#undef CONVERT_INT
#undef CONVERT_REAL
#undef CONVERT_COMPLEX
}  /* convert_constant_EDG_to_targ */


void convert_constant_targ_to_EDG(void                   *targ_value,
                                  a_constant_ptr         EDG_value,
                                  an_intrinsic_type_code tco)
{
#define CONVERT_INT(type)     EDG_value->variant.integer_value = (an_integer_value) *((type *)targ_value)
#define CONVERT_REAL(type) \
  (void)memcpy((char *)&EDG_value->variant.float_value.bytes, (char *)targ_value, sizeof(type)); \
  EDG_value->variant.float_value.float_string = NULL
#define CONVERT_COMPLEX(type) \
  (void)memcpy((char *)&EDG_value->variant.complex_value->real, (char *)targ_value, sizeof(type)); \
  EDG_value->variant.complex_value->real.float_string = NULL; \
  (void)memcpy((char *)&EDG_value->variant.complex_value->imag, ((char *)targ_value)+sizeof(type), sizeof(type)); \
  EDG_value->variant.complex_value->imag.float_string = NULL

  switch (tco) {
    case tco_logical1:
    case tco_integer1:     CONVERT_INT(signed char);      break;
    case tco_logical2:
    case tco_integer2:     CONVERT_INT(short);            break;
    case tco_logical4:
    case tco_integer4:     CONVERT_INT(int);              break;
    case tco_logical8:
    case tco_integer8:     CONVERT_INT(long long);        break;
    case tco_real4:        CONVERT_REAL(float);           break;
    case tco_real8:        CONVERT_REAL(double);          break;
    case tco_real16:       CONVERT_REAL(long double);     break;
    case tco_complex8:     CONVERT_COMPLEX(float);        break;
    case tco_complex16:    CONVERT_COMPLEX(double);       break;
    case tco_complex32:    CONVERT_COMPLEX(long double);  break;
#if CHECKING
    default:
      internal_error("bad case in convert_constant_targ_to_EDG");
      break;
#endif /* if CHECKING */
  }  /* switch */

#undef CONVERT_INT
#undef CONVERT_REAL
#undef CONVERT_COMPLEX
}  /* convert_constant_targ_to_EDG */



a_boolean fold_general_exponentiation(an_expr_operator_kind op,
                                      an_fe_constant_ptr    constant_1,
                                      an_fe_constant_ptr    constant_2,
                                      a_type_ptr            result_type,
                                      an_fe_constant_ptr    result,
                                      an_error_code         *err_code,
                                      an_error_severity     *err_severity)
{
  a_boolean success = FALSE;

  if (is_real4_type(result_type)) {
    float targ_arguments[2], targ_result;
    convert_constant_EDG_to_targ(&constant_1->constant, (void *)&targ_arguments[0], tco_real4);
    convert_constant_EDG_to_targ(&constant_2->constant, (void *)&targ_arguments[1], tco_real4);
    targ_result = powf(targ_arguments[0], targ_arguments[1]);
    clear_fe_constant(result, (a_constant_repr_kind)ck_float);
    convert_constant_targ_to_EDG((void *)&targ_result, &result->constant, tco_real4);
    result->constant.type = result_type;
    if (strict_f77_mode) {
      *err_code = ec_noninteger_exponent_in_constant_expr_nonstandard;
      *err_severity = es_warning;
    }
    success = TRUE;
  } else if (is_real8_type(result_type)) {
    double targ_arguments[2], targ_result;
    convert_constant_EDG_to_targ(&constant_1->constant, (void *)&targ_arguments[0], tco_real8);
    convert_constant_EDG_to_targ(&constant_2->constant, (void *)&targ_arguments[1], tco_real8);
    targ_result = pow(targ_arguments[0], targ_arguments[1]);
    clear_fe_constant(result, (a_constant_repr_kind)ck_float);
    convert_constant_targ_to_EDG((void *)&targ_result, &result->constant, tco_real8);
    result->constant.type = result_type;
    if (strict_f77_mode) {
      *err_code = ec_noninteger_exponent_in_constant_expr_nonstandard;
      *err_severity = es_warning;
    }
    success = TRUE;
  } else if (is_real16_type(result_type)) {
    long double targ_arguments[2], targ_result;
    convert_constant_EDG_to_targ(&constant_1->constant, (void *)&targ_arguments[0], tco_real16);
    convert_constant_EDG_to_targ(&constant_2->constant, (void *)&targ_arguments[1], tco_real16);
    targ_result = qpow(targ_arguments[0], targ_arguments[1]);
    clear_fe_constant(result, (a_constant_repr_kind)ck_float);
    convert_constant_targ_to_EDG((void *)&targ_result, &result->constant, tco_real16);
    result->constant.type = result_type;
    if (strict_f77_mode) {
      *err_code = ec_noninteger_exponent_in_constant_expr_nonstandard;
      *err_severity = es_warning;
    }
    success = TRUE;
  } else if (is_complex8_type(result_type)) {
    struct _cpx_float targ_arguments[2], targ_result;
    convert_constant_EDG_to_targ(&constant_1->constant, (void *)&targ_arguments[0], tco_complex8);
    convert_constant_EDG_to_targ(&constant_2->constant, (void *)&targ_arguments[1], tco_complex8);
    targ_result = __powcc(targ_arguments[0].real, targ_arguments[0].imag, targ_arguments[1].real, targ_arguments[1].imag);
    clear_fe_constant(result, (a_constant_repr_kind)ck_complex);
    convert_constant_targ_to_EDG((void *)&targ_result, &result->constant, tco_complex8);
    result->constant.type = result_type;
    if (strict_f77_mode) {
      *err_code = ec_noninteger_exponent_in_constant_expr_nonstandard;
      *err_severity = es_warning;
    }
    success = TRUE;
  } else if (is_complex16_type(result_type)) {
    struct _cpx_double targ_arguments[2], targ_result;
    convert_constant_EDG_to_targ(&constant_1->constant, (void *)&targ_arguments[0], tco_complex16);
    convert_constant_EDG_to_targ(&constant_2->constant, (void *)&targ_arguments[1], tco_complex16);
    targ_result = __powzz(targ_arguments[0].dreal, targ_arguments[0].dimag, targ_arguments[1].dreal, targ_arguments[1].dimag);
    clear_fe_constant(result, (a_constant_repr_kind)ck_complex);
    convert_constant_targ_to_EDG((void *)&targ_result, &result->constant, tco_complex16);
    result->constant.type = result_type;
    if (strict_f77_mode) {
      *err_code = ec_noninteger_exponent_in_constant_expr_nonstandard;
      *err_severity = es_warning;
    }
    success = TRUE;
  } else if (is_complex32_type(result_type)) {
    struct _cpx_long_double targ_arguments[2], targ_result;
    convert_constant_EDG_to_targ(&constant_1->constant, (void *)&targ_arguments[0], tco_complex32);
    convert_constant_EDG_to_targ(&constant_2->constant, (void *)&targ_arguments[1], tco_complex32);
    targ_result = __cqpow(targ_arguments[0].qreal, targ_arguments[0].qimag, targ_arguments[1].qreal, targ_arguments[1].qimag);
    clear_fe_constant(result, (a_constant_repr_kind)ck_complex);
    convert_constant_targ_to_EDG((void *)&targ_result, &result->constant, tco_complex32);
    result->constant.type = result_type;
    if (strict_f77_mode) {
      *err_code = ec_noninteger_exponent_in_constant_expr_nonstandard;
      *err_severity = es_warning;
    }
    success = TRUE;
#if CHECKING
  } else {
    internal_error("bad case in fold_general_exponentiation");
#endif /* if CHECKING */
  }

  return success;
}  /* fold_general_exponentiation */



a_boolean perform_constant_intrinsic_call(
                                an_fe_constant_ptr           result_constant,
                                an_intr_func_description_ptr ifdp,
                                an_expr_node_ptr             arguments)
{
  void *wrapper_void_ptr = intr_wrap[ifdp-intr_func_info];
  if (wrapper_void_ptr == NULL) {
    return FALSE;
  }  /* if */
  switch (ifdp->arg_type) {
    case tco_integer1: {
      switch (ifdp->return_type) {
        case tco_integer1: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          signed char targ_arguments[5];
          signed char targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_integer1, ret=tco_integer1\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_integer1);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %hd\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef signed char (*wrapper_type)(signed char);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
            case 2: {
              typedef signed char (*wrapper_type)(signed char, signed char);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1]);
              break;
            }
            case 3: {
              typedef signed char (*wrapper_type)(signed char, signed char, signed char);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1], targ_arguments[2]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %hd\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer1);
          break;
        }
        case tco_logical1: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          signed char targ_arguments[5];
          signed char targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_integer1, ret=tco_logical1\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_integer1);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %hd\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 2: {
              typedef signed char (*wrapper_type)(signed char, signed char);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %hd\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_logical1);
          break;
        }
#if CHECKING
        default:
          internal_error("bad case in do_compile_time_constant_intrinsic_call");
          break;
#endif /* CHECKING */
      }
      break;
    }
    case tco_integer2: {
      switch (ifdp->return_type) {
        case tco_integer2: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          short targ_arguments[5];
          short targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_integer2, ret=tco_integer2\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_integer2);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %hd\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef short (*wrapper_type)(short);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
            case 2: {
              typedef short (*wrapper_type)(short, short);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1]);
              break;
            }
            case 3: {
              typedef short (*wrapper_type)(short, short, short);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1], targ_arguments[2]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %hd\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer2);
          break;
        }
        case tco_logical2: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          short targ_arguments[5];
          short targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_integer2, ret=tco_logical2\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_integer2);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %hd\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 2: {
              typedef short (*wrapper_type)(short, short);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %hd\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_logical2);
          break;
        }
#if CHECKING
        default:
          internal_error("bad case in do_compile_time_constant_intrinsic_call");
          break;
#endif /* CHECKING */
      }
      break;
    }
    case tco_integer4: {
      switch (ifdp->return_type) {
        case tco_integer4: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          int targ_arguments[5];
          int targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_integer4, ret=tco_integer4\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_integer4);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %d\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef int (*wrapper_type)(int);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
            case 2: {
              typedef int (*wrapper_type)(int, int);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1]);
              break;
            }
            case 3: {
              typedef int (*wrapper_type)(int, int, int);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1], targ_arguments[2]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %d\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer4);
          break;
        }
        case tco_logical4: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          int targ_arguments[5];
          int targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_integer4, ret=tco_logical4\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_integer4);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %d\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 2: {
              typedef int (*wrapper_type)(int, int);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %d\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_logical4);
          break;
        }
#if CHECKING
        default:
          internal_error("bad case in do_compile_time_constant_intrinsic_call");
          break;
#endif /* CHECKING */
      }
      break;
    }
    case tco_integer8: {
      switch (ifdp->return_type) {
        case tco_integer8: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          long long targ_arguments[5];
          long long targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_integer8, ret=tco_integer8\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_integer8);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %lld\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef long long (*wrapper_type)(long long);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
            case 2: {
              typedef long long (*wrapper_type)(long long, long long);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1]);
              break;
            }
            case 3: {
              typedef long long (*wrapper_type)(long long, long long, long long);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1], targ_arguments[2]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %lld\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer8);
          break;
        }
        case tco_logical8: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          long long targ_arguments[5];
          long long targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_integer8, ret=tco_logical8\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_integer8);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %lld\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 2: {
              typedef long long (*wrapper_type)(long long, long long);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %lld\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_logical8);
          break;
        }
#if CHECKING
        default:
          internal_error("bad case in do_compile_time_constant_intrinsic_call");
          break;
#endif /* CHECKING */
      }
      break;
    }
    case tco_real4: {
      switch (ifdp->return_type) {
        case tco_integer2: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          float targ_arguments[5];
          short targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real4, ret=tco_integer2\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real4);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %lf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef short (*wrapper_type)(float);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %hd\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer2);
          break;
        }
        case tco_integer4: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          float targ_arguments[5];
          int targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real4, ret=tco_integer4\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real4);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %lf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef int (*wrapper_type)(float);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %d\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer4);
          break;
        }
        case tco_integer8: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          float targ_arguments[5];
          long long targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real4, ret=tco_integer8\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real4);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %lf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef long long (*wrapper_type)(float);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %lld\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer8);
          break;
        }
        case tco_real4: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          float targ_arguments[5];
          float targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real4, ret=tco_real4\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real4);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %lf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef float (*wrapper_type)(float);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
            case 2: {
              typedef float (*wrapper_type)(float, float);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %lf\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_float);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_real4);
          break;
        }
        case tco_real8: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          float targ_arguments[5];
          double targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real4, ret=tco_real8\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real4);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %lf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 2: {
              typedef double (*wrapper_type)(float, float);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %lf\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_float);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_real8);
          break;
        }
        case tco_complex8: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          float targ_arguments[5];
          struct _cpx_float targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real4, ret=tco_complex8\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real4);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %lf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef struct _cpx_float (*wrapper_type)(float);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is (%lf,%lf)\n", targ_result.real, targ_result.imag);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_complex);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_complex8);
          break;
        }
#if CHECKING
        default:
          internal_error("bad case in do_compile_time_constant_intrinsic_call");
          break;
#endif /* CHECKING */
      }
      break;
    }
    case tco_real8: {
      switch (ifdp->return_type) {
        case tco_integer2: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          double targ_arguments[5];
          short targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real8, ret=tco_integer2\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real8);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %lf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef short (*wrapper_type)(double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %hd\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer2);
          break;
        }
        case tco_integer4: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          double targ_arguments[5];
          int targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real8, ret=tco_integer4\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real8);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %lf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef int (*wrapper_type)(double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %d\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer4);
          break;
        }
        case tco_integer8: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          double targ_arguments[5];
          long long targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real8, ret=tco_integer8\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real8);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %lf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef long long (*wrapper_type)(double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %lld\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer8);
          break;
        }
        case tco_real8: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          double targ_arguments[5];
          double targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real8, ret=tco_real8\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real8);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %lf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef double (*wrapper_type)(double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
            case 2: {
              typedef double (*wrapper_type)(double, double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %lf\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_float);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_real8);
          break;
        }
        case tco_real16: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          double targ_arguments[5];
          long double targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real8, ret=tco_real16\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real8);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %lf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 2: {
              typedef long double (*wrapper_type)(double, double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %llf\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_float);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_real16);
          break;
        }
        case tco_complex16: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          double targ_arguments[5];
          struct _cpx_double targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real8, ret=tco_complex16\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real8);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %lf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef struct _cpx_double (*wrapper_type)(double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is (%lf,%lf)\n", targ_result.dreal, targ_result.dimag);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_complex);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_complex16);
          break;
        }
#if CHECKING
        default:
          internal_error("bad case in do_compile_time_constant_intrinsic_call");
          break;
#endif /* CHECKING */
      }
      break;
    }
    case tco_real16: {
      switch (ifdp->return_type) {
        case tco_integer2: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          long double targ_arguments[5];
          short targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real16, ret=tco_integer2\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real16);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %llf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef short (*wrapper_type)(long double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %hd\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer2);
          break;
        }
        case tco_integer4: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          long double targ_arguments[5];
          int targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real16, ret=tco_integer4\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real16);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %llf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef int (*wrapper_type)(long double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %d\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer4);
          break;
        }
        case tco_integer8: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          long double targ_arguments[5];
          long long targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real16, ret=tco_integer8\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real16);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %llf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef long long (*wrapper_type)(long double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %lld\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer8);
          break;
        }
        case tco_real16: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          long double targ_arguments[5];
          long double targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real16, ret=tco_real16\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real16);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %llf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef long double (*wrapper_type)(long double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
            case 2: {
              typedef long double (*wrapper_type)(long double, long double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %llf\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_float);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_real16);
          break;
        }
        case tco_complex32: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          long double targ_arguments[5];
          struct _cpx_long_double targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_real16, ret=tco_complex32\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_real16);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is %llf\n", num_args, targ_arguments[num_args]);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef struct _cpx_long_double (*wrapper_type)(long double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is (%llf,%llf)\n", targ_result.qreal, targ_result.qimag);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_complex);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_complex32);
          break;
        }
#if CHECKING
        default:
          internal_error("bad case in do_compile_time_constant_intrinsic_call");
          break;
#endif /* CHECKING */
      }
      break;
    }
    case tco_complex8: {
      switch (ifdp->return_type) {
        case tco_real4: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          struct _cpx_float targ_arguments[5];
          float targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_complex8, ret=tco_real4\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_complex8);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is (%lf,%lf)\n", num_args, targ_arguments[num_args].real, targ_arguments[num_args].imag);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef float (*wrapper_type)(struct _cpx_float);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %lf\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_float);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_real4);
          break;
        }
        case tco_real8: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          struct _cpx_float targ_arguments[5];
          double targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_complex8, ret=tco_real8\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_complex8);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is (%lf,%lf)\n", num_args, targ_arguments[num_args].real, targ_arguments[num_args].imag);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef double (*wrapper_type)(struct _cpx_float);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %lf\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_float);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_real8);
          break;
        }
        case tco_complex8: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          struct _cpx_float targ_arguments[5];
          struct _cpx_float targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_complex8, ret=tco_complex8\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_complex8);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is (%lf,%lf)\n", num_args, targ_arguments[num_args].real, targ_arguments[num_args].imag);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef struct _cpx_float (*wrapper_type)(struct _cpx_float);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is (%lf,%lf)\n", targ_result.real, targ_result.imag);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_complex);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_complex8);
          break;
        }
#if CHECKING
        default:
          internal_error("bad case in do_compile_time_constant_intrinsic_call");
          break;
#endif /* CHECKING */
      }
      break;
    }
    case tco_complex16: {
      switch (ifdp->return_type) {
        case tco_real8: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          struct _cpx_double targ_arguments[5];
          double targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_complex16, ret=tco_real8\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_complex16);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is (%lf,%lf)\n", num_args, targ_arguments[num_args].dreal, targ_arguments[num_args].dimag);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef double (*wrapper_type)(struct _cpx_double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %lf\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_float);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_real8);
          break;
        }
        case tco_complex16: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          struct _cpx_double targ_arguments[5];
          struct _cpx_double targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_complex16, ret=tco_complex16\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_complex16);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is (%lf,%lf)\n", num_args, targ_arguments[num_args].dreal, targ_arguments[num_args].dimag);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef struct _cpx_double (*wrapper_type)(struct _cpx_double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is (%lf,%lf)\n", targ_result.dreal, targ_result.dimag);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_complex);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_complex16);
          break;
        }
#if CHECKING
        default:
          internal_error("bad case in do_compile_time_constant_intrinsic_call");
          break;
#endif /* CHECKING */
      }
      break;
    }
    case tco_complex32: {
      switch (ifdp->return_type) {
        case tco_real16: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          struct _cpx_long_double targ_arguments[5];
          long double targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_complex32, ret=tco_real16\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_complex32);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is (%llf,%llf)\n", num_args, targ_arguments[num_args].qreal, targ_arguments[num_args].qimag);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef long double (*wrapper_type)(struct _cpx_long_double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %llf\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_float);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_real16);
          break;
        }
        case tco_complex32: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          struct _cpx_long_double targ_arguments[5];
          struct _cpx_long_double targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_complex32, ret=tco_complex32\n");
#endif
          while (enp != NULL) {
            convert_constant_EDG_to_targ(enp->variant.constant, (void *)&targ_arguments[num_args], tco_complex32);
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is (%llf,%llf)\n", num_args, targ_arguments[num_args].qreal, targ_arguments[num_args].qimag);
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 1: {
              typedef struct _cpx_long_double (*wrapper_type)(struct _cpx_long_double);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is (%llf,%llf)\n", targ_result.qreal, targ_result.qimag);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_complex);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_complex32);
          break;
        }
#if CHECKING
        default:
          internal_error("bad case in do_compile_time_constant_intrinsic_call");
          break;
#endif /* CHECKING */
      }
      break;
    }
    case tco_character: {
      switch (ifdp->return_type) {
        case tco_integer1: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          char *targ_arguments[5];
          int targ_lengths[5];
          int i;
          signed char targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_character, ret=tco_integer1\n");
#endif
          while (enp != NULL) {
            targ_arguments[num_args] = enp->variant.operation.operands->variant.constant->variant.string.value;
            targ_lengths[num_args] = enp->variant.operation.operands->variant.constant->variant.string.length;
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is '", num_args);
            for (i=0;i<targ_lengths[num_args];i++)  putc(targ_arguments[num_args][i], stderr);
            fprintf(stderr, "'\n");
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 2: {
              typedef signed char (*wrapper_type)(char *, char *, int, int);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1], targ_lengths[0], targ_lengths[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %hd\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer1);
          break;
        }
        case tco_integer2: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          char *targ_arguments[5];
          int targ_lengths[5];
          int i;
          short targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_character, ret=tco_integer2\n");
#endif
          while (enp != NULL) {
            targ_arguments[num_args] = enp->variant.operation.operands->variant.constant->variant.string.value;
            targ_lengths[num_args] = enp->variant.operation.operands->variant.constant->variant.string.length;
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is '", num_args);
            for (i=0;i<targ_lengths[num_args];i++)  putc(targ_arguments[num_args][i], stderr);
            fprintf(stderr, "'\n");
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 2: {
              typedef short (*wrapper_type)(char *, char *, int, int);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1], targ_lengths[0], targ_lengths[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %hd\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer2);
          break;
        }
        case tco_integer4: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          char *targ_arguments[5];
          int targ_lengths[5];
          int i;
          int targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_character, ret=tco_integer4\n");
#endif
          while (enp != NULL) {
            targ_arguments[num_args] = enp->variant.operation.operands->variant.constant->variant.string.value;
            targ_lengths[num_args] = enp->variant.operation.operands->variant.constant->variant.string.length;
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is '", num_args);
            for (i=0;i<targ_lengths[num_args];i++)  putc(targ_arguments[num_args][i], stderr);
            fprintf(stderr, "'\n");
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 2: {
              typedef int (*wrapper_type)(char *, char *, int, int);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1], targ_lengths[0], targ_lengths[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %d\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer4);
          break;
        }
        case tco_integer8: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          char *targ_arguments[5];
          int targ_lengths[5];
          int i;
          long long targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_character, ret=tco_integer8\n");
#endif
          while (enp != NULL) {
            targ_arguments[num_args] = enp->variant.operation.operands->variant.constant->variant.string.value;
            targ_lengths[num_args] = enp->variant.operation.operands->variant.constant->variant.string.length;
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is '", num_args);
            for (i=0;i<targ_lengths[num_args];i++)  putc(targ_arguments[num_args][i], stderr);
            fprintf(stderr, "'\n");
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 2: {
              typedef long long (*wrapper_type)(char *, char *, int, int);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1], targ_lengths[0], targ_lengths[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %lld\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_integer8);
          break;
        }
        case tco_logical1: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          char *targ_arguments[5];
          int targ_lengths[5];
          int i;
          signed char targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_character, ret=tco_logical1\n");
#endif
          while (enp != NULL) {
            targ_arguments[num_args] = enp->variant.operation.operands->variant.constant->variant.string.value;
            targ_lengths[num_args] = enp->variant.operation.operands->variant.constant->variant.string.length;
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is '", num_args);
            for (i=0;i<targ_lengths[num_args];i++)  putc(targ_arguments[num_args][i], stderr);
            fprintf(stderr, "'\n");
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 2: {
              typedef signed char (*wrapper_type)(char *, char *, int, int);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1], targ_lengths[0], targ_lengths[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %hd\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_logical1);
          break;
        }
        case tco_logical2: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          char *targ_arguments[5];
          int targ_lengths[5];
          int i;
          short targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_character, ret=tco_logical2\n");
#endif
          while (enp != NULL) {
            targ_arguments[num_args] = enp->variant.operation.operands->variant.constant->variant.string.value;
            targ_lengths[num_args] = enp->variant.operation.operands->variant.constant->variant.string.length;
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is '", num_args);
            for (i=0;i<targ_lengths[num_args];i++)  putc(targ_arguments[num_args][i], stderr);
            fprintf(stderr, "'\n");
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 2: {
              typedef short (*wrapper_type)(char *, char *, int, int);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1], targ_lengths[0], targ_lengths[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %hd\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_logical2);
          break;
        }
        case tco_logical4: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          char *targ_arguments[5];
          int targ_lengths[5];
          int i;
          int targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_character, ret=tco_logical4\n");
#endif
          while (enp != NULL) {
            targ_arguments[num_args] = enp->variant.operation.operands->variant.constant->variant.string.value;
            targ_lengths[num_args] = enp->variant.operation.operands->variant.constant->variant.string.length;
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is '", num_args);
            for (i=0;i<targ_lengths[num_args];i++)  putc(targ_arguments[num_args][i], stderr);
            fprintf(stderr, "'\n");
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 2: {
              typedef int (*wrapper_type)(char *, char *, int, int);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1], targ_lengths[0], targ_lengths[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %d\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_logical4);
          break;
        }
        case tco_logical8: {
          int num_args = 0;
          an_expr_node_ptr enp = arguments;
          char *targ_arguments[5];
          int targ_lengths[5];
          int i;
          long long targ_result;
#if INTR_FOLD_DEBUG
          fprintf(stderr, "Case for arg=tco_character, ret=tco_logical8\n");
#endif
          while (enp != NULL) {
            targ_arguments[num_args] = enp->variant.operation.operands->variant.constant->variant.string.value;
            targ_lengths[num_args] = enp->variant.operation.operands->variant.constant->variant.string.length;
#if INTR_FOLD_DEBUG
            fprintf(stderr, "arg %d is '", num_args);
            for (i=0;i<targ_lengths[num_args];i++)  putc(targ_arguments[num_args][i], stderr);
            fprintf(stderr, "'\n");
#endif
            num_args++;
            enp = enp->next;
          }
          switch (num_args) {
            case 2: {
              typedef long long (*wrapper_type)(char *, char *, int, int);
              wrapper_type wrapper_func;
              wrapper_func = (wrapper_type) wrapper_void_ptr;
              targ_result = (*wrapper_func)(targ_arguments[0], targ_arguments[1], targ_lengths[0], targ_lengths[1]);
              break;
            }
#if CHECKING
            default:
              internal_error("bad case in do_compile_time_constant_intrinsic_call");
              break;
#endif /* CHECKING */
          }
#if INTR_FOLD_DEBUG
          fprintf(stderr, "result is %lld\n", targ_result);
#endif
          clear_fe_constant(result_constant, (a_constant_repr_kind)ck_integer);
          convert_constant_targ_to_EDG((void *)&targ_result, &result_constant->constant, tco_logical8);
          break;
        }
#if CHECKING
        default:
          internal_error("bad case in do_compile_time_constant_intrinsic_call");
          break;
#endif /* CHECKING */
      }
      break;
    }
    case tco_logical1: {
      switch (ifdp->return_type) {
#if CHECKING
        default:
          internal_error("bad case in do_compile_time_constant_intrinsic_call");
          break;
#endif /* CHECKING */
      }
      break;
    }
    case tco_logical2: {
      switch (ifdp->return_type) {
#if CHECKING
        default:
          internal_error("bad case in do_compile_time_constant_intrinsic_call");
          break;
#endif /* CHECKING */
      }
      break;
    }
    case tco_logical4: {
      switch (ifdp->return_type) {
#if CHECKING
        default:
          internal_error("bad case in do_compile_time_constant_intrinsic_call");
          break;
#endif /* CHECKING */
      }
      break;
    }
    case tco_logical8: {
      switch (ifdp->return_type) {
#if CHECKING
        default:
          internal_error("bad case in do_compile_time_constant_intrinsic_call");
          break;
#endif /* CHECKING */
      }
      break;
    }
#if CHECKING
    default:
      internal_error("bad case in do_compile_time_constant_intrinsic_call");
      break;
#endif /* CHECKING */
  }
  return TRUE;
}  /* perform_constant_intrinsic_call */

