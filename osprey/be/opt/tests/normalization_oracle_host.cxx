/* Copyright (C) 2026 Open64 Project */

/* Standalone LN1-d host witnesses and independent MPFR interval checks.
 * Test-only: no compiler/reduction/WHIRL linkage or generated-kernel claim.
 * Design: doc/AI-COMPILER-LAYERNORM-ARITHMETIC-ORACLE-v0.1.md. */
#include <cfenv>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>
#include <mpfr.h>
#if defined(__SSE__)
#include <xmmintrin.h>
#endif
#pragma STDC FENV_ACCESS ON

/* Reinterpret exactly one binary32 word, with no decimal/numeric conversion. */
static float LN_ORACLE_value(uint32_t bits)
{
  float result;
  std::memcpy(&result, &bits, sizeof(result));
  return result;
}

/* Preserve bits, including signed zeros; NaN payloads are not compared. */
static uint32_t LN_ORACLE_bits(float value)
{
  uint32_t result;
  std::memcpy(&result, &value, sizeof(result));
  return result;
}

/* Accept only exact words, never silently truncate an integer or decimal. */
static uint32_t LN_ORACLE_read_word(void)
{
  std::string token;
  if (!(std::cin >> token) || token.size() != 8 || token.find_first_not_of("0123456789abcdef") != std::string::npos)
    throw std::runtime_error("expected canonical binary32 hex word");
  return static_cast<uint32_t>(std::strtoul(token.c_str(), NULL, 16));
}

/* Construct MPFR inputs exactly from integer significands and powers of two,
 * including IEEE exceptional classes. Host floating input conversion is absent. */
static void LN_ORACLE_set_mpfr(mpfr_t output, uint32_t bits)
{
  unsigned exponent = (bits >> 23) & 255, fraction = bits & 0x7fffff;
  int sign = bits >> 31 ? -1 : 1;
  if (exponent == 255) {
    if (fraction) mpfr_set_nan(output);
    else mpfr_set_inf(output, sign);
  } else if (!exponent && !fraction) {
    mpfr_set_zero(output, sign);
  } else {
    mpfr_set_ui(output, exponent ? fraction | 0x800000 : fraction, MPFR_RNDN);
    mpfr_mul_2si(output, output, exponent ? static_cast<long>(exponent) - 150 : -149, MPFR_RNDN);
    if (sign < 0) mpfr_neg(output, output, MPFR_RNDN);
  }
}

/* Apply one MPFR operation under directed rounding. FMA is a counterexample
 * operation only; it never becomes an admitted LN-REF-1 instruction. */
static void LN_ORACLE_mpfr_apply(unsigned operation, mpfr_t out, const mpfr_t a,
    const mpfr_t b, const mpfr_t c, mpfr_rnd_t rounding)
{
  switch (operation) {
  case 1: mpfr_add(out, a, b, rounding); break;
  case 2: mpfr_sub(out, a, b, rounding); break;
  case 3: mpfr_mul(out, a, b, rounding); break;
  case 4: mpfr_div(out, a, b, rounding); break;
  case 5: mpfr_sqrt(out, a, rounding); break;
  case 6: mpfr_fma(out, a, b, c, rounding); break;
  default: throw std::runtime_error("unknown scalar operation");
  }
}

/* A directed interval must round at both ends to the same binary32 result.
 * Otherwise increase precision; never accept an unchecked high-precision
 * approximation as an oracle. Binary32 conversion handles subnormal rounding. */
static uint32_t LN_ORACLE_mpfr(unsigned operation, uint32_t left, uint32_t right,
    uint32_t third, unsigned *used_precision)
{
  mpfr_t a, b, c, lower, upper;
  mpfr_inits2(64, a, b, c, lower, upper, (mpfr_ptr)0);
  LN_ORACLE_set_mpfr(a, left); LN_ORACLE_set_mpfr(b, right); LN_ORACLE_set_mpfr(c, third);
  for (unsigned precision = 64; precision <= 1024; precision *= 2) {
    mpfr_set_prec(lower, precision); mpfr_set_prec(upper, precision);
    LN_ORACLE_mpfr_apply(operation, lower, a, b, c, MPFR_RNDD);
    LN_ORACLE_mpfr_apply(operation, upper, a, b, c, MPFR_RNDU);
    uint32_t low = LN_ORACLE_bits(mpfr_get_flt(lower, MPFR_RNDN));
    uint32_t high = LN_ORACLE_bits(mpfr_get_flt(upper, MPFR_RNDN));
    bool both_nan = (low & 0x7fffffff) > 0x7f800000 && (high & 0x7fffffff) > 0x7f800000;
    // Exact zero can have opposite signs under directed rounding. Redo only
    // this exact-zero operation with RNE to resolve its required zero sign.
    if (mpfr_zero_p(lower) && mpfr_zero_p(upper) && low != high) {
      LN_ORACLE_mpfr_apply(operation, lower, a, b, c, MPFR_RNDN);
      low = high = LN_ORACLE_bits(mpfr_get_flt(lower, MPFR_RNDN));
    }
    if (low == high || both_nan) {
      *used_precision = precision;
      mpfr_clears(a, b, c, lower, upper, (mpfr_ptr)0);
      return low;
    }
  }
  mpfr_clears(a, b, c, lower, upper, (mpfr_ptr)0);
  throw std::runtime_error("MPFR interval inconclusive at 1024 bits");
}

/* Separately evaluated native binary32 witness. Volatile result stores and
 * strict compiler controls preserve each rounding; this is not the oracle. */
static uint32_t LN_ORACLE_host(unsigned operation, uint32_t left, uint32_t right, uint32_t third = 0)
{
  volatile float a = LN_ORACLE_value(left), b = LN_ORACLE_value(right), c = LN_ORACLE_value(third);
  volatile float output;
  switch (operation) {
  case 1: output = a + b; break;
  case 2: output = a - b; break;
  case 3: output = a * b; break;
  case 4: output = a / b; break;
  case 5: output = std::sqrt(a); break;
  case 6: output = std::fma(a, b, c); break;
  default: throw std::runtime_error("unknown native scalar operation");
  }
  return LN_ORACLE_bits(output);
}

/* The MPFR row is an external scalar engine; C0/C1/C2 use native FP32.
 * Neither route depends on the exact-rational Python reference implementation. */
static uint32_t LN_ORACLE_eval(const std::string &engine, unsigned operation, uint32_t a, uint32_t b = 0)
{
  unsigned precision;
  return engine == "MPFR" ? LN_ORACLE_mpfr(operation, a, b, 0, &precision) : LN_ORACLE_host(operation, a, b);
}

/* Positional numerical-witness rows, distinct from native WOPT trace tags.
 * The source line names this host producer, not an input Python/WHIRL node. */
static void LN_ORACLE_emit(const std::string &id, const std::string &engine,
    const char *stage, unsigned index, uint32_t bits)
{
  std::cout << "AI_NORM_NUM 1 VALUE " << id << ' ' << engine << ' ' << stage << ' ' << index << ' '
            << std::hex << std::setw(8) << std::setfill('0') << bits << std::dec << ' ' << __LINE__ << '\n';
}

/* Exercise ordered means and three explicit host storage realizations. C2
 * overwrites a private row copy only after the whole mean and each last read;
 * this witnesses numerical equivalence, not target/runtime event legality. */
static void LN_ORACLE_row(const std::string &id, const std::string &engine)
{
  unsigned version, count;
  if (!(std::cin >> version >> count) || version != 2 || !count || count > 16777216 ||
      (engine != "C0" && engine != "C1" && engine != "C2" && engine != "MPFR"))
    throw std::runtime_error("explicit LN-REF-1 v2, legal engine and count required");
  uint32_t epsilon = LN_ORACLE_read_word();
  if (!epsilon || epsilon >= 0x7f800000) throw std::runtime_error("positive finite epsilon required");
  std::vector<uint32_t> x(count), gamma(count), beta(count), centered(count), scratch(count);
  for (unsigned i = 0; i < count; ++i) x[i] = LN_ORACLE_read_word();
  for (unsigned i = 0; i < count; ++i) gamma[i] = LN_ORACLE_read_word();
  for (unsigned i = 0; i < count; ++i) beta[i] = LN_ORACLE_read_word();
  scratch = x;
  uint32_t n32 = LN_ORACLE_bits(static_cast<float>(count));
  LN_ORACLE_emit(id, engine, "count", 0, n32);
  uint32_t state = 0;
  for (unsigned i = 0; i < count; ++i) {
    state = LN_ORACLE_eval(engine, 1, state, x[i]);
    LN_ORACLE_emit(id, engine, "sum_step", i, state);
  }
  uint32_t mu = LN_ORACLE_eval(engine, 4, state, n32);
  LN_ORACLE_emit(id, engine, "mu", 0, mu);
  state = 0;
  for (unsigned i = 0; i < count; ++i) {
    uint32_t d = LN_ORACLE_eval(engine, 2, engine == "C2" ? scratch[i] : x[i], mu);
    centered[i] = d;
    if (engine == "C2") scratch[i] = d;
    uint32_t square = LN_ORACLE_eval(engine, 3, d, d);
    state = LN_ORACLE_eval(engine, 1, state, square);
    LN_ORACLE_emit(id, engine, "d", i, d);
    LN_ORACLE_emit(id, engine, "square", i, square);
    LN_ORACLE_emit(id, engine, "var_step", i, state);
  }
  uint32_t variance = LN_ORACLE_eval(engine, 4, state, n32);
  uint32_t t = LN_ORACLE_eval(engine, 1, variance, epsilon);
  uint32_t h = LN_ORACLE_eval(engine, 5, t);
  uint32_t q = LN_ORACLE_eval(engine, 4, 0x3f800000, h);
  LN_ORACLE_emit(id, engine, "v", 0, variance); LN_ORACLE_emit(id, engine, "t", 0, t);
  LN_ORACLE_emit(id, engine, "h", 0, h); LN_ORACLE_emit(id, engine, "q", 0, q);
  for (unsigned i = 0; i < count; ++i) {
    uint32_t d = engine == "C0" ? LN_ORACLE_eval(engine, 2, x[i], mu) : engine == "C2" ? scratch[i] : centered[i];
    uint32_t u = LN_ORACLE_eval(engine, 3, d, q), z = LN_ORACLE_eval(engine, 3, u, gamma[i]);
    LN_ORACLE_emit(id, engine, "u", i, u); LN_ORACLE_emit(id, engine, "z", i, z);
    LN_ORACLE_emit(id, engine, "y", i, LN_ORACLE_eval(engine, 1, z, beta[i]));
  }
}

/* Establish and measure this process's FP environment before accepting input.
 * No modification leaks into the compiler; restore it before normal exit. */
int main(void)
{
  std::fenv_t saved;
  if (std::feholdexcept(&saved) || std::fesetround(FE_TONEAREST)) return 2;
#if defined(__SSE__)
  _mm_setcsr((_mm_getcsr() | 0x1f80U) & ~0x8040U);
  if ((_mm_getcsr() & 0xe040U) != 0 || (_mm_getcsr() & 0x1f80U) != 0x1f80U) return 2;
#else
  std::cerr << "verified SSE gradual-underflow route required\n";
  return 2;
#endif
  if (sizeof(float) != 4 || std::fegetround() != FE_TONEAREST) return 2;
  std::cout << "AI_NORM_ENV 1 HOST rne 0 0 masked class_only " << mpfr_get_version() << ' ' << __LINE__ << '\n';
  std::cout << "AI_NORM_PRIM 1 HEADER case operation left right third host mpfr precision source_line\n";
  std::cout << "AI_NORM_NUM 1 HEADER case engine stage index bits source_line\n";
  try {
    std::string command, id;
    while (std::cin >> command >> id) {
      if (command == "OP") {
        unsigned operation;
        if (!(std::cin >> operation)) throw std::runtime_error("operation required");
        uint32_t a = LN_ORACLE_read_word(), b = LN_ORACLE_read_word(), c = LN_ORACLE_read_word();
        unsigned precision;
        uint32_t witness = LN_ORACLE_host(operation, a, b, c), reference = LN_ORACLE_mpfr(operation, a, b, c, &precision);
        std::cout << "AI_NORM_PRIM 1 VALUE " << id << ' ' << operation << ' ' << std::hex << std::setfill('0')
                  << std::setw(8) << a << ' ' << std::setw(8) << b << ' ' << std::setw(8) << c << ' '
                  << std::setw(8) << witness << ' ' << std::setw(8) << reference << std::dec << ' ' << precision << ' ' << __LINE__ << '\n';
      } else if (command == "ROW") {
        std::string engine;
        if (!(std::cin >> engine)) throw std::runtime_error("engine required");
        LN_ORACLE_row(id, engine);
      } else if (command == "COUNT") {
        unsigned count;
        if (!(std::cin >> count) || !count || count > 16777216) throw std::runtime_error("invalid count");
        LN_ORACLE_emit(id, "COUNT", "count", 0, LN_ORACLE_bits(static_cast<float>(count)));
      } else throw std::runtime_error("unknown request");
    }
  } catch (const std::exception &error) {
    std::cerr << error.what() << '\n';
    return 2;
  }
  return std::fesetenv(&saved) ? 2 : 0;
}
