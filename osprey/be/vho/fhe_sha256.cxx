/*
 * Copyright (C) 2026 Open64 Project
 *
 * Dependency-free SHA-256 used for process-local FHE plan signatures. This
 * is authentication bookkeeping, not a cryptographic runtime primitive.
 * Design: doc/FHE-SYNC6-S6-0C-DETAILED-EXECUTION-PLAN.md.
 */

#include "fhe_sha256.h"

#include <stdint.h>
#include <stdio.h>
#include <string.h>

namespace {

struct SHA256_Context {
  uint32_t state[8];
  uint64_t bit_count;
  unsigned char block[64];
  uint32_t block_size;
};

/* Rotate one SHA-256 word right by a compile-time algorithm amount. */
uint32_t Rotate(uint32_t value, uint32_t amount)
{
  return (value >> amount) | (value << (32 - amount));
}

/* Compress one complete 512-bit block into the running digest state. */
void Transform(SHA256_Context *context, const unsigned char *block)
{
  static const uint32_t constants[64] = {
    0x428a2f98U, 0x71374491U, 0xb5c0fbcfU, 0xe9b5dba5U,
    0x3956c25bU, 0x59f111f1U, 0x923f82a4U, 0xab1c5ed5U,
    0xd807aa98U, 0x12835b01U, 0x243185beU, 0x550c7dc3U,
    0x72be5d74U, 0x80deb1feU, 0x9bdc06a7U, 0xc19bf174U,
    0xe49b69c1U, 0xefbe4786U, 0x0fc19dc6U, 0x240ca1ccU,
    0x2de92c6fU, 0x4a7484aaU, 0x5cb0a9dcU, 0x76f988daU,
    0x983e5152U, 0xa831c66dU, 0xb00327c8U, 0xbf597fc7U,
    0xc6e00bf3U, 0xd5a79147U, 0x06ca6351U, 0x14292967U,
    0x27b70a85U, 0x2e1b2138U, 0x4d2c6dfcU, 0x53380d13U,
    0x650a7354U, 0x766a0abbU, 0x81c2c92eU, 0x92722c85U,
    0xa2bfe8a1U, 0xa81a664bU, 0xc24b8b70U, 0xc76c51a3U,
    0xd192e819U, 0xd6990624U, 0xf40e3585U, 0x106aa070U,
    0x19a4c116U, 0x1e376c08U, 0x2748774cU, 0x34b0bcb5U,
    0x391c0cb3U, 0x4ed8aa4aU, 0x5b9cca4fU, 0x682e6ff3U,
    0x748f82eeU, 0x78a5636fU, 0x84c87814U, 0x8cc70208U,
    0x90befffaU, 0xa4506cebU, 0xbef9a3f7U, 0xc67178f2U
  };
  uint32_t words[64];
  for (uint32_t i = 0; i < 16; ++i)
    words[i] = (uint32_t(block[i * 4]) << 24) |
               (uint32_t(block[i * 4 + 1]) << 16) |
               (uint32_t(block[i * 4 + 2]) << 8) |
               uint32_t(block[i * 4 + 3]);
  for (uint32_t i = 16; i < 64; ++i) {
    const uint32_t s0 = Rotate(words[i - 15], 7) ^
                        Rotate(words[i - 15], 18) ^
                        (words[i - 15] >> 3);
    const uint32_t s1 = Rotate(words[i - 2], 17) ^
                        Rotate(words[i - 2], 19) ^
                        (words[i - 2] >> 10);
    words[i] = words[i - 16] + s0 + words[i - 7] + s1;
  }
  uint32_t a = context->state[0], b = context->state[1];
  uint32_t c = context->state[2], d = context->state[3];
  uint32_t e = context->state[4], f = context->state[5];
  uint32_t g = context->state[6], h = context->state[7];
  for (uint32_t i = 0; i < 64; ++i) {
    const uint32_t s1 = Rotate(e, 6) ^ Rotate(e, 11) ^ Rotate(e, 25);
    const uint32_t choice = (e & f) ^ ((~e) & g);
    const uint32_t temp1 = h + s1 + choice + constants[i] + words[i];
    const uint32_t s0 = Rotate(a, 2) ^ Rotate(a, 13) ^ Rotate(a, 22);
    const uint32_t majority = (a & b) ^ (a & c) ^ (b & c);
    const uint32_t temp2 = s0 + majority;
    h = g; g = f; f = e; e = d + temp1;
    d = c; c = b; b = a; a = temp1 + temp2;
  }
  context->state[0] += a; context->state[1] += b;
  context->state[2] += c; context->state[3] += d;
  context->state[4] += e; context->state[5] += f;
  context->state[6] += g; context->state[7] += h;
}

}  // namespace

/* Hash exact bytes with standard SHA-256 padding and lowercase rendering. */
std::string VHO_FHE_SHA256(const unsigned char *bytes, size_t size)
{
  static const uint32_t initial[8] = {
    0x6a09e667U, 0xbb67ae85U, 0x3c6ef372U, 0xa54ff53aU,
    0x510e527fU, 0x9b05688cU, 0x1f83d9abU, 0x5be0cd19U
  };
  if (bytes == NULL && size != 0)
    return std::string();
  SHA256_Context context;
  memcpy(context.state, initial, sizeof(initial));
  context.bit_count = uint64_t(size) * 8;
  context.block_size = 0;
  for (size_t i = 0; i < size; ++i) {
    context.block[context.block_size++] = bytes[i];
    if (context.block_size == 64) {
      Transform(&context, context.block);
      context.block_size = 0;
    }
  }
  context.block[context.block_size++] = 0x80;
  if (context.block_size > 56) {
    while (context.block_size < 64)
      context.block[context.block_size++] = 0;
    Transform(&context, context.block);
    context.block_size = 0;
  }
  while (context.block_size < 56)
    context.block[context.block_size++] = 0;
  for (int shift = 56; shift >= 0; shift -= 8)
    context.block[context.block_size++] =
        static_cast<unsigned char>(context.bit_count >> shift);
  Transform(&context, context.block);

  char hex[65];
  for (uint32_t i = 0; i < 8; ++i)
    snprintf(hex + i * 8, 9, "%08x", context.state[i]);
  hex[64] = '\0';
  return std::string(hex);
}
