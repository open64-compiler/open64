/*
 * Copyright (C) 2026 Open64 Project
 *
 * Small dependency-free SHA-256 implementation for mock-runtime evidence.
 */

#include "open64_fhe_mock_sha256.h"

#include <string.h>

namespace {

struct SHA256_CONTEXT {
  uint32_t state[8];
  uint64_t total_size;
  uint8_t block[64];
  size_t block_size;
};

static uint32_t
Rotate_Right(uint32_t value, uint32_t amount)
{
  return (value >> amount) | (value << (32 - amount));
}

static uint32_t
Load_BE32(const uint8_t *bytes)
{
  return (uint32_t(bytes[0]) << 24) | (uint32_t(bytes[1]) << 16) |
         (uint32_t(bytes[2]) << 8) | uint32_t(bytes[3]);
}

static void
Store_BE32(uint8_t *bytes, uint32_t value)
{
  bytes[0] = uint8_t(value >> 24);
  bytes[1] = uint8_t(value >> 16);
  bytes[2] = uint8_t(value >> 8);
  bytes[3] = uint8_t(value);
}

static void
Transform(SHA256_CONTEXT *context, const uint8_t block[64])
{
  static const uint32_t constants[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
  };
  uint32_t words[64];
  for (uint32_t i = 0; i < 16; ++i)
    words[i] = Load_BE32(block + i * 4);
  for (uint32_t i = 16; i < 64; ++i) {
    uint32_t s0 = Rotate_Right(words[i - 15], 7) ^
                  Rotate_Right(words[i - 15], 18) ^
                  (words[i - 15] >> 3);
    uint32_t s1 = Rotate_Right(words[i - 2], 17) ^
                  Rotate_Right(words[i - 2], 19) ^
                  (words[i - 2] >> 10);
    words[i] = words[i - 16] + s0 + words[i - 7] + s1;
  }

  uint32_t a = context->state[0];
  uint32_t b = context->state[1];
  uint32_t c = context->state[2];
  uint32_t d = context->state[3];
  uint32_t e = context->state[4];
  uint32_t f = context->state[5];
  uint32_t g = context->state[6];
  uint32_t h = context->state[7];
  for (uint32_t i = 0; i < 64; ++i) {
    uint32_t s1 = Rotate_Right(e, 6) ^ Rotate_Right(e, 11) ^
                  Rotate_Right(e, 25);
    uint32_t choose = (e & f) ^ ((~e) & g);
    uint32_t temp1 = h + s1 + choose + constants[i] + words[i];
    uint32_t s0 = Rotate_Right(a, 2) ^ Rotate_Right(a, 13) ^
                  Rotate_Right(a, 22);
    uint32_t majority = (a & b) ^ (a & c) ^ (b & c);
    uint32_t temp2 = s0 + majority;
    h = g;
    g = f;
    f = e;
    e = d + temp1;
    d = c;
    c = b;
    b = a;
    a = temp1 + temp2;
  }
  context->state[0] += a;
  context->state[1] += b;
  context->state[2] += c;
  context->state[3] += d;
  context->state[4] += e;
  context->state[5] += f;
  context->state[6] += g;
  context->state[7] += h;
}

static void
Update(SHA256_CONTEXT *context, const uint8_t *bytes, size_t size)
{
  context->total_size += size;
  while (size != 0) {
    size_t available = sizeof(context->block) - context->block_size;
    size_t copied = size < available ? size : available;
    memcpy(context->block + context->block_size, bytes, copied);
    context->block_size += copied;
    bytes += copied;
    size -= copied;
    if (context->block_size == sizeof(context->block)) {
      Transform(context, context->block);
      context->block_size = 0;
    }
  }
}

} // namespace

void
open64_fhe_mock_sha256(const void *bytes, size_t size, uint8_t digest[32])
{
  SHA256_CONTEXT context = {
    { 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
      0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19 },
    0,
    { 0 },
    0
  };
  const uint8_t *input = static_cast<const uint8_t *>(bytes);
  if (size != 0)
    Update(&context, input, size);
  uint64_t bit_size = context.total_size * 8;
  const uint8_t marker = 0x80;
  Update(&context, &marker, 1);
  const uint8_t zero = 0;
  while (context.block_size != 56)
    Update(&context, &zero, 1);
  uint8_t length[8];
  for (uint32_t i = 0; i < 8; ++i)
    length[7 - i] = uint8_t(bit_size >> (i * 8));
  Update(&context, length, sizeof(length));
  for (uint32_t i = 0; i < 8; ++i)
    Store_BE32(digest + i * 4, context.state[i]);
}
