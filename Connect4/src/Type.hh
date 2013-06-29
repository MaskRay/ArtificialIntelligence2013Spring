#ifndef TYPE_HH
#define TYPE_HH

#include <stdint.h>
#include <stdio.h>

typedef uint8_t Move;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

#ifdef _MSC_VER
# define MSVC
#endif

#ifdef __GNUC__
# define popcount64 __builtin_popcountll
#elif defined(MSVC)
# include <intrin.h>
# ifdef __popcnt64
#  define popcount64 __popcnt64
# else
static __inline u32 popcount64(u64 x)
{
  return __popcnt(*(u32*)&x) + __popcnt(((u32*)&x)[1]);
}
#  define popcount64 popcount64
# endif
#endif

static int SWARPopCount64(uint64_t x)
{
  const uint64_t k1 = 0x5555555555555555ULL; // -1/3
  const uint64_t k2 = 0x3333333333333333ULL; // -1/5
  const uint64_t k4 = 0x0f0f0f0f0f0f0f0fULL; // -1/17
  x =  x - ((x >> 1) & k1);
  x = (x & k2) + ((x >> 2) & k2);
  x = (x + (x >> 4)) & k4;
  x = (x * 0x0101010101010101ULL) >> 56;
  return int(x);
}
#ifndef popcount64
# define popcount64 SWARPopCount64
#endif

#define FOR(i, a, b) for (int i = (a); i < (b); i++)
#define REP(i, n) FOR(i, 0, n)

enum Color {
  WHITE,
  BLACK,
};

enum Status {
  UNDETERMINED,
  WIN,
  DRAW,
  LOSE,
};

const int MAXM = 12, MAXN = 12;
const Move NULL_MOVE = 0xFF;

#endif
