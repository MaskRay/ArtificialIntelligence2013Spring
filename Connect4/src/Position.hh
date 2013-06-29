#ifndef POSITION_HH
#define POSITION_HH

#include "Type.hh"

class Bitboard
{
public:
  Bitboard(u64 a0, u64 a64, u64 a128) {
    a[0] = a0;
    a[1] = a64;
    a[2] = a128;
  }
  bool operator==(const Bitboard &x) const {
    return a[0] == x.a[0] && a[1] == x.a[1] && a[2] == x.a[2];
  }
  Bitboard operator>>(int x) const {
    return Bitboard(a[1] << (64 - x) | a[0] >> x,
                    a[2] << (64 - x) | a[1] >> x,
                    a[2] >> x);
  }
  Bitboard operator&(const Bitboard &x) const {
    return Bitboard(a[0] & x.a[0], a[1] & x.a[1], a[2] & x.a[2]);
  }
  Bitboard operator|(const Bitboard &x) const {
    return Bitboard(a[0] | x.a[0], a[1] | x.a[1], a[2] | x.a[2]);
  }
  Bitboard operator+(const Bitboard &x) const {
    return Bitboard(a[0] + x.a[0], a[1] + x.a[1], a[2] + x.a[2]);
  }
  operator bool() const {
    return (a[0] | a[1] | a[2]) != 0;
  }
  void set(int x) {
    a[x >> 6] |= 1ULL << (x & 63);
  }
  bool test(int x) const {
    return a[x >> 6] & 1ULL << (x & 63);
  }
  int pop() const {
    return popcount64(a[0]) + popcount64(a[1]) + popcount64(a[2]);
  }
  void dump() const;
  union {
    u64 a[3];
    u16 b[12];
  };
};

class Position
{
public:
  Position(int m, int n, const int *board, int noX, int noY);
  void play(Move x) {
    (side2move == WHITE ? w : b).set(x);
    side2move = side2move == WHITE ? BLACK : WHITE;
  }
  int genMoves(Move moves[]) const;
  int round() const {
    return (w | b).pop();
  }
  static void init() {
    ctz[0] = ctz[1] = 0;
    FOR(i, 1, 1 << 15) {
      ctz[i*2] = ctz[i] + 1;
      ctz[i*2+1] = 0;
    }
  }
  Status status() const;
  Color side2move;
  int m, n;
public:
  Bitboard w, b;
  Move move;
  int hole;
  static int ctz[1 << 16];
};

#endif
