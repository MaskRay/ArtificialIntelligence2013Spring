#include "Position.hh"

int Position::ctz[1 << 16];

Position::Position(int m, int n, const int *board, int noX, int noY)
  : m(m), n(n), hole(m-1-noX + 16*noY), side2move(WHITE)
    , w(0, 0, 0), b(0, 0, 0)
{
  REP(y, n)
    REP(x, m)
      switch (board[n*(m-1-x)+y]) {
      case 2:
        w.set(x + 16 * y);
        break;
      case 1:
        b.set(x + 16 * y);
        break;
      }
  init();
}

int Position::genMoves(Move moves[]) const
{
  Bitboard occ = w | b;
  occ.set(hole);
  occ = occ + Bitboard(0x0001000100010001ULL, 0x0001000100010001ULL, 0x0001000100010001ULL);
  int cnt = 0;
  REP(y, n) {
    //int top = __builtin_ctz(*((unsigned short *)&occ.a + y));
    int top = ctz[occ.b[y]];
    if (top < m) {
      moves[y] = top + 16 * y;
      cnt++;
    } else
      moves[y] = NULL_MOVE;
  }
  return cnt;
}

void Bitboard::dump() const
{
  REP(x, 16) {
    REP(y, 12)
      putchar(test(15-x+y*16) ? 'x' : '.');
    puts("");
  }
}

static bool winRow(const Bitboard &w)
{
  Bitboard vert = w & w >> 1 & w >> 2 & w >> 3;
  Bitboard hori = w & w >> 16 & w >> 32 & w >> 48;
  Bitboard bslash = w & w >> 15 & w >> 30 & w >> 45;
  Bitboard slash = w & w >> 17 & w >> 34 & w >> 51;
  return (hori | vert | bslash | slash).pop() > 0;
}

Status Position::status() const
{
  if (winRow(w)) return side2move == WHITE ? WIN : LOSE;
  if (winRow(b)) return side2move == BLACK ? WIN : LOSE;
  return round() == m * n - 1 ? DRAW : UNDETERMINED;
}
