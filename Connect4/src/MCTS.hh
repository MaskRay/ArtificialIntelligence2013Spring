#ifndef UCT_HH
#define UCT_HH

#include <time.h>
#include "Position.hh"
#include "Type.hh"

namespace MCTS {
const double MAX_TIME = 4.4e6;
extern Move think(const Position &pos, Move lastMove);
extern void destruct();
}

#endif
