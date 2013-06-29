#include <algorithm>
#include <climits>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <queue>
#include <vector>
using namespace std;

const int factorial[] = {1,1,2,6,24,120,720,5040,40320};

#define FOR(i, a, b) for (int i = (a); i < (b); i++)
#define REP(i, n) FOR(i, 0, n)

enum Direction {U, R, D, L};

struct Node
{
  int a[9], g, h;

  void updateH(const Node &src);
  int zero() const {
    return int(find(a, a + 9, 0) - a);
  }
  Node up() {
    int p = zero();
    if (p < 3) throw 0;
    Node res = *this;
    swap(res.a[p], res.a[p-3]);
    res.g = g + 1;
    return res;
  }
  Node right() {
    int p = zero();
    if (p % 3 == 2) throw 0;
    Node res = *this;
    swap(res.a[p], res.a[p+1]);
    res.g = g + 1;
    return res;
  }
  Node down() {
    int p = zero();
    if (p >= 6) throw 0;
    Node res = *this;
    swap(res.a[p], res.a[p+3]);
    res.g = g + 1;
    return res;
  }
  Node left() {
    int p = zero();
    if (p % 3 == 0) throw 0;
    Node res = *this;
    swap(res.a[p], res.a[p-1]);
    res.g = g + 1;
    return res;
  }
  bool operator<(const Node &rhs) const {
    return g + h < rhs.g + rhs.h;
  }
  bool operator>(const Node &rhs) const {
    return g + h > rhs.g + rhs.h;
  }
  bool operator==(const Node &rhs) const {
    return equal(a, a + 9, rhs.a);
  }
  bool operator!=(const Node &rhs) const {
    return ! (*this == rhs);
  }
};

int closed[362880];

const Node dst = {{1,2,3,8,0,4,7,6,5}};

void Node::updateH(const Node &src)
{
  int p[9], pp[9];
  h = 0;
  REP(i, 9)
    p[src.a[i]] = i;
  REP(i, 9)
    pp[a[i]] = i;
  FOR(i, 1, 9) {
    int x = p[i] % 3, y = p[i] / 3;
    int xx = pp[i] % 3, yy = pp[i] / 3;
    h += abs(x - xx) + abs(y - yy);
  }
}

int lehmorCode(const Node &x)
{
  int h = 0;
  REP(i, 9)
    REP(j, i)
      if (x.a[j] > x.a[i])
        h += factorial[8-j];
  return h;
}

int parity(const Node &x)
{
  int h = 0;
  REP(i, 9)
    REP(j, i)
      if (x.a[i] > 0 && x.a[j] > x.a[i])
        h++;
  return h % 2;
}

int astar(const Node &src)
{
  if (parity(src) != parity(dst))
    return -1;

  priority_queue<Node, vector<Node>, greater<Node>> q;
  fill_n(closed, 362880, -1);
  int dst_code = lehmorCode(dst);
  int src_code = lehmorCode(src);

  Node orig = dst;
  orig.updateH(src);
  q.push(orig);
  closed[dst_code] = -2;

  while (! q.empty()) {
    Node curr = q.top();
    q.pop();
    if (curr == src) return curr.g;
    int dir = 0;
    function<Node(Node &)> fs[] = {&Node::up, &Node::right, &Node::down, &Node::left};
    for (auto f : fs) {
      dir++;
      try {
        Node succ = f(curr);
        succ.updateH(src);
        int h = lehmorCode(succ);
        if (closed[h] == -1) {
          q.push(succ);
          closed[h] = dir - 1;
        }
      } catch (int) {
      }
    }
  }
  return -1;
}

void print(Node x)
{
  for(;;) {
    switch (closed[lehmorCode(x)]) {
    case U:
      x = x.down();
      break;
    case R:
      x = x.left();
      break;
    case D:
      x = x.up();
      break;
    case L:
      x = x.right();
      break;
    case -2:
      return;
    }
    puts("");
    REP(i, 9)
      printf("%d%c", x.a[i], i % 3 == 2 ? '\n' : ' ');
  }
}

int main(int argc, char *argv[])
{
  if (argc >= 2) freopen(argv[1], "r", stdin);
  if (argc >= 3) freopen(argv[2], "w", stdout);
  Node src;
  REP(i, 9)
    scanf("%1d", &src.a[i]);
  int res = astar(src);
  if (res == -1)
    puts("no solution");
  else {
    printf("%d\n", res);
    print(src);
  }
}
