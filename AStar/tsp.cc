#include <cstdio>
#include <algorithm>
#include <vector>
#include <climits>
#include <map>
#include <queue>
#include <functional>
#include <climits>
using namespace std;

const int N = 5;
const int M = 10000000;

int graph[N][N] = {
  {0,7,6,10,13},
  {7,0,7,10,10},
  {6,7,0,5,9},
  {10,10,5,0,6},
  {13,10,9,6,0},
}, gmin[N];

struct Node;
extern map<Node, int> mapping;

struct Node
{
  int n, a[N+1], g, f;
  Node(int n, const int b[], int g) : n(n), g(g) {
    copy(b, b+n, a);
    f = g + h();
  }
  int hh() const {
    int res = 0;
    for (int i = 0; i < N; i++)
      res += gmin[i];
    for (int i = 1; i < n; i++)
      res -= gmin[a[i-1]];
    return res;
  }
  int h() const {
    int d[N], res = 0;
    bool flag[N] = {};
    fill(d, d+N, INT_MAX);
    for (int i = 0; i < max(1, n-1); i++) {
      for (int j = 0; j < N; j++)
        d[j] = min(d[j], graph[a[i]][j]);
      flag[a[i]] = true;
    }
    for(;;) {
      int u = -1, mind = INT_MAX;
      for (int i = 0; i < N; i++)
        if (! flag[i] && d[i] < mind)
          mind = d[u = i];
      if (u == -1) break;
      flag[u] = true;
      res += mind;
      for (int i = 0; i < N; i++)
        d[i] = min(d[i], graph[u][i]);
    }
    return res;
  }
  bool operator==(const Node &rhs) const {
    if (n != rhs.n) return false;
    for (int i = 0; i < n; i++)
      if (a[i] != rhs.a[i])
        return false;
    return true;
  }
  bool operator<(const Node &rhs) const {
    if (n != rhs.n) return n < rhs.n;
    for (int i = 0; i < n; i++)
      if (a[i] != rhs.a[i])
        return a[i] < rhs.a[i];
    return false;
  }
  bool operator>(const Node &rhs) const {
    return g+h() > rhs.g+rhs.h();
  }
  void draw() {
    printf("%d [shape=box,label=\"", mapping[*this]);
    for (int i = 0; i < n; i++)
      putchar('A'+a[i]);
    printf("\\n%d+%d=%d\"];\n", g, f-g, f);
  }
};

map<Node, int> mapping;

int main()
{
  for (int i = 0; i < N; i++) {
    gmin[i] = INT_MAX;
    for (int j = 0; j < N; j++)
      if (j != i)
        gmin[i] = min(gmin[i], graph[i][j]);
  }

  priority_queue<Node, vector<Node>, greater<Node> > pq;

  puts("digraph {");

  int a[1] = {};
  Node c(1, a, 0);
  mapping[c] = 0;
  pq.push(c);
  c.draw();

  while (! pq.empty()) {
    Node c = pq.top();
    pq.pop();
    if (mapping[c] < 0) continue;
    mapping[c] = ~ mapping[c];
    if (c.n == N+1) {
      //printf("%d\n", ~ mapping[c]);
      break;
    }

    bool exist[N] = {};
    for (int i = 0; i < c.n; i++)
      exist[c.a[i]] = true;
    if (c.n == N)
      exist[c.a[0]] = false;
    for (int i = 0; i < N; i++)
      if (! exist[i]) {
        c.a[c.n] = i;
        Node s(c.n+1, c.a, c.g+graph[c.a[c.n-1]][i]);
        if (! mapping.count(s)) {
          int t = mapping.size();
          mapping[s] = t;
          pq.push(s);
          s.draw();
          int x = mapping[c], y = mapping[s];
          if (x < 0) x = ~x;
          if (y < 0) y = ~y;
          printf("%d -> %d\n", x, y);
        }
      }
  }

L1:;

  puts("}");
}
