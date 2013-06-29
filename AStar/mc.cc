#include <cstdio>
#include <algorithm>
#include <vector>
#include <climits>
#include <map>
#include <queue>
#include <functional>
#include <climits>
using namespace std;

const int N = 5, K = 3;

struct Node;
extern map<Node, int> mapping;

struct Node
{
  int m, c, g, f;
  bool b;
  Node(int m, int c, bool b, int g) : m(m), c(c), b(b), g(g) {
    f = g + h();
  }
  int h() const {
    //return m + c - 2 * b;
    return 0;
  }
  bool operator==(const Node &rhs) const {
    return m == rhs.m && c == rhs.c && b == rhs.b;
  }
  bool operator<(const Node &rhs) const {
    if (m != rhs.m) return m < rhs.m;
    if (c != rhs.c) return c < rhs.c;
    return b < rhs.b;
  }
  bool operator>(const Node &rhs) const {
    return g+h() > rhs.g+rhs.h();
  }
  void draw() {
    printf("%d [shape=box,label=\"%d,%d,%d\\n%d+%d=%d\"];\n", mapping[*this], m, c, b, g, f-g, f);
  }
};

map<Node, int> mapping;

int main()
{
  priority_queue<Node, vector<Node>, greater<Node> > pq;

  puts("digraph {");

  int a[1] = {};
  Node c(N, N, 1, 0);
  mapping[c] = 0;
  pq.push(c);
  c.draw();

  while (! pq.empty()) {
    Node e = pq.top();
    pq.pop();
    if (mapping[e] < 0) continue;
    mapping[e] = ~ mapping[e];
    if (e.m == 0 && e.c == 0) {
      //printf("%d\n", ~ mapping[c]);
      break;
    }

    for (int i = 1; i <= K; i++)
      for (int m = 0; m <= i; m++) {
        int c = i-m;
        if (e.b) {
          if (e.m < m || e.c < c) continue;
          if (e.m-m > 0 && e.m-m < e.c-c) continue;
          if (N-(e.m-m) > 0 && N-(e.m-m) < N-(e.c-c)) continue;
          if (m > 0 && m < c) continue;
          Node s(e.m-m, e.c-c, false, e.g+1);
          if (! mapping.count(s)) {
            int t = mapping.size();
            mapping[s] = t;
            s.draw();
            pq.push(s);
            int x = mapping[e], y = mapping[s];
            if (x < 0) x = ~x;
            if (y < 0) y = ~y;
            printf("%d -> %d\n", x, y);
          }
        } else {
          if (e.m+m > N || e.c+c > N) continue;
          if (e.m+m > 0 && e.m+m < e.c+c) continue;
          if (N-(e.m+m) > 0 && N-(e.m+m) < N-(e.c+c)) continue;
          if (m > 0 && m < c) continue;
          Node s(e.m+m, e.c+c, true, e.g+1);
          if (! mapping.count(s)) {
            int t = mapping.size();
            mapping[s] = t;
            s.draw();
            pq.push(s);
            int x = mapping[e], y = mapping[s];
            if (x < 0) x = ~x;
            if (y < 0) y = ~y;
            printf("%d -> %d\n", x, y);
          }
        }
      }
  }

L1:;

  puts("}");
}
