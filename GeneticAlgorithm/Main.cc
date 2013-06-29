#include <algorithm>
#include <cfloat>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <ctime>
using namespace std;

#define FOR(i, a, b) for (int i = (a); i < (b); i++)
#define REP(i, n) FOR(i, 0, n)

class TSP
{
public:
  virtual ~TSP() {
    delete[] names;
    delete[] x;
    delete[] y;
    REP(i, n)
      delete[] g[i];
    delete[] g;
  }

  void read() {
    scanf("%d", &n);
    names = new char[n][99];
    g = new double*[n];
    x = new double[n];
    y = new double[n];
    REP(i, n) {
      g[i] = new double[n];
      scanf("%s%lf%lf", names[i], &x[i], &y[i]);
    }
    // compute the adjacent matrix
    REP(i, n)
      REP(j, n)
        g[i][j] = hypot(x[i] - x[j], y[i] - y[j]);
  }

  // print the tour
  void print(int tour[]) {
    REP(i, n)
      printf("%s ", names[tour[i]]);
    printf("%lf\n", tourCost(tour));
  }

  // calculate the total length of the tour
  double tourCost(int tour[]) {
    double cost = 0;
    REP(i, n)
      cost += g[tour[i]][tour[(i+1)%n]];
    return cost;
  }

  virtual void solve() = 0;

  int n;
  double **g, *x, *y;
  char (*names)[99];
};

class SimulatedAnnealing : public TSP
{
public:
  virtual ~SimulatedAnnealing() {
  }
  virtual void solve() {
    int *tour = new int[n+1], *optTour = new int [n];

    // Initial temperature: maximum distance between two vertices
    double T = 0;
    REP(i, n)
      REP(j, n)
        T = max(T, g[i][j]);

    REP(i, n)
      tour[i] = i;
    tour[n] = 0;
    double cost = tourCost(tour);

    int whenOnArena = -1;
    double optCost = cost;
    copy(tour, tour + n, optTour);

    // quit after 11000 times cooling
    for (int tick = 0; tick - whenOnArena < ON_ARENA_THRESHOLD; tick++) {
      // found a better solution?
      bool flag = false;

      // randomly pick 1000 pairs of vertices for each temperature
      REP(iter, ITERATIONS) {
        int u = rand() % (n-1) + 1;
        int v = rand() % (n-1) + 1;
        if (u == v) continue;
        double cost2; // cost of new state
        if (abs(u - v) == 1) {
          if (u > v) swap(u, v);
          cost2 = cost - g[tour[u-1]][tour[u]] - g[tour[v+1]][tour[v]]
            + g[tour[u-1]][tour[v]] + g[tour[v+1]][tour[u]];
        } else
          cost2 = cost - g[tour[u-1]][tour[u]] - g[tour[u+1]][tour[u]]
            - g[tour[v-1]][tour[v]] - g[tour[v+1]][tour[v]]
            + g[tour[u-1]][tour[v]] + g[tour[u+1]][tour[v]]
            + g[tour[v-1]][tour[u]] + g[tour[v+1]][tour[u]];

        // new state is better or its evaluation is within the acceptance probability
        if (cost2 < cost || exp((cost-cost2)/T) > rand()/(RAND_MAX+1.0)) {
          swap(tour[u], tour[v]);
          cost = cost2;
          if (cost2 < optCost - 1e-10) {
            flag = true;
            whenOnArena = tick;
            optCost = cost2;
            copy(tour, tour + n, optTour);
          }
        }
      }

      if (flag)
        print(optTour);
      // exponential cooling schedule
      T *= 0.999;
    }

    delete[] tour;
    delete[] optTour;
  }

  static const int ITERATIONS = 1000;
  static const int ON_ARENA_THRESHOLD = 100000;
};

class Genetic : public TSP
{
public:
  struct Chromosome {
    int *tour; // n vertices representing the circuit
    double cost; // length of the circuit
    void init(int n) { tour = new int[n]; }
    void destruct() { delete[] tour; }
    int &operator[](size_t i) { return tour[i]; }
    // compare adapability
    bool operator<(const Chromosome &rhs) const { return cost < rhs.cost; }
  };

  virtual ~Genetic() {
  }

  // Crossover technique: partially matched crossover
  // Randomly pick the slices to be swapped
  // and then loop over indices outside of the slice to remove duplicates
  void crossover(const Chromosome &x, const Chromosome &y, Chromosome &z, Chromosome &w) {
    copy(x.tour, x.tour + n, z.tour);
    copy(y.tour, y.tour + n, w.tour);
    REP(i, n)
      to[i] = to2[i] = i;

    int u = rand() % n, v = rand() % n, len = (v - u + n) % n;
    REP(j, len) {
      int i = (u + j) % n;
      swap(z[i], w[i]);
      to[z[i]] = w[i];
      to2[w[i]] = z[i];
    }
    REP(i, n) {
      int d = (i - u + n) % n;
      int d2 = (v - u + n) % n;
      if (d >= d2) {
        while (z[i] != to[z[i]])
          z[i] = to[z[i]];
        while (w[i] != to2[w[i]])
          w[i] = to2[w[i]];
      }
    }

    z.cost = tourCost(z.tour);
    w.cost = tourCost(w.tour);
  }

  // Mutation technique: randomly pick two vertices to be swapped in the circuit
  void mutate(const Chromosome &y, Chromosome &z) {
    int u = rand() % n, v = rand() % n;
    copy(y.tour, y.tour + n, z.tour);
    swap(z[u], z[v]);
    z.cost = tourCost(z.tour);
  }

  void update(int tick, const Chromosome &chrom) {
    if (chrom.cost < optCost) {
      whenOnArena = tick;
      optCost = chrom.cost;
      copy(chrom.tour, chrom.tour + n, optTour);
    }
  }

  virtual void solve() {
    to = new int[n];
    to2 = new int[n];

    const int max_size = CHRONOSOMES * 3 + CHRONOSOMES / 2;
    Chromosome *chrom = new Chromosome[max_size];
    REP(i, max_size) chrom[i].init(n);

    whenOnArena = -1;
    optTour = new int[n];
    optCost = DBL_MAX;

    // Initialization
    // Generate `CHRONOSOMES` random permutations
    REP(i, CHRONOSOMES) {
      REP(j, n)
        chrom[i][j] = j;
      random_shuffle(chrom[i].tour, chrom[i].tour + n);
      chrom[i].cost = tourCost(chrom[i].tour);
      update(-1, chrom[i]);
    }

    // Termination condition: 
    int whenOnArena = 0;
    for (int tick = 0; tick - whenOnArena < ON_ARENA_THRESHOLD; tick++) {
      int tot = CHRONOSOMES;

      // mutation
      REP(i, CHRONOSOMES) {
        Chromosome &t = chrom[tot++];
        mutate(chrom[i], t);
        update(tick, t);
      }

      // crossover
      sort(chrom, chrom + CHRONOSOMES);
      REP(i, CHRONOSOMES / 2) {
        int j = rand() % CHRONOSOMES;
        Chromosome &t = chrom[tot], &tt = chrom[tot+1];
        tot += 2;
        crossover(chrom[i], chrom[j], t, tt);
        update(tick, t);
        update(tick, tt);
      }

      // fitness proportionate selection
      // keep `CHRONOSOMES` chronosomes and remove the rest
      REP(i, CHRONOSOMES) {
        double roulette = 0;
        int u = i;
        FOR(j, i, tot)
          roulette += 1.0 / chrom[j].cost;
        roulette *= rand() / (RAND_MAX + 1.0);
        FOR(j, i, tot)
          if ((roulette -= 1.0 / chrom[j].cost) < 0.0) {
            u = j;
            break;
          }
        swap(chrom[i], chrom[u]);
      }
    }

    print(optTour);

    delete[] to;
    delete[] to2;
    delete[] optTour;
    REP(i, max_size)
      chrom[i].destruct();
    delete[] chrom;
  }

  int *to, *to2, *optTour, whenOnArena;
  double optCost;

  static const int CHRONOSOMES = 100;
  static const int ON_ARENA_THRESHOLD = 20000;
};

int main(int argc, char *argv[])
{
  srand(time(NULL));
  Genetic tsp;

  FILE *in = stdin;
  if (argc > 1)
    freopen(argv[1], "r", stdin);
  tsp.read();
  fclose(in);

  FILE *out = stdout;
  if (argc > 2)
    freopen(argv[2], "w", stdout);
  tsp.solve();
  fclose(out);
}
