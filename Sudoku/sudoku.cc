#include <limits.h>
#include <stdio.h>

const int SENTRY = 9*9*4, N = SENTRY+1+9*9*9*4;
int a[9][9], L[N], R[N], U[N], D[N], C[N], meaning[N], S[SENTRY+1], selected[9*9];

void insert(int cur, int left, int right, int top)
{
  L[cur] = left; R[cur] = right;
  U[cur] = U[top]; D[U[cur]] = cur;
  D[cur] = top; U[top] = cur;
  C[cur] = top;
  S[top]++;
}

void cover(int c)
{
  L[R[c]] = L[c];
  R[L[c]] = R[c];
  for (int i = D[c]; i != c; i = D[i])
    for (int j = R[i]; j != i; j = R[j]) {
      U[D[j]] = U[j];
      D[U[j]] = D[j];
    }
}

void uncover(int c)
{
  for (int i = U[c]; i != c; i = U[i])
    for (int j = L[i]; j != i; j = L[j]) {
      U[D[j]] = j;
      D[U[j]] = j;
    }
	L[R[c]] = c;
	R[L[c]] = c;
}

// Dancing links with Algorithm X
bool DLX()
{
  if (R[SENTRY] == SENTRY) // all columns are covered, that is, all cells are filled
    return true;

  // heuristic: select the column associated with the fewest rows
  int c, mins = INT_MAX;
  for (int i = R[SENTRY]; i != SENTRY; i = R[i])
    if (S[i] < mins)
      mins = S[i], c = i;

  cover(c);
  for (int i = D[c]; i != c; i = D[i]) { // enumerate all candidates that covers column c
    for (int j = R[i]; j != i; j = R[j])
      cover(C[j]);
    a[meaning[i]/9/9][meaning[i]/9%9] = 1+meaning[i]%9;
    if (DLX()) return true;
    for (int j = L[i]; j != i; j = L[j])
      uncover(C[j]);
  }
  uncover(c);
  return false;
}

int main()
{
  for (int i = 0; i < 9; i++)
    for (int j = 0; j < 9; j++)
      scanf("%1d", &a[i][j]);

  int cur = SENTRY+1;
  for (int i = 0; i <= SENTRY; ++i) {
    L[i] = i-1; R[i] = i+1;
    U[i] = D[i] = i;
    S[i] = 0;
  }
  L[0] = SENTRY; R[SENTRY] = 0;

  for (int i = 0; i < 9; i++)
    for (int j = 0; j < 9; j++) {
      int lo = 0, hi = 9;
      if (a[i][j]) {
        lo = a[i][j]-1;
        hi = a[i][j];
      }
      for (int k = lo; k < hi; k++) {
        // make a row to represent number k at cell (i, j)
        for (int l = 0; l < 4; l++)
          meaning[cur+l] = (i*9+j)*9+k;

        // putting number k at cell (i,j) has four consequences

        // no other number k can appear at row i
        insert(cur, cur+3, cur+1, i*9+k); cur++;
        // no other number k can appear at C j
        insert(cur, cur-1, cur+1, 9*9+j*9+k); cur++;
        // no other number k can appear at box (i/3*3+j/3)
        insert(cur, cur-1, cur+1, 9*9*2+(i/3*3+j/3)*9+k); cur++;
        // no other number k can appear at cell (i,j)
        insert(cur, cur-1, cur-3, 9*9*3+i*9+j); cur++;
      }
    }

  if (DLX())
    for (int i = 0; i < 9; i++) {
      for (int j = 0; j < 9; j++)
        printf("%d", a[i][j]);
      puts("");
    }
}
