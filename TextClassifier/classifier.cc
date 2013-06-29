#include <algorithm>
#include <cmath>
#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <vector>
#include <svm.h>
using namespace std;

class Word
{
public:
  Word(string filename) : fin(filename) {
  }
  void operator()(function<void(string)> f) {
    string buf;
    bool flag = false;
    for(;;) {
      char c = tolower(fin.get());
      if (flag) {
        if (! fin) {
          f(buf);
          break;
        } else if (islower(c))
          buf += c;
        else {
          f(buf);
          flag = false;
        }
      } else {
        if (! fin)
          break;
        else if (islower(c)) {
          flag = true;
          buf = c;
        }
      }
    }
  }
protected:
  ifstream fin;
};

template<typename K, typename V>
V findDefault(const map<K,V> &m, K k, V def = V())
{
  return m.count(k) ? m[k] : def;
}

class Classifier
{
public:
  virtual void train(map<string, vector<string>> cat2f) = 0;
  virtual string classify(string) = 0;
protected:
  void calcIdf(map<string, vector<string>> cat2f) {
    ndoc = 0;

    for (auto i: cat2f) {
      string cat = i.first;
      ndoc += i.second.size();
      categories.push_back(cat);
      for (auto filename: i.second) {
        set<string> added;
        Word reader(filename);
        reader([&](string w) {
          if (! vocab.count(w)) {
            int t = int(vocab.size());
            vocab[w] = t;
          }
          // document frequency
          if (! added.count(w)) {
            added.insert(w);
            idf[vocab[w]] += 1.0;
          }
        });
      }
    }

    // df -> idf
    for (auto &i: idf)
      i.second = log((ndoc + 1.0) / i.second);
  }

  int ndoc;
  map<string, int> vocab;
  map<int, double> idf;
  vector<string> categories;
};

class NaiveBayes : public Classifier
{
public:
  virtual void train(map<string, vector<string>> cat2f) override {
    calcIdf(cat2f);
    prior.resize(categories.size());
    prob.resize(categories.size());

    for (size_t i = 0; i < categories.size(); i++) {
      string cat = categories[i];
      prior[i] = double(cat2f[cat].size()) / ndoc;

      for (auto filename: cat2f[cat]) {
        Word reader(filename);
        reader([&](string w) {
          prob[i][vocab[w]] += 1.0;
        });
      }

      double tot = 0.0;
      for (auto w: prob[i])
        tot += w.second + 1.0;
      for (auto w: vocab)
        prob[i][w.second] = (prob[i][w.second] + 1.0) / tot;
    }
  }

  virtual string classify(string filename) override {
    vector<double> score(prior);
    Word reader(filename);
    reader([&](string w) {
      if (vocab.count(w))
        for (size_t i = 0; i < categories.size(); i++)
          score[i] += log(prob[i][vocab[w]]);
    });
    return categories[max_element(score.begin(), score.end()) - score.begin()];
  }

protected:
  vector<double> prior;
  vector<map<int, double> > prob;
};

class SVM : public Classifier
{
public:
  ~SVM() {
    svm_free_model_content(model);

    for (int i = 0; i < problem.l; i++)
      delete[] problem.x[i];
    delete[] problem.x;
    delete[] problem.y;
  }

  virtual void train(map<string, vector<string>> cat2f) override {
    calcIdf(cat2f);

    problem.l = ndoc;
    problem.y = new double[ndoc];
    problem.x = new svm_node*[ndoc];

    int row = 0;
    for (size_t i = 0; i < categories.size(); i++) {
      string cat = categories[i];

      for (auto filename: cat2f[cat]) {
        map<int, int> ws;
        Word reader(filename);
        reader([&](string w) {
          if (vocab.count(w))
            ws[vocab[w]]++;
        });

        int col = 0;
        problem.y[row] = i;
        problem.x[row] = new svm_node[ws.size() + 1];
        for (auto j: ws) {
          problem.x[row][col].index = j.first;
          problem.x[row][col].value = j.second * idf[j.first]; // tf*idf
          col++;
        }
        problem.x[row][col].index = -1;
        normalize(col, problem.x[row]);
        row++;
      }
    }

    svm_parameter param;
    param.svm_type = C_SVC;
    param.kernel_type = LINEAR;
    param.cache_size = 100;
    param.eps = 1e-5;
    param.C = 1;
    param.nr_weight = 0;
    param.weight_label = NULL;
    param.weight = NULL;
    param.shrinking = 1;
    param.probability = 0;

    const char *pstr = svm_check_parameter(&problem, &param);
    if (pstr)
      puts(pstr);
    model = svm_train(&problem, &param);
  }

  virtual string classify(string filename) override {
    map<int, int> ws;
    Word reader(filename);
    reader([&](string w) {
      if (vocab.count(w))
        ws[vocab[w]]++;
    });

    int col = 0;
    svm_node *x = new svm_node[ws.size() + 1];
    for (auto j: ws) {
      x[col].index = j.first;
      x[col].value = j.second * idf[j.first]; // tf*idf
      col++;
    }
    x[col].index = -1;
    normalize(col, x);

    double res = svm_predict(model, x);
    delete[] x;
    return categories[int(res)];
  }

protected:
  void normalize(int n, svm_node x[]) {
    double scale = 0;
    for (int i = 0; i < n; i++)
      scale += x[i].value * x[i].value;
    scale = 1 / sqrt(scale);
    //for (int i = 0; i < n; i++)
      //x[i].value *= scale;
  }

  svm_model *model;
  svm_problem problem;
};

class MakeClassifier
{
public:
  MakeClassifier(Classifier &c, bool verbose = false) : c(c), verbose(verbose) {}

  void train(string filelist) {
    map<string, vector<string>> cat2f;
    processFilelist(filelist, [&](string cat, string filename) {
      cat2f[cat].push_back(filename);
    });
    c.train(cat2f);
  }

  void classify(string filelist) {
    int correct = 0, tot = 0;
    processFilelist(filelist, [&](string cat, string filename) {
      string res = c.classify(filename);
      if (verbose)
        cout << filename << ' ' << res << endl;
      if (res == cat) // correct
        correct++;
      tot++;
    });
    cout << "precision: " << double(correct) / tot << endl;
  }

protected:
  void processFilelist(string filelist, function<void(string, string)> cont) {
    ifstream fin(filelist);
    string line, cat;
    while (getline(fin, line)) {
      if (line[0] == '#') {
        cat = line;
        while (! isalnum(cat[0]))
          cat = cat.substr(1);
      } else {
        cont(cat, line);
      }
    }
  }

  Classifier &c;
  bool verbose;
};

int main(int argc, char *argv[])
{
  bool verbose = false;
  if (argc >= 2 && argv[1][0] == '-') {
    verbose = true;
    argc--;
  }
  if (argc == 1) {
    NaiveBayes nb;
    MakeClassifier c(nb, verbose);
    c.train("train.list");
    c.classify("classify.list");
  } else {
    SVM svm;
    MakeClassifier c(svm, verbose);
    c.train("train.list");
    c.classify("classify.list");
  }
}
