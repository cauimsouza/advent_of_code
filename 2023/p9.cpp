#include <bits/stdc++.h>

using namespace std;

vector<string> ReadFile(const string& filename) {
    ifstream is(filename);
    
    vector<string> lines;
    string s;
    while (getline(is, s)) {
        lines.push_back(s);
    }
    
    return lines;
}

vector<int> ParseSeq(const string& line) {
    vector<int> seq;
    
    istringstream is(line);
    int n;
    while (is >> n) {
        seq.push_back(n);
    }
    return seq;
}

int Extrapolate(const vector<int>& seq) {
   vector<vector<int>> seqs;
   seqs.push_back(seq);
   
   int i = 0;
   while (any_of(seqs[i].begin(), seqs[i].end(), [](int n) { return n != 0; })) {
       seqs.push_back(vector<int>(seqs[i].size() - 1));
       for (int j = 1; j < seqs[i].size(); j++) {
           seqs[i + 1][j - 1] = seqs[i][j] - seqs[i][j - 1];
       }
       i++;
   }
   
   for (; i > 0; i--) {
       seqs[i - 1].push_back(seqs[i - 1].back() + seqs[i].back());
   }
   return seqs[0].back();
}

int main() {
    vector<string> lines = ReadFile("input_p9.txt");
    vector<vector<int>> seqs;
    for (const string& line : lines) {
        seqs.push_back(ParseSeq(line));
    }
    
    int ans1 = 0;
    for (const vector<int>& seq : seqs) {
        ans1 += Extrapolate(seq);
    }
    printf("Answer to part 1 is %d\n", ans1);
    
    int ans2 = 0;
    for (const vector<int>& seq : seqs) {
        ans2 += Extrapolate(vector<int> (seq.rbegin(), seq.rend()));
    }
    printf("Answer to part 2 is %d\n", ans2);
    
    return 0;
}