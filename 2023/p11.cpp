#include <bits/stdc++.h>

using namespace std;

using ii = pair<int, int>;
using ll = long long;

ll SumDistances(const vector<string>& grid, int empty_multiplier) {
    const int n = grid.size();
    const int m = grid[0].size();
    
    vector<int> empty_rows(n, 1);
    vector<int> empty_cols(m, 1);
    
    vector<ii> galaxies;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            if (grid[i][j] == '#') {
                galaxies.push_back(ii(i, j));
                empty_rows[i] = 0;
                empty_cols[j] = 0;
            }
        }
    }
    
    for (int i = 1; i < n; i++) {
        empty_rows[i] += empty_rows[i - 1];
    }
    for (int j = 1; j < m; j++) {
        empty_cols[j] += empty_cols[j - 1];
    }
    
    ll sum = 0;
    for (int k = 0; k < galaxies.size(); k++) {
        auto [i0, j0] = galaxies[k];
        for (int l = k + 1; l < galaxies.size(); l++) {
           auto [i, j] = galaxies[l]; 
           
           ll erows = abs(empty_rows[i] - empty_rows[i0]) * ((ll) (empty_multiplier - 1));
           ll ecols = abs(empty_cols[j] - empty_cols[j0]) * ((ll) (empty_multiplier - 1));
           sum += erows + ecols + abs(i - i0) + abs(j - j0);
        }
    }
    
    return sum;
}

vector<string> ReadFile(const string& filename) {
    ifstream is(filename);
    
    vector<string> lines;
    string s;
    while (getline(is, s)) {
        lines.push_back(s);
    }
    
    return lines;
}

int main() {
    vector<string> grid = ReadFile("input_p11.txt");
    for (const string& line : grid) {
        cout << line << endl;
    }
    
    printf("Answer to part 1 is %lld\n", SumDistances(grid, 2));
    printf("Answer to part 2 is %lld\n", SumDistances(grid, 1000000));
    
    return 0;
}