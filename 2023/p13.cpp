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

vector<int> VectorDiff(const vector<int>& A, const vector<int>& B) {
    set<int> s;
    for (int n : B) {
        s.insert(n);
    }
    
    vector<int> res;
    for (int n : A) {
        if (s.count(n) == 0) {
            res.push_back(n);
            return res;
        }
    }
    return res;
}

struct Case {
    vector<int> rows;
    vector<int> cols;
    
    Case(const vector<string>& grid) {
        for (int i = 0; i < grid.size(); i++) {
            int n = 0;
            for (int j = 0; j < grid[0].size(); j++) {
                if (grid[i][j] == '#') {
                    n |= (1 << j);
                }
            }
            rows.push_back(n);
        }
        
        for (int i = 0; i < grid[0].size(); i++) {
            int n = 0;
            for (int j = 0; j < grid.size(); j++) {
                if (grid[j][i] == '#') {
                    n |= (1 << j);
                }
            }
            cols.push_back(n);
        }
    }
    
    pair<vector<int>, vector<int>> Part2Score() {
        const int n = rows.size();
        const int m = cols.size();
        
        const auto& [left0, up0] = Score();
        
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                rows[i] ^= (1 << j);
                cols[j] ^= (1 << i);
                const auto& [left, up] = Score();
                rows[i] ^= (1 << j);
                cols[j] ^= (1 << i);
                
                vector<int> left_diff = VectorDiff(left, left0);
                vector<int> up_diff = VectorDiff(up, up0);
                
                if (!left_diff.empty()) {
                    return make_pair(left_diff, vector<int> ());
                } else if (!up_diff.empty()) {
                    return make_pair(vector<int> (), up_diff);
                }
            }
        }
        
        return make_pair(vector<int> (), vector<int> ()); // Should never get here
    }
    
    pair<vector<int>, vector<int>> Score() {
        return make_pair(Score(cols), Score(rows));
    }
    
    vector<int> Score(const vector<int>& nums) {
        vector<int> scores;
        for (int i = 0; i < nums.size() - 1; i++) {
            int j = i;
            int k = i + 1;
            bool match = true;
            while (j >= 0 && k < nums.size()) {
                if (nums[j] != nums[k]) {
                    match = false;
                    break;
                }
                j--;
                k++;
            }
            if (match) {
                scores.push_back(i + 1);
            }
        }
        return scores;
    }
};

vector<Case> ParseCases(const vector<string>& lines) {
    vector<Case> cases;
    
    vector<string> grid;
    for (const string& line : lines) {
        if (line.size() == 0) {
            Case c(grid);
            cases.push_back(c);
            
            grid.clear();
            continue;
        }
        grid.push_back(line);
    }
    if (!grid.empty()) {
        Case c(grid);
        cases.push_back(c);
    }
    
    return cases; 
}

int main() {
    vector<Case> cases = ParseCases(ReadFile("input_p13.txt"));
    
    int sum = 0;
    for (Case& c : cases) {
        auto [left, up] = c.Score();
        if (!left.empty()) {
            sum += left[0];
        } else {
            sum += up[0] * 100;
        }
    }
    printf("Answer to part 1 is %d\n", sum);
    
    sum = 0;
    int i = 1;
    for (Case& c : cases) {
        auto [left, up] = c.Part2Score();
        if (!left.empty()) {
            sum += left[0];
        } else {
            sum += up[0] * 100;
        }
    }
    printf("Answer to part 2 is %d\n", sum);
    
    return 0;
}