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

using ii = pair<int, int>;
using ll = long long;

struct Case {
    string chars;
    vector<int> nums;
    
    map<ii, ll> memo;
    
    ll Count() {
        memo.clear();
        
        int i = 0;
        int j = 0;
        while (i < chars.size()) {
            if (chars[i] == '.') {
                i++;
            } else if (chars[i] == '#') {
                i += nums[j] + 1;
                j++;
            } else {
                return Count(i, j);
            }
        }
        return 1;
    }
    
    ll Count(int char_id, int nums_id) {
        if (nums_id >= nums.size()) {
            for (int i = char_id; i < chars.size(); i++) {
                if (chars[i] == '#') return 0;
            }
            return 1;
        } else if (char_id >= chars.size()) {
            return 0;
        }
        
        if (memo.count(ii(char_id, nums_id))) {
            return memo[ii(char_id, nums_id)];
        }
        
        ll& ans = memo[ii(char_id, nums_id)];
        
        if (chars[char_id] == '.') {
            return ans = Count(char_id + 1, nums_id);
        }
        if (chars[char_id] == '#') {
            int n = nums[nums_id];
            for (int i = 0; i < n; i++) {
                if (char_id + i >= chars.size()) return ans = 0;
                if (chars[char_id + i] == '.') return ans = 0;
            }
            if (char_id + n < chars.size() && chars[char_id + n] == '#') {
                return ans = 0;
            }
            return ans = Count(char_id + n + 1, nums_id + 1);
        }
        
        ll count = Count(char_id + 1, nums_id);
        
        int n = nums[nums_id];
        for (int i = 0; i < n; i++) {
            if (char_id + i >= chars.size()) return ans = count;
            if (chars[char_id + i] == '.') return ans = count;
        }
        if (char_id + n < chars.size() && chars[char_id + n] == '#') {
            return ans = count;
        }
        return ans = count + Count(char_id + n + 1, nums_id + 1);
    }
};

Case ParseCase(const string& line) {
    int i = line.find(" ");
    string chars = line.substr(0, i);
    
    vector<int> nums;
    i++;
    while (i < line.size()) {
        int j;
        for (j = i; j < line.size() && isdigit(line[j]); j++) { }
        nums.push_back(stoi(line.substr(i, j - i)));
        i = j + 1;
    }
    
    Case c;
    c.chars = chars;
    c.nums = nums;
    return c;
}

vector<Case> ParseCases(const vector<string>& lines) {
    vector<Case> cases;
    for (const string& line : lines) {
        cases.push_back(ParseCase(line)); 
    }
    return cases;
}

void TransformCases(vector<Case>& cases) {
    for (Case& c : cases) {
        string new_chars = c.chars;
        for (int i = 0; i < 4; i++) {
            new_chars += "?" + c.chars;
        }
        c.chars = new_chars;
        
        vector<int> new_nums;
        for (int i = 0; i < 5; i++) {
            new_nums.insert(new_nums.end(), c.nums.begin(), c.nums.end());
        }
        c.nums = new_nums;
    }    
}

int main() {
    vector<Case> cases = ParseCases(ReadFile("input.txt"));
    
    ll sum = 0;
    for (Case& c : cases) {
        sum += c.Count();
    }
    printf("Answer to part 1 is %lld\n", sum);
    
    TransformCases(cases);
    sum = 0;
    for (Case& c : cases) {
        sum += c.Count();
    }
    printf("Answer to part 2 is %lld\n", sum);
    
    return 0;
}