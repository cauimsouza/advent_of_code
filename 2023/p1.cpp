#include <bits/stdc++.h>

using namespace std;

vector<string> ReadFile(const string& filename) {
    vector<string> content;

    ifstream is(filename);
    string line;
    while (getline(is, line)) {
        content.push_back(line);
    }

    return content;
}

optional<int> GetDigit(const string& line, int start) {
    unordered_map<string, int> digits = {
        {"zero", 0},
        {"one", 1},
        {"two", 2},
        {"three", 3},
        {"four", 4},
        {"five", 5},
        {"six", 6},
        {"seven", 7},
        {"eight", 8},
        {"nine", 9},
    };

    if (isdigit(line[start])) return line[start] - '0';

    for (const auto& [digit_str, digit] : digits) {
        if (line.substr(start, digit_str.size()) == digit_str) return digit;
    }

    return {};
}

int GetFirstDigit(const string& line) {
    for (int i = 0; i < line.size(); i++) {
        if (auto digit = GetDigit(line, i)) {
            return *digit;
        }
    }
    return 0; // Should never get here
}

int GetLastDigit(const string& line) {
    for (int i = line.size() - 1; i >= 0; i--) {
        if (auto digit = GetDigit(line, i)) {
            return *digit;
        }
    }
    return 0; // Should never get here
}

int CalibrationValue(const string& line) {
    return GetFirstDigit(line) * 10 + GetLastDigit(line);
}

int main() {
    vector<string> content = ReadFile("input_p1.txt");

    int res = 0;
    for (const string& line : content) {
        res += CalibrationValue(line);
    }

    cout << res << endl;

    return 0;
}