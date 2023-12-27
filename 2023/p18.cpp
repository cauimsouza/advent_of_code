#include <bits/stdc++.h>

using namespace std;

using coordt = pair<int, int>;
using ll = long long;

vector<string> ReadFile(const string& filename) {
    ifstream is(filename);
    
    vector<string> lines;
    string s;
    while (getline(is, s)) {
        lines.push_back(s);
    }
    
    return lines;
}

enum Dir {
    Left = 0,
    Right,
    Up,
    Down,
};

coordt Advance(coordt coord, Dir dir) {
    auto [i, j] = coord;
    switch (dir) {
        case Left:
            return coordt(i, j - 1);
        case Right:
            return coordt(i, j + 1);
        case Up:
            return coordt(i - 1, j);
        case Down:
            return coordt(i + 1, j);
    }
    printf("error\n");
    exit(1);
}

struct Move {
    Dir dir;
    int steps;
    
    Move(Dir dir, int steps) : dir(dir), steps(steps) {}
    
    Move(const string& hash) {
        switch (hash[7]) {
            case '0':
                dir = Right;
                break;
            case '1':
                dir = Down;
                break;
            case '2':
                dir = Left;
                break;
            case '3':
                dir = Up;
                break;
        }
        
        steps = 0;
        for (int i = 2; i < 7; i++) {
            steps *= 16;
            steps += isdigit(hash[i]) ? hash[i] - '0' : hash[i] - 'a' + 10;
        }
    }
};

Move ParseLine(const string& line) {
    istringstream is(line);

    string s;
    is >> s;
    Dir dir;
    if (s == "R") {
        dir = Right;
    } else if (s == "L") {
        dir = Left;
    } else if (s == "D") {
        dir = Down;
    } else {
        dir = Up;
    }

    int steps;
    is >> steps;
    
    is >> s;

    return Move(dir, steps);
}

Move ParseLineHash(const string& line) {
    istringstream is(line);
    string s;
    is >> s;
    is >> s;
    is >> s;
    return Move(s);
}

vector<Move> ParseLines(const vector<string>& lines) {
    vector<Move> moves;
    for (const string& line : lines) {
        moves.push_back(ParseLine(line));
    }
    return moves;
}

vector<Move> ParseLinesHash(const vector<string>& lines) {
    vector<Move> moves;
    for (const string& line : lines) {
        moves.push_back(ParseLineHash(line));
    }
    return moves;
}

ll Area(const vector<Move>& moves) {
    ll x = 0;
    ll y = 0;
    
    ll perimeter = 0;
    ll sum = 0;
    for (const Move& move : moves) {
        Dir dir = move.dir;
        ll steps = move.steps;
        
        perimeter += steps;
        
        switch (dir) {
            case Left:
                sum += steps * y;
                x -= steps;
                break;
            case Right:
                sum += -steps * y;
                x += steps;
                break;
            case Up:
                sum += -x * steps;
                y += -steps;
                break;
            case Down:
                sum += steps * x;
                y += steps;
                break;
        }
    }
    
    return (abs(sum) + perimeter - 4) / 2 + 3;
}

int main() {
    vector<string> lines = ReadFile("input.txt");
    printf("Answer to part 1 is %lld\n", Area(ParseLines(lines)));
    printf("Answer to part 2 is %lld\n", Area(ParseLinesHash(lines)));

    return 0;
}