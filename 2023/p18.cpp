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

enum Dir {
    Left = 0,
    Right,
    Up,
    Down,
};

using coordt = pair<int, int>;

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

struct DirStepsColour {
    Dir dir;
    int steps;
    string colour;
};

DirStepsColour ParseLine(const string& line) {
    DirStepsColour dsc;

    istringstream is(line);

    string s;
    is >> s;
    if (s == "R") {
        dsc.dir = Right;
    } else if (s == "L") {
        dsc.dir = Left;
    } else if (s == "D") {
        dsc.dir = Down;
    } else {
        dsc.dir = Up;
    }

    is >> dsc.steps >> dsc.colour;

    return dsc;
}

vector<DirStepsColour> ParseLines(const vector<string>& lines) {
    vector<DirStepsColour> dscs;
    for (const string& line : lines) {
        dscs.push_back(ParseLine(line));
    }
    return dscs;
}

using dimt = tuple<int, int, int, int>; // left, right, up, down

dimt FindDim(const vector<DirStepsColour>& dscs) {
    int left = 0;
    int right = 0;
    int up = 0;
    int down = 0;

    int i = 0;
    int j = 0;
    for (const DirStepsColour& dsc : dscs) {
        switch (dsc.dir) {
            case Left:
                j -= dsc.steps;
                break;
            case Right:
                j += dsc.steps;
                break;
            case Up:
                i -= dsc.steps;
                break;
            case Down:
                i += dsc.steps;
                break;
        }

        left = min(left, j);
        right = max(right, j);
        up = min(up, i);
        down = max(down, i);
    }
    return dimt(left, right, up, down);
}

vector<vector<char>> BuildGrid(dimt dim, const vector<DirStepsColour>& dscs) {
    auto [left, right, up, down] = dim;
    int nrows = down - up + 1;
    int ncols = right - left + 1;

    vector<vector<char>> grid(nrows, vector<char> (ncols, '.'));

    int i = -up;
    int j = -left;
    grid[i][j] = '#';
    for (const DirStepsColour& dsc : dscs) {
        Dir dir = dsc.dir;
        for (int k = 1; k <= dsc.steps; k++) {
            coordt coord = Advance(coordt(i, j), dir);
            i = coord.first;
            j = coord.second;
            grid[i][j] = '#';
        } 
    }

    return grid;
}

int SolvePart1(const vector<vector<char>>& grid) {
    int nrows = grid.size();
    int ncols = grid[0].size();

    int i = 1;
    int j = 0;
    while (grid[0][j] != '#') j++;
    j++;

    vector<vector<bool>> visited(nrows, vector<bool> (ncols, false));
    queue<coordt> q;

    visited[i][j] = true;
    q.push(coordt(i, j));
    int n = 1;
    while (!q.empty()) {
        auto [i, j] = q.front();
        q.pop();

        int di[] = {-1, 0, 1, 0};
        int dj[] = {0, 1, 0, -1};
        for (int k = 0; k < 4; k++) {
            int ii = i + di[k];
            int jj = j + dj[k];

            if (ii < 0 || ii >= nrows || jj < 0 || jj >= ncols) continue; 
            if (visited[ii][jj]) continue;
            if (grid[i][j] == '#' && grid[ii][jj] == '.') continue;

            visited[ii][jj] = true;
            q.push(coordt(ii, jj));
            n++;
        }
    } 

    return n;
}

int main() {
    vector<string> lines = ReadFile("input.txt");
    vector<DirStepsColour> steps = ParseLines(lines);

    dimt dim = FindDim(steps);

    vector<vector<char>> grid = BuildGrid(dim, steps);

    /*
    for (const vector<char>& line : grid) {
        for (char c : line) putchar(c);
        putchar('\n');
    }
    putchar('\n');
    */
   printf("Answer to part 1 is %d\n", SolvePart1(grid));

    return 0;
}