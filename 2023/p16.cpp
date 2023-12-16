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
    Up = 0,
    Left,
    Down,
    Right,
};

struct CellDir {
    int i;
    int j;
    Dir dir;

    CellDir(int i, int j, Dir dir) : i(i), j(j), dir(dir) {}

    vector<CellDir> Next(char c);
};

vector<CellDir> CellDir::Next(char c) {
    if (c == '.') {
        int ii, jj;
        switch (dir) {
            case Up:
                ii = i - 1;
                jj = j;
                break;
            case Left:
                ii = i;
                jj = j - 1;
                break;
            case Down:
                ii = i + 1;
                jj = j;
                break;
            case Right:
                ii = i;
                jj = j + 1;
                break;
            default:
                // Should never get here
                printf("A for %d, %d, %d, %c\n", i, j, dir, c);
                exit(1);
        }
        return vector<CellDir> { CellDir(ii, jj, dir) };
    } else if (c == '-' && (dir == Left || dir == Right)) {
        int ii = i;
        int jj = dir == Left ? j - 1 : j + 1;
        return vector<CellDir> { CellDir(ii, jj, dir) };
    } else if (c == '|' && (dir == Up || dir == Down)) {
        int jj = j;
        int ii = dir == Up ? i - 1 : i + 1;
        return vector<CellDir> { CellDir(ii, jj, dir) };
    } else if (c == '-') {
        return vector<CellDir> { CellDir(i, j - 1, Left), CellDir(i, j + 1, Right) };
    } else if (c == '|') {
        return vector<CellDir> { CellDir(i - 1, j, Up), CellDir(i + 1, j, Down) };
    } else if (c == '\\') {
        int ii, jj;
        Dir dd;
        switch (dir) {
            case Left:
                ii = i - 1;
                jj = j;
                dd = Up;
                break;
            case Right:
                ii = i + 1;
                jj = j;
                dd = Down;
                break;
            case Up:
                ii = i;
                jj = j - 1;
                dd = Left;
                break;
            case Down:
                ii = i;
                jj = j + 1;
                dd = Right;
                break;
            default:
                // Should never get here
                printf("B for %d, %d, %d, %c\n", i, j, dir, c);
                exit(1);
        }
        return vector<CellDir> { CellDir(ii, jj, dd) };
    } else { // c == '/'
        int ii, jj;
        Dir dd;
        switch (dir) {
            case Left:
                ii = i + 1;
                jj = j;
                dd = Down;
                break;
            case Right:
                ii = i - 1;
                jj = j;
                dd = Up;
                break;
            case Up:
                ii = i;
                jj = j + 1;
                dd = Right;
                break;
            case Down:
                ii = i;
                jj = j - 1;
                dd = Left;
                break;
            default:
                // Should never get here
                printf("C for %d, %d, %d, %c\n", i, j, dir, c);
                exit(1);
        }
        return vector<CellDir> { CellDir(ii, jj, dd) };
    }

    // Should never get here
    exit(1);
}

int NumEnergised(const vector<string>& grid, CellDir start) {
    const int n = grid.size();
    const int m = grid[0].size();

    vector<vector<vector<bool>>> visited(n, vector<vector<bool>> (m, vector<bool> (4, false)));
    queue<CellDir> q;

    q.push(start);
    visited[start.i][start.j][start.dir] = true;
    while (!q.empty()) {
        CellDir celldir = q.front();
        q.pop();

        vector<CellDir> cell_dirs = celldir.Next(grid[celldir.i][celldir.j]);
        for (const CellDir& cd : cell_dirs) {
            auto [i, j, dir] = cd;

            if (i < 0 || i >= n || j < 0 || j >= m) continue;
            if (visited[i][j][dir]) continue;

            visited[i][j][dir] = true;
            q.push(cd);
        }
    }

    int nenergised = 0;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            if (any_of(visited[i][j].begin(), visited[i][j].end(), [](bool b) { return b; })) {
                nenergised++;
            }
        }
    }
    return nenergised;
}

int SolvePart2(const vector<string>& grid) {
    int ans = 0;

    for (int i = 0; i < grid.size(); i++) {
        ans = max(ans, NumEnergised(grid, CellDir(i, 0, Right)));
        ans = max(ans, NumEnergised(grid, CellDir(i, grid[0].size() - 1, Left)));
    }

    for (int j = 0; j < grid[0].size(); j++) {
        ans = max(ans, NumEnergised(grid, CellDir(0, j, Down)));
        ans = max(ans, NumEnergised(grid, CellDir(grid.size() - 1, j, Up)));
    }
    
    return ans;
}

int main() {
    vector<string> grid = ReadFile("input.txt");
    printf("Answer to part 1 is %d\n", NumEnergised(grid, CellDir(0, 0, Right)));
    printf("Answer to part 2 is %d\n", SolvePart2(grid));

    return 0;
}