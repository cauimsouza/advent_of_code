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

int TotalLoad(const vector<string>& grid) {
    const int n = grid.size();
    const int m = grid[0].size();
    
    int total = 0;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            if (grid[i][j] == 'O') total += n - i;
        }
    }
    
    return total;
}

enum class Direction {
    kUp = 0,
    kLeft,
    kDown,
    kRight,
    kLast,
};

void Tilt(vector<string>& grid, Direction dir) {
    if (dir == Direction::kUp) {
        for (int j = 0; j < grid[0].size(); j++) {
            int i = 0;
            for (int k = 0; k < grid.size(); k++) {
                if (grid[k][j] == '.') continue;
                else if (grid[k][j] == '#') i = k + 1;
                else if (grid[k][j] == 'O') {
                    if (k != i) {
                        grid[i][j] = 'O';
                        grid[k][j] = '.';
                    }
                    i++;
                }
            }
        }
        return;
    }
    
    if (dir == Direction::kDown) {
        for (int j = 0; j < grid[0].size(); j++) {
            int i = grid.size() - 1;
            for (int k = i; k >= 0; k--) {
                if (grid[k][j] == '.') continue;
                else if (grid[k][j] == '#') i = k - 1;
                else if (grid[k][j] == 'O') {
                    if (k != i) {
                        grid[i][j] = 'O';
                        grid[k][j] = '.';
                    }
                    i--;
                }
            }
        }
        return;
    }
    
    if (dir == Direction::kLeft) {
        for (int i = 0; i < grid.size(); i++) {
            int j = 0;
            for (int k = 0; k < grid[0].size(); k++) {
                if (grid[i][k] == '.') continue;
                else if (grid[i][k] == '#') j = k + 1;
                else if (grid[i][k] == 'O') {
                    if (k != j) {
                        grid[i][j] = 'O';
                        grid[i][k] = '.';
                    }
                    j++;
                }
            }
        }
        return;
    }
    
    if (dir == Direction::kRight) {
        for (int i = 0; i < grid.size(); i++) {
            int j = grid[0].size() - 1;
            for (int k = j; k >= 0; k--) {
                if (grid[i][k] == '.') continue;
                else if (grid[i][k] == '#') j = k - 1;
                else if (grid[i][k] == 'O') {
                    if (k != j) {
                        grid[i][j] = 'O';
                        grid[i][k] = '.';
                    }
                    j--;
                }
            }
        }
        return;
    }
}

void TiltCycle(vector<string>& grid) {
    Direction dir = Direction::kUp;
    while (true) {
       Tilt(grid, dir); 
       
       if (dir == Direction::kUp) dir = Direction::kLeft;
       else if (dir == Direction::kLeft) dir = Direction::kDown;
       else if (dir == Direction::kDown) dir = Direction::kRight;
       else break;
    }
}

int SolvePart1(const vector<string>& grid) {
    vector<string> grid_cp = grid;
    Tilt(grid_cp, Direction::kUp);  
    return TotalLoad(grid_cp);
}

void SolvePart2(const vector<string>& grid) {
    vector<string> grid_cp = grid;
    
    printf("#0: %d\n", TotalLoad(grid_cp));
    
    for (int i = 0; i < 1000; i++) {
        TiltCycle(grid_cp);
        printf("%d: %d\n", i + 1, TotalLoad(grid_cp));
    }
}

void PrintGrid(const vector<string>& grid) {
    for (const string& line : grid) {
        cout << line << endl;
    }
}

int main() {
    vector<string> grid = ReadFile("input.txt");
    
    int ans1 = SolvePart1(grid);
    printf("Answer to part 1 is %d\n", ans1);
    
    SolvePart2(grid);
    /*
    for (int i = 0; i < 1000000000; i++) {
        TiltCycle(grid);
    }
    printf("Answer to part 2 is %d\n", TotalLoad(grid));
    */
    
    return 0;
}