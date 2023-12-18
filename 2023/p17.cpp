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

using five_tp = tuple<int, int, int, Dir, int>;
using four_tp = tuple<int, int, Dir, int>;
using coordt = pair<int, int>;

Dir TurnLeft(Dir dir) {
    switch (dir) {
        case Left:
            return Down;
        case Down:
            return Right;
        case Right:
            return Up;
        case Up:
            return Left;
    }
    printf("Error");
    exit(1);
}

Dir TurnRight(Dir dir) {
    switch (dir) {
        case Left:
            return Up;
        case Up:
            return Right;
        case Right:
            return Down;
        case Down:
            return Left;
    }
    printf("Error");
    exit(1);
}

coordt Advance(coordt coord, Dir dir, int nsteps) {
    auto [i, j] = coord;

    switch (dir) {
        case Left:
            return coordt(i, j - nsteps);
        case Right:
            return coordt(i, j + nsteps);
        case Up:
            return coordt(i - nsteps, j);
        case Down:
            return coordt(i + nsteps, j);
    }
    printf("Error");
    exit(1);
}

vector<five_tp> GetNeighbours(coordt node, Dir dir, int rem, int min_same_dir, int max_same_dir) {
    vector<five_tp> neighbours;

    if (rem > 0) {
        auto [ii, jj] = Advance(node, dir, 1);
        neighbours.push_back(five_tp(1, ii, jj, dir, rem - 1));
    }

    coordt neighbour = Advance(node, TurnLeft(dir), min_same_dir);
    neighbours.push_back(five_tp(min_same_dir, neighbour.first, neighbour.second, TurnLeft(dir), max_same_dir - min_same_dir));

    neighbour = Advance(node, TurnRight(dir), min_same_dir);
    neighbours.push_back(five_tp(min_same_dir, neighbour.first, neighbour.second, TurnRight(dir), max_same_dir - min_same_dir));

    return neighbours;
}

int GetDistance(const vector<string>& grid, int min_same_dir, int max_same_dir) {
    const int n = grid.size();
    const int m = grid[0].size();

    map<four_tp, int> dists;
    priority_queue<five_tp, vector<five_tp>, greater<five_tp>> pq;

    int d = 0;
    for (int j = 1; j <= min_same_dir; j++) {
        d += grid[0][j] - '0';
    }
    dists[four_tp(0, min_same_dir, Right, max_same_dir - min_same_dir)] = d;
    pq.push(five_tp(d, 0, min_same_dir, Right, max_same_dir - min_same_dir));

    d = 0;
    for (int i = 1; i <= min_same_dir; i++) {
        d += grid[i][0] - '0';
    }
    dists[four_tp(min_same_dir, 0, Down, max_same_dir - min_same_dir)] = d;
    pq.push(five_tp(d, min_same_dir, 0, Down, max_same_dir - min_same_dir));

    while (!pq.empty()) {
        auto [dist, i, j, dir, rem] = pq.top();
        pq.pop();

        if (dist > dists[four_tp(i, j, dir, rem)]) continue;

        if (i == n - 1 && j == m - 1) return dist;

        vector<five_tp> neighbours = GetNeighbours(coordt(i, j), dir, rem, min_same_dir, max_same_dir);
        for (const five_tp& neighbour : neighbours) {
            auto [nsteps, ii, jj, dir, rem] = neighbour;

            if (ii < 0 || ii >= n || jj < 0 || jj >= m) continue;

            int d = dist;
            for (int k = 1; k <= nsteps; k++) {
                auto [a, b] = Advance(coordt(i, j), dir, k);
                d += grid[a][b] - '0';
            }

            four_tp dists_node = four_tp(ii, jj, dir, rem);
            if (dists.count(dists_node) > 0 && dists[dists_node] <= d) continue;

            dists[dists_node] = d;
            pq.push(five_tp(d, ii, jj, dir, rem));
        }
    }

    return 0;
}

int main() {
    vector<string> grid = ReadFile("input.txt");
    printf("Answer to part 1 is %d\n", GetDistance(grid, 1, 3));
    printf("Answer to part 2 is %d\n", GetDistance(grid, 4, 10));

    return 0;
}