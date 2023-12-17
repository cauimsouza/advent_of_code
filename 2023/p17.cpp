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
    printf("Error");
    exit(1);
}

vector<four_tp> GetNeighbours(coordt node, Dir dir, int rem) {
    vector<four_tp> neighbours;

    if (rem > 0) {
        auto [ii, jj] = Advance(node, dir);
        neighbours.push_back(four_tp(ii, jj, dir, rem - 1));
    }

    coordt neighbour = Advance(node, TurnLeft(dir));
    neighbours.push_back(four_tp(neighbour.first, neighbour.second, TurnLeft(dir), 2));

    neighbour = Advance(node, TurnRight(dir));
    neighbours.push_back(four_tp(neighbour.first, neighbour.second, TurnRight(dir), 2));

    return neighbours;
}

int GetDistance(const vector<string>& grid) {
    const int n = grid.size();
    const int m = grid[0].size();

    map<four_tp, int> dists;
    priority_queue<five_tp, vector<five_tp>, greater<five_tp>> pq;

    pq.push(five_tp(0, 0, 0, Right, 3));
    pq.push(five_tp(0, 0, 0, Down, 3));
    dists[four_tp(0, 0, Right, 3)] = 0;
    dists[four_tp(0, 0, Down, 3)] = 0;

    while (!pq.empty()) {
        auto [dist, i, j, dir, rem] = pq.top();
        pq.pop();

        if (dist > dists[four_tp(i, j, dir, rem)]) continue;

        if (i == n - 1 && j == m - 1) return dist;

        vector<four_tp> neighbours = GetNeighbours(coordt(i, j), dir, rem);
        for (const four_tp& neighbour : neighbours) {
            auto [ii, jj, dir, rem] = neighbour;

            if (ii < 0 || ii >= n || jj < 0 || jj >= m) continue;

            int d = dist + grid[ii][jj] - '0';
            if (dists.count(neighbour) > 0 && dists[neighbour] <= d) continue;

            dists[neighbour] = d;
            pq.push(five_tp(d, ii, jj, dir, rem));
        }
    }

    return 0;
}

int main() {
    vector<string> grid = ReadFile("input.txt");
    printf("Answer to part 1 is %d\n", GetDistance(grid));

    return 0;
}