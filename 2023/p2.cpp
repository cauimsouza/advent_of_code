#include <bits/stdc++.h>

using namespace std;

vector<string> ReadFile(const string& filename) {
    ifstream is(filename);

    vector<string> content;
    string s;
    while (getline(is, s)) {
        content.push_back(s);
    }

    return content;
}

struct Draw {
    int red;
    int green;
    int blue;

    Draw() : red(0), green(0), blue(0) {}
};

class Game {
private:
    int id_;
    vector<Draw> draws_;

public:
    Game(int id) : id_(id) {}

    int GetID() const {
        return id_;
    }

    vector<Draw> GetDraws() const {
        return draws_;
    }

    void AddDraw(const Draw& draw) {
        draws_.push_back(draw);
    }

    int Power() const {
        int red = 0;
        int green = 0;
        int blue = 0;
        for (const Draw& draw : draws_) {
            red = max(red, draw.red);
            green = max(green, draw.green);
            blue = max(blue, draw.blue);
        }
        return red * green * blue;
    }
};

Game ParseGame(const string& line) {
    int i = line.find(' '); i++;
    int j = line.find(':', i);
    int id = stoi(line.substr(i, j - i));
    Game game(id);

    for (i = j + 2; i < line.size();) {
        Draw draw;
        while (true) {
            for (j = i + 1; line[j] != ' '; j++) {}
            int n = stoi(line.substr(i, j - i));
            i = j + 1;

            for (j = i + 1; j < line.size() && isalpha(line[j]); j++) {}
            string colour = line.substr(i, j - i);
            i = j + 2;

            if (colour == "red") draw.red = n;
            else if (colour == "green") draw.green = n;
            else draw.blue = n;

            if (j == line.size() || line[j] == ';') break;
        }
        game.AddDraw(draw);
    }

    return game;
}

vector<Game> ParseGames(const vector<string>& lines) {
    vector<Game> games;
    for (const string& line : lines) {
        games.push_back(ParseGame(line));
    }
    return games;
}

bool Possible(const Game& game) {
    for (const Draw& draw : game.GetDraws()) {
        if (draw.red > 12 || draw.green > 13 || draw.blue > 14) return false;
    }
    return true;
}

int main() {
    vector<string> lines = ReadFile("input_p2.txt");
    vector<Game> games = ParseGames(lines);

    int ans1 = 0;
    for (const Game& game : games) {
        if (Possible(game)) {
            ans1 += game.GetID();
        }
    }

    int ans2 = 0;
    for (const Game& game : games) {
        ans2 += game.Power();
    }

    printf("Answer to part 1 is %d\nAnswer to part 2 is %d\n", ans1, ans2);

    return 0;
}