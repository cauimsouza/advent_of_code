#include <bits/stdc++.h>

using namespace std;

map<char, int> order = {
    {'J', 1},
    {'2', 2},
    {'3', 3},
    {'4', 4},
    {'5', 5},
    {'6', 6},
    {'7', 7},
    {'8', 8},
    {'9', 9},
    {'T', 10},
    {'Q', 11},
    {'K', 12},
    {'A', 13},
};

bool LabelCmp(char a, char b) {
    return order[a] < order[b];
}

enum HandType {
    HIGH,
    ONE_PAIR,
    TWO_PAIR,
    THREE,
    FULL_HOUSE,
    FOUR,
    FIVE,
};

struct Hand {
    string hand;
    HandType type;
    int bid;
    
    bool operator<(const Hand& other) {
        if (type != other.type) {
            return type < other.type;
        }
        for (int i = 0; i < hand.size(); i++) {
            if (hand[i] == other.hand[i]) continue;
            return LabelCmp(hand[i], other.hand[i]);
        }
        return false; // should never get here
    }
};

HandType Type(const string& hand) {
    map<char, int> freq;
    for (char c : hand) {
        freq[c]++;
    }
    
    vector<int> types(6, 0);
    int njokers = freq['J'];
    types[0] = 1;
    for (const auto& [c, f] : freq) {
        if (c == 'J') continue;
        types[f]++;
    }
    
    if (njokers >= 4) return FIVE;
    if (njokers == 3) {
        if (types[2]) return FIVE;
        return FOUR;
    }
    if (njokers == 2) {
        if (types[3]) return FIVE;
        if (types[2]) return FOUR;
        return THREE;
    }
    if (njokers == 1) {
        if (types[4]) return FIVE;
        if (types[3]) return FOUR;
        if (types[2] >= 2) return FULL_HOUSE;
        if (types[2]) return THREE;
        return ONE_PAIR;
    }
    
    if (types[5]) return FIVE;
    if (types[4]) return FOUR;
    if (types[3] && types[2]) return FULL_HOUSE;
    if (types[3]) return THREE;
    if (types[2] == 2) return TWO_PAIR;
    if (types[2]) return ONE_PAIR;
    return HIGH;
}

Hand ParseHand(const string& hand, int bid) {
    Hand h;
    h.hand = hand;
    h.type = Type(hand);
    h.bid = bid;
    
    return h;
}

vector<string> ReadFile(const string& filename) {
    ifstream is(filename);
    
    vector<string> lines;
    string s;
    while (getline(is, s)) {
        lines.push_back(s);
    }
    
    return lines;
}

vector<Hand> ParseInput(const vector<string>& lines) {
    vector<Hand> hands;
    
    for (const string& line : lines) {
        istringstream is(line);
        
        string hand;
        int bid;
        is >> hand >> bid;
        
        hands.push_back(ParseHand(hand, bid));
    }
    
    return hands;
}

int main() {
    vector<string> input = ReadFile("input_p7.txt");
    vector<Hand> hands = ParseInput(input);
    
    sort(hands.begin(), hands.end());
    
    int ans1 = 0;
    for (int i = 0; i < hands.size(); i++) {
        ans1 += (i + 1) * hands[i].bid;
    }
    printf("Answer to part 1 is %d\n", ans1);
    
    return 0;
}