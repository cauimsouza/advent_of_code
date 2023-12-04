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

class Card {
public:
    Card(const vector<string>& winning, const vector<string>& hand) : winning_(winning), hand_(hand) {}
    
    int GetPoints() const {
        int n = GetNumMatches();
        if (n == 0) return 0;
        return pow(2, n - 1);
    }
    
    int GetNumMatches() const {
        unordered_set<string> winnings(winning_.begin(), winning_.end());
        
        int n = 0;
        for (const string& h : hand_) {
            if (winnings.count(h) > 0) n++;
        }
        
        return n;
    }
    
private:
    vector<string> winning_;
    vector<string> hand_;
};

Card ParseCardLine(const string& line) {
    istringstream is(line);
    string s;
    
    is >> s; is >> s; // Parse the "Card n:" start
    
    vector<string> winning;
    while (is >> s) {
        if (s == "|") break;
        winning.push_back(s);
    }
    
    vector<string> hand;
    while (is >> s) {
        hand.push_back(s);
    }
    
    return Card(winning, hand);
}

int TotalNumCards(const vector<Card>& cards) {
    vector<int> copies(cards.size(), 1);
    for (int i = 0; i < cards.size(); i++) {
       int n = cards[i].GetNumMatches(); 
       for (int j = 1; j <= n && i + j < cards.size(); j++) {
           copies[i + j] += copies[i];
       }
    }
    
    int ncards = 0;
    for (int c : copies) {
        ncards += c;
    }
    return ncards;
}

int main() {
    vector<string> lines = ReadFile("input_p4.txt");
    
    vector<Card> cards;
    for (const string& line : lines) {
        cards.push_back(ParseCardLine(line));
    }
    
    int n = 0;
    for (const Card& card : cards) {
        n += card.GetPoints();
    }
    
    printf("Answer to part 1 is %d\n", n);
    printf("Answer to part 2 is %d\n", TotalNumCards(cards));
    
    return 0;
}