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

int Hash(const string& line, int start, int end) {
    int h = 0;
    for (int i = start; i <= end; i++) {
        h = ((h + line[i]) * 17) % 256;
    }
    return h;
}

int SolvePart1(const string& line) {
    int sum = 0;
    int i = 0;
    int j = 0;
    while (j < line.size()) {
        if (line[j] == ',') {
            sum += Hash(line, i, j - 1);
            i = j + 1;
        }
        j++;
    }
    sum += Hash(line, i, j - 1);
    return sum;
}

class HashMap {
private: 
    struct Node {
        const string key;
        int val;
        Node* next;
        
        Node(const string& key, int val) : key(key), val(val), next(nullptr) {}
    };
    vector<Node*> buckets;
    
public:
    HashMap(int nbuckets) : buckets(nbuckets, nullptr) {
        string s = "";
        for (int i = 0; i < nbuckets; i++) {
            Node *hd = new Node(s, 0);
            buckets[i] = hd;
        } 
    }
    
    ~HashMap() {
        for (int i = 0; i < buckets.size(); i++) {
            Node* node = buckets[i];
            while (node->next != nullptr) {
                Node* next = node->next;
                delete node;
                node = next;
            }
            delete node;
        }
    }
    
    void Insert(const string& key, int val) {
        int h = Hash(key, 0, key.size() - 1); 
        
        Node* node = buckets[h];
        
        while (node->next != nullptr) {
            Node* next = node->next;
            if (next->key == key) {
                next->val = val;
                return;
            }
            node = next;
        }
        
        node->next = new Node(key, val);
    }
    
    void Delete(const string& key) {
        int h = Hash(key, 0, key.size() - 1);
        Node* node = buckets[h];
        while (node->next != nullptr) {
            Node* next = node->next;
            if (next->key == key) {
                node->next = next->next;
                delete next;
                return;
            }
            node = next;
        }
    }
    
    int SumFocusingPower() const {
        int sum = 0;
        for (int i = 0; i < buckets.size(); i++) {
            int j = 1;
            Node* node = buckets[i]->next; 
            while (node != nullptr) {
                sum += (i + 1) * j * node->val;
                j++;
                node = node->next;
            }
        }
        return sum;
    }
};

enum OpType {
    Insertion,
    Deletion,
};

struct Op {
    string key;
    OpType type;
    int val;
};

vector<Op> ParseOperations(const string& line) {
   vector<Op> ops; 
   
   int i = 0;
   while (i < line.size()) {
       int j = i;
       while (isalpha(line[j])) {
           j++;
       }
       
       string key = line.substr(i, j - i);
       
       if (line[j] == '-') {
           Op op;
           op.key = key;
           op.type = Deletion;
           ops.push_back(op);
           
           i = j + 2;
       } else {
          j++;
          i = j;
          while (j < line.size() && isdigit(line[j])) {
              j++;
          }
          
          Op op;
          op.key = key;
          op.type = Insertion;
          op.val = stoi(line.substr(i, j - i));
          ops.push_back(op);
          
          i = j + 1;
       }
   }
   
   return ops;
}

int SolvePart2(const vector<Op>& ops) {
    HashMap hm(256);
    
    for (const Op& op : ops) {
        if (op.type == Insertion) {
            hm.Insert(op.key, op.val);
        } else {
            hm.Delete(op.key);
        }
    }
    
    return hm.SumFocusingPower();
}

int main() {
    vector<string> lines = ReadFile("input.txt");
    printf("Answer to part 1 is %d\n", SolvePart1(lines[0]));
    printf("Answer to part 2 is %d\n", SolvePart2(ParseOperations(lines[0])));
    
    return 0;
}