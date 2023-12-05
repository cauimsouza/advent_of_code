#include <bits/stdc++.h>

using namespace std;

using ll = long long;

struct MapRange {
    ll dst;
    ll src;
    ll len;
};

class Mapper {
public:
    virtual ll Map(ll n) const = 0;
};

class RangeMapper : public Mapper {
public:
    RangeMapper(const vector<MapRange> ranges) {
        for (const MapRange& range : ranges) {
            DstLen dst_len;
            dst_len.dst = range.dst;
            dst_len.len = range.len;

            src2dstlen_[range.src] = dst_len;
        }
    }

    ll Map(ll n) const {
        auto it = src2dstlen_.upper_bound(n);

        if (it == src2dstlen_.begin()) {
            return n;
        }

        it = prev(it);
        if (n >= it->first + it->second.len) {
            return n;
        } 
        return (n - it->first) + it->second.dst;
    }

private:
    struct DstLen {
        ll dst;
        ll len;
    };
    map<ll, DstLen> src2dstlen_;
};

class ProductMapper : public Mapper {
public:
    ProductMapper(const Mapper* a, const Mapper* b) : a_(a), b_(b) { }

    ll Map(ll n) const {
        return a_->Map(b_->Map(n));
    }

private:
    const Mapper* a_;
    const Mapper* b_; 
};

Mapper* ComposeMappers(const vector<Mapper*> mappers) {
    if (mappers.empty()) {
        return new RangeMapper(vector<MapRange> ());
    }

    if (mappers.size() == 1) {
        return mappers[0];
    }

    Mapper* res = new ProductMapper(mappers[1], mappers[0]);
    for (int i = 2; i < mappers.size(); i++) {
        res = new ProductMapper(mappers[i], res);
    }
    return res;
}

vector<ll> ReadSeedsLine(ifstream& is) {
    string line;
    getline(is, line);

    vector<ll> seeds;
    int i = 7; // Skip "seeds: "
    while (i < line.size()) {
        int j;
        for (j = i; j < line.size() && isdigit(line[j]); j++) {}
        seeds.push_back(stoll(line.substr(i, j - i)));

        i = j + 1;
    }

    return seeds;
}

Mapper* ReadMapperLines(ifstream& is) {
    string s;
    getline(is, s);

    vector<MapRange> ranges;
    for (getline(is, s); !s.empty(); getline(is, s)) {
        istringstream iss(s);

        MapRange range;
        iss >> range.dst >> range.src >> range.len;
        ranges.push_back(range);
    }

    return new RangeMapper(ranges);
}

pair<vector<ll>, Mapper*> ReadInput(const string& filename) {
    ifstream is(filename);

    vector<ll> seeds = ReadSeedsLine(is);
    { string s; getline(is, s); }

    vector<Mapper*> mappers;
    for (int i = 0; i < 7; i++) {
        mappers.push_back(ReadMapperLines(is));
    }

    return make_pair(seeds, ComposeMappers(mappers));
}

int main() {
    const auto [seeds, mapper] = ReadInput("input_p5.txt");

    ll min_location = numeric_limits<ll>::max();
    for (ll seed : seeds) {
        min_location = min(min_location, mapper->Map(seed));
    }

    printf("Answer to part 1 is %lld\n", min_location);

    return 0;
}