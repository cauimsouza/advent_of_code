#include <bits/stdc++.h>

using namespace std;

using ll = long long;

struct MapRange {
    ll dst;
    ll src;
    ll len;
};

class RangeMapper;
class ProductMapper;

class Mapper {
public:
    virtual ll Map(ll n) const = 0;
    
    ll FindMinInterval(ll start, ll length) {
        set<ll> points = TransitionPoints(start, length);
        ll ans = numeric_limits<ll>::max();
        for (ll point : points) {
            ans = min(ans, Map(point));
        }
        return ans;
    }

protected:
    virtual set<ll> TransitionPoints() const = 0;
    
    set<ll> TransitionPoints(ll start, ll length) const {
        set<ll> points;
        points.insert(start);
        points.insert(start + length - 1);
        
        set<ll> tpoints = TransitionPoints();
        for (auto it = tpoints.lower_bound(start); it != tpoints.end() && *it < start + length; it++) {
            points.insert(*it);
        }
        return points; 
    }
    
    friend class RangeMapper;
    friend class ProductMapper;
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

        it--;
        if (n >= it->first + it->second.len) {
            return n;
        } 
        return (n - it->first) + it->second.dst;
    }
    

protected:
    set<ll> TransitionPoints() const {
        set<ll> points;
        for (const auto& [src, dstlen] : src2dstlen_) {
            points.insert(src);
        }
        return points;
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
    
protected:
    set<ll> TransitionPoints() const {
        set<ll> pointsa = a_->TransitionPoints();
        set<ll> pointsb = b_->TransitionPoints();
        pointsa.insert(pointsb.begin(), pointsb.end());
        return pointsa;
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

vector<pair<ll, ll>> SeedsToSeedIntervals(const vector<ll>& seeds) {
    vector<pair<ll, ll>> seed_intervals;
    for (int i = 0; i < seeds.size(); i += 2) {
        seed_intervals.push_back(make_pair(seeds[i], seeds[i + 1]));
    }
    return seed_intervals;
}

using ii = pair<int, int>;

int main() {
    const auto [seeds, mapper] = ReadInput("input_p5.txt");

    ll ans1 = numeric_limits<ll>::max();
    for (ll seed : seeds) {
        ans1 = min(ans1, mapper->Map(seed));
    }
    printf("Answer to part 1 is %lld\n", ans1);
    
    ll ans2 = numeric_limits<ll>::max();
    vector<pair<ll, ll>> seed_intervals = SeedsToSeedIntervals(seeds); 
    for (const auto& [start, len] : seed_intervals) {
        ans2 = min(ans2, mapper->FindMinInterval(start, len));
    }
    printf("Answer to part 2 is %lld\n", ans2);

    return 0;
}