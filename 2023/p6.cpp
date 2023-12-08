#include <bits/stdc++.h>

using namespace std;

using ll = long long;

ll WaysWin(ll time, ll record) {
    ll n = 0; 
    for (ll i = 1; i < time; i++) {
        ll dist = (time - i) * i;
        if (dist > record) n++;
    }
    return n;
}

int main() {
    vector<ll> times = { 40, 82, 84, 92 };
    vector<ll> records = { 233, 1011, 1110, 1487 };
    ll ans1 = 1;
    for (int i = 0; i < times.size(); i++) {
        ans1 *= WaysWin(times[i], records[i]);
    }
    printf("Answer to part 1 is %d\n", ans1);
    
    printf("Answer to part 2 is %lld\n", WaysWin(40828492LL, 233101111101487LL));
    
    return 0;
}