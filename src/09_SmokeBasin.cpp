#include <algorithm>
#include <iostream>
#include <vector>

namespace {

class Caves
{
private:
    std::vector<int> depths_;
    size_t width_;

public:
    Caves() : width_(0) {}

    size_t size() const { return depths_.size(); }

    size_t width() const { return width_; }

    size_t height() const { return size() / width(); }

    int depth_at(size_t i) const { return depths_[i]; }

    std::vector<int> risk_levels() const;

    std::vector<int> basin_sizes() const;

    template<class Func>
    void for_each_neighbour(size_t i, Func func) const;

    friend std::istream& operator>>(std::istream& in, Caves& caves);
};

std::vector<int> Caves::risk_levels() const
{
    std::vector<int> risks;

    for (size_t i = 0; i < size(); ++i) {
        int depth = depth_at(i);
        bool low_point = true;

        for_each_neighbour(i, [this, &low_point, depth](size_t j) {
            low_point &= depth < depth_at(j);
        });

        if (low_point) {
            risks.push_back(depth + 1);
        }
    }

    return risks;
}

std::vector<int> Caves::basin_sizes() const
{
    std::vector<size_t> flows(size());
    std::vector<int> sizes(size());

    for (size_t i = 0; i < size(); ++i) {
        flows[i] = i;
        sizes[i] = depth_at(i) < 9;
    }

    for (size_t i = 0; i < size(); ++i) {
        if (sizes[i] > 0) {
            for_each_neighbour(i, [&flows, &sizes, i](size_t j) {
                if (sizes[j] > 0) {
                    size_t a = i;
                    while (a != flows[a]) {
                        a = flows[a];
                    }

                    size_t b = j;
                    while (b != flows[b]) {
                        b = flows[b];
                    }

                    if (a != b) {
                        if (sizes[a] < sizes[b]) {
                            size_t c = a;
                            a = b;
                            b = c;
                        }

                        flows[b] = a;
                        sizes[a] += sizes[b];
                    }
                }
            });
        }
    }

    std::vector<int> basins;

    for (size_t i = 0; i < size(); ++i) {
        if (flows[i] == i && sizes[i] > 0) {
            basins.push_back(sizes[i]);
        }
    }

    return basins;
}

template<class Func>
void Caves::for_each_neighbour(size_t i, Func func) const
{
    if (i >= width()) {
        func(i - width());
    }
    if (i % width() >= 1) {
        func(i - 1);
    }
    if (i % width() + 1 < width()) {
        func(i + 1);
    }
    if (i + width() < size()) {
        func(i + width());
    }
}

std::istream& operator>>(std::istream& in, Caves& caves)
{
    char c;

    while (in.get(c)) {
        if (c >= '0' && c <= '9') {
            caves.depths_.push_back(c - '0');
        }
        else if (c != '\n') {
            throw std::runtime_error("Unexpected input");
        }
        else if (caves.width_ == 0) {
            caves.width_ = caves.depths_.size();
        }
        else if (caves.depths_.size() % caves.width_ != 0) {
            throw std::runtime_error("Ragged input");
        }
    }

    return in;
}

} // namespace

int
main(int argc, char **argv)
{
    Caves caves;

    std::cin >> caves;

    std::vector<int> risks(caves.risk_levels());
    int risk_sum = std::accumulate(risks.begin(), risks.end(), 0, std::plus<int>());

    std::cout << risk_sum << std::endl;

    std::vector<int> basins(caves.basin_sizes());
    std::sort(basins.begin(), basins.end(), std::greater<int>());
    int basin_product = std::accumulate(basins.begin(), basins.begin() + 3, 1, std::multiplies<int>());

    std::cout << basin_product << std::endl;

    return 0;
}
