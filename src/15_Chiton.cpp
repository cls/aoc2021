#include <iostream>
#include <limits>
#include <queue>
#include <vector>

namespace {

class Cavern
{
private:
    std::vector<int> risks_;
    size_t width_;

public:
    Cavern() : width_(0) {}

    size_t size() const { return risks_.size(); }

    size_t width() const { return width_; }

    size_t height() const { return size() / width(); }

    int risk_level_at(size_t i) const { return risks_[i]; }

    int least_total_risk() const;

    Cavern tile(int tile_width, int tile_height) const;

    template<class Func>
    void for_each_neighbour(size_t i, Func func) const;

    friend std::istream& operator>>(std::istream& in, Cavern& cavern);
};

class GreaterTotalRisk
{
private:
    const std::vector<int>& total_risk_;

public:
    GreaterTotalRisk(const std::vector<int>& total_risk) :
        total_risk_(total_risk)
    {}

    bool operator()(size_t i, size_t j) const
    {
        return total_risk_[i] > total_risk_[j];
    }
};

int Cavern::least_total_risk() const
{
    std::vector<int> total_risk(size(), std::numeric_limits<int>::max());

    std::priority_queue<size_t, std::vector<size_t>, GreaterTotalRisk> queue(total_risk);

    total_risk[0] = 0;
    queue.push(0);

    while (!queue.empty()) {
        size_t i = queue.top();

        if (i + 1 == size()) {
            break;
        }

        queue.pop();

        for_each_neighbour(i, [this, &queue, &total_risk, i](size_t j) {
            int risk = total_risk[i] + risk_level_at(j);

            if (total_risk[j] > risk) {
                total_risk[j] = risk;
                queue.push(j);
            }
        });
    }

    return total_risk.back();
}

Cavern Cavern::tile(int tile_width, int tile_height) const
{
    Cavern tiled_cavern;

    tiled_cavern.width_ = width_ * tile_width;

    for (int tile_y = 0; tile_y < tile_height; ++tile_y) {
        for (size_t y = 0; y < height(); ++y) {
            for (int tile_x = 0; tile_x < tile_width; ++tile_x) {
                for (size_t x = 0; x < width(); ++x) {
                    int risk_level = risk_level_at(x + y*width());

                    tiled_cavern.risks_.push_back((risk_level + tile_x + tile_y - 1) % 9 + 1);
                }
            }
        }
    }

    return tiled_cavern;
}

template<class Func>
void Cavern::for_each_neighbour(size_t i, Func func) const
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

std::istream& operator>>(std::istream& in, Cavern& cavern)
{
    char c;

    while (in.get(c)) {
        if (c >= '0' && c <= '9') {
            cavern.risks_.push_back(c - '0');
        }
        else if (c != '\n') {
            throw std::runtime_error("Unexpected input");
        }
        else if (cavern.width_ == 0) {
            cavern.width_ = cavern.risks_.size();
        }
        else if (cavern.risks_.size() % cavern.width_ != 0) {
            throw std::runtime_error("Ragged input");
        }
    }

    return in;
}

} // namespace

int
main(int argc, char **argv)
{
    Cavern cavern;

    std::cin >> cavern;

    int total_risk = cavern.least_total_risk();

    std::cout << total_risk << std::endl;

    Cavern tiled_cavern(cavern.tile(5, 5));

    int tiled_total_risk = tiled_cavern.least_total_risk();

    std::cout << tiled_total_risk << std::endl;

    return 0;
}
