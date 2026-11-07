#include <algorithm>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using Cost = std::uint64_t;

struct Input {
    std::size_t width = 0;
    std::vector<std::string> minterms;
    std::vector<std::string> terms;
};

// 빈 줄과 설명용 주석을 허용하되, 실제 데이터의 형식은 엄격히 검사한다.
Input readInput(const std::string& path) {
    std::ifstream file(path);
    if (!file) throw std::runtime_error("Cannot open input: " + path);
    Input input;
    std::map<std::string, char> records;
    std::string line;
    std::size_t lineNumber = 0;
    while (std::getline(file, line)) {
        ++lineNumber;
        if (lineNumber == 1 && line.compare(0, 3, "\xEF\xBB\xBF") == 0)
            line.erase(0, 3);
        const auto comment = line.find("//");
        if (comment != std::string::npos) line.erase(comment);
        std::istringstream row(line);
        std::string first, bits, extra;
        if (!(row >> first)) continue;
        const std::string where = " at line " + std::to_string(lineNumber);
        if (input.width == 0) {
            if (first.find_first_not_of("0123456789") != std::string::npos ||
                (row >> extra))
                throw std::runtime_error("Invalid bit length" + where);
            const auto width = std::stoull(first);
            if (width == 0 || width > std::numeric_limits<std::size_t>::max())
                throw std::runtime_error("Bit length must be positive" + where);
            input.width = static_cast<std::size_t>(width);
            continue;
        }
        if ((first != "m" && first != "d") || !(row >> bits) ||
            (row >> extra) || bits.size() != input.width ||
            bits.find_first_not_of("01") != std::string::npos)
            throw std::runtime_error("Invalid minterm record" + where);
        const auto old = records.find(bits);
        if (old != records.end() && old->second != first[0])
            throw std::runtime_error("Conflicting m/d record" + where);
        records[bits] = first[0];
    }
    if (file.bad()) throw std::runtime_error("Cannot read input: " + path);
    if (input.width == 0) throw std::runtime_error("Missing bit length");
    for (const auto& record : records) {
        input.terms.push_back(record.first);
        if (record.second == 'm') input.minterms.push_back(record.first);
    }
    return input;
}

bool covers(const std::string& pattern, const std::string& minterm) {
    for (std::size_t i = 0; i < pattern.size(); ++i)
        if (pattern[i] != '-' && pattern[i] != minterm[i]) return false;
    return true;
}

// 같은 위치에 '-'가 있고, 나머지 한 비트만 0/1인 두 항을 결합한다.
// 문자열로 비트를 표현하므로 정수형의 32/64비트 제한을 두지 않는다.
std::set<std::string> generatePrimes(const Input& input) {
    std::set<std::string> current(input.terms.begin(), input.terms.end());
    std::set<std::string> primes;
    while (!current.empty()) {
        std::set<std::string> next, used;
        for (const auto& term : current) {
            for (std::size_t bit = 0; bit < input.width; ++bit) {
                if (term[bit] != '0') continue;
                std::string partner = term;
                partner[bit] = '1';
                if (current.count(partner) == 0) continue;
                used.insert(term);
                used.insert(partner);
                partner[bit] = '-';
                next.insert(std::move(partner));
            }
        }
        for (const auto& term : current)
            if (used.count(term) == 0) primes.insert(term);
        current.swap(next);
    }
    return primes;
}

struct Prime {
    std::string pattern;
    std::vector<std::size_t> covered;
    std::vector<std::size_t> inverted;
    Cost andCost = 0;
};

struct Selection {
    std::vector<bool> covered, inverted;
    std::vector<std::size_t> terms;
    std::size_t remaining = 0;
    Cost cost = 0;
};

class ExactCover {
    std::vector<Prime> primes;
    std::vector<std::vector<std::size_t>> chart;
    Selection initial, best;

    // 첫 항에는 OR가 없고, 둘째 항은 2입력 OR, 이후에는 입력당 2개가 추가된다.
    Cost extraCost(const Selection& state, std::size_t index) const {
        Cost cost = primes[index].andCost;
        if (state.terms.size() == 1) cost += 6;
        else if (state.terms.size() >= 2) cost += 2;
        for (const auto bit : primes[index].inverted)
            if (!state.inverted[bit]) cost += 2;
        return cost;
    }

    void add(Selection& state, std::size_t index) const {
        state.cost += extraCost(state, index);
        state.terms.push_back(index);
        for (const auto bit : primes[index].inverted) state.inverted[bit] = true;
        for (const auto m : primes[index].covered) {
            if (!state.covered[m]) {
                state.covered[m] = true;
                --state.remaining;
            }
        }
    }

    std::size_t gain(const Selection& state, std::size_t index) const {
        std::size_t count = 0;
        for (const auto m : primes[index].covered)
            if (!state.covered[m]) ++count;
        return count;
    }

    // 탐욕 해는 탐색의 상한으로만 사용하고, 최종 답은 정확 탐색으로 결정한다.
    Selection greedy(Selection state) const {
        while (state.remaining != 0) {
            std::size_t choice = primes.size(), largest = 0;
            Cost cheapest = std::numeric_limits<Cost>::max();
            for (std::size_t p = 0; p < primes.size(); ++p) {
                const auto count = gain(state, p);
                const auto cost = extraCost(state, p);
                if (count > largest || (count != 0 && count == largest && cost < cheapest)) {
                    choice = p;
                    largest = count;
                    cheapest = cost;
                }
            }
            if (choice == primes.size()) throw std::logic_error("Uncoverable minterm");
            add(state, choice);
        }
        return state;
    }

    // 각 분기에서 앞서 시도한 후보를 금지하여 같은 항 집합의 순열을 제거한다.
    void search(const Selection& state, std::vector<bool> forbidden) {
        if (state.remaining == 0) {
            if (state.cost < best.cost) best = state;
            return;
        }
        if (state.cost >= best.cost) return;
        std::vector<std::size_t> choices;
        std::size_t fewest = std::numeric_limits<std::size_t>::max();
        Cost lowerBound = 0;
        for (std::size_t m = 0; m < chart.size(); ++m) {
            if (state.covered[m]) continue;
            std::vector<std::size_t> candidates;
            Cost cheapest = std::numeric_limits<Cost>::max();
            for (const auto p : chart[m]) {
                if (forbidden[p]) continue;
                candidates.push_back(p);
                cheapest = std::min(cheapest, extraCost(state, p));
            }
            if (candidates.empty()) return;
            if (candidates.size() < fewest) {
                fewest = candidates.size();
                choices = std::move(candidates);
            }
            // 공유 가능한 게이트를 중복 합산하지 않도록 최댓값만 하한으로 쓴다.
            lowerBound = std::max(lowerBound, cheapest);
        }
        if (lowerBound >= best.cost - state.cost) return;
        std::sort(choices.begin(), choices.end(), [&](std::size_t a, std::size_t b) {
            const auto ca = extraCost(state, a), cb = extraCost(state, b);
            if (ca != cb) return ca < cb;
            const auto ga = gain(state, a), gb = gain(state, b);
            if (ga != gb) return ga > gb;
            return a < b;
        });
        for (const auto p : choices) {
            if (extraCost(state, p) < best.cost - state.cost) {
                Selection next = state;
                add(next, p);
                search(next, forbidden);
            }
            forbidden[p] = true;
        }
    }

public:
    explicit ExactCover(const Input& input) : chart(input.minterms.size()) {
        initial.covered.assign(input.minterms.size(), false);
        initial.inverted.assign(input.width, false);
        initial.remaining = input.minterms.size();
        if (input.minterms.empty()) return;
        for (const auto& pattern : generatePrimes(input)) {
            Prime prime;
            prime.pattern = pattern;
            for (std::size_t m = 0; m < input.minterms.size(); ++m)
                if (covers(pattern, input.minterms[m])) prime.covered.push_back(m);
            // don't-care만 덮는 PI는 선택 대상에서 제외한다.
            if (prime.covered.empty()) continue;
            std::size_t literals = 0;
            for (std::size_t bit = 0; bit < input.width; ++bit) {
                if (pattern[bit] != '-') ++literals;
                if (pattern[bit] == '0') prime.inverted.push_back(bit);
            }
            // 0/1리터럴 항은 상수/배선이며 AND 게이트가 필요 없다.
            if (literals >= 2) prime.andCost = 2 * static_cast<Cost>(literals) + 2;
            for (const auto m : prime.covered) chart[m].push_back(primes.size());
            primes.push_back(std::move(prime));
        }
    }

    std::pair<std::vector<std::string>, Cost> solve() {
        for (const auto& column : chart) {
            if (column.empty()) throw std::logic_error("Uncoverable minterm");
            if (column.size() == 1) {
                const auto p = column.front();
                if (gain(initial, p) != 0) add(initial, p);
            }
        }
        best = greedy(initial);
        search(initial, std::vector<bool>(primes.size(), false));
        std::vector<std::string> answer;
        for (const auto p : best.terms) answer.push_back(primes[p].pattern);
        std::sort(answer.begin(), answer.end());
        return {answer, best.cost};
    }
};

int main(int argc, char* argv[]) {
    try {
        if (argc > 3) throw std::runtime_error("Usage: QMalgorithm2 [input_file [output_file]]");
        const std::string inputPath = argc >= 2 ? argv[1] : "input_minterm.txt";
        const std::string outputPath = argc >= 3 ? argv[2] : "result.txt";
        const Input input = readInput(inputPath);
        ExactCover solver(input);
        const auto answer = solver.solve();
        // 읽기 및 계산이 성공한 뒤에만 출력 파일을 연다.
        std::ofstream output(outputPath);
        if (!output) throw std::runtime_error("Cannot open output: " + outputPath);
        for (const auto& term : answer.first) output << term << '\n';
        output << "Cost (# of transistors): " << answer.second << '\n';
        output.close();
        if (!output) throw std::runtime_error("Cannot write output: " + outputPath);
        return 0;
    } catch (const std::exception& error) {
        std::cerr << "Error: " << error.what() << '\n';
        return 1;
    }
}
