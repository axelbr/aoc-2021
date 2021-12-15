#include <iostream>
#include <vector>
#include <fstream>
#include <optional>
#include <stack>
#include <map>
#include <algorithm>
#include <numeric>

std::vector<std::string> read_input(const std::string& path) {
    std::vector<std::string> lines;
    std::ifstream file(path);
    std::string line;
    while (std::getline(file, line))
    {
        lines.push_back(line);
    }
    return lines;
}

std::map<char, int> error_scores = {
        {')', 3},
        {'}', 1197},
        {'>', 25137},
        {']', 57},
};

std::map<char, char> pairs = {
        {'(', ')'},
        {'{', '}'},
        {'<', '>'},
        {'[', ']'},
};


struct SyntaxError {
    char actual, expected;
    uint column;
};

std::optional<SyntaxError> check_line(const std::string& line) {
    std::stack<char> stack;
    for (uint i = 0; i < line.length(); i++) {
        char c = line[i];
        if (pairs.contains(c)) {
            stack.push(c);
        } else {
            char opening = stack.top();
            stack.pop();
            char expected_closing = pairs[opening];
            if (c != expected_closing)  {
                return std::make_optional<SyntaxError>({c, pairs[opening], i});
            }
        }
    }
    return std::nullopt;
}

int compute_score(const std::vector<SyntaxError>& errors) {
    std::vector<int> scores = {};
    std::transform(errors.begin(), errors.end(), std::back_inserter(scores), [&](const auto& error) {
        return error_scores[error.actual];
    });
    return std::reduce(scores.begin(), scores.end(), 0, [](auto a, auto b) {return a+b;});
}

void solve_part1(const std::vector<std::string>& input) {
    std::vector<SyntaxError> errors = {};
    for (int i = 0; i < input.size(); i++) {
        const auto &line = input[i];
        auto error = check_line(line);
        if (error.has_value()) {
            std::cout << "Line " << i << ", Syntax error in column " << error->column << ": Expected '"
                      << error->expected << "', but found '" << error->actual << "'." << std::endl;
            errors.push_back(error.value());
        } else {
            std::cout << "Line " << i << ": ok" << std::endl;
        }
    }
    std::cout << "##### Score: " << compute_score(errors) << std::endl;
}

int main() {
    auto input = read_input("./input/large.txt");
    solve_part1(input);
}