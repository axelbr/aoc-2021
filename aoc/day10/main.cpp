#include <iostream>
#include <vector>
#include <fstream>
#include <optional>
#include <stack>
#include <map>
#include <algorithm>
#include <numeric>
#include <sstream>

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

struct SyntaxCheckerState {
    std::string line;
    bool corrupted;
    char last_symbol;
    char expected_symbol;
    size_t position;
    std::stack<char> stack;
};



SyntaxCheckerState run_checker(const std::string& line) {
    std::map<char, char> pairs = {
            {'(', ')'},
            {'{', '}'},
            {'<', '>'},
            {'[', ']'},
    };

    std::stack<char> stack;
    char c;
    for (uint i = 0; i < line.length(); i++) {
        c = line[i];
        if (pairs.contains(c)) {
            stack.push(pairs[c]);
        } else {
            char expected_closing =  stack.top();
            if (c != expected_closing)  {
                return SyntaxCheckerState({line, true, c, expected_closing, i, stack});
            }
            stack.pop();
        }
    }
    return SyntaxCheckerState({line, false, c, c, line.length(), stack});
}

int compute_score(const std::vector<SyntaxCheckerState>& errors) {
    std::map<char, int> error_scores = {
            {')', 3},
            {'}', 1197},
            {'>', 25137},
            {']', 57},
    };
    std::vector<int> scores = {};
    std::transform(errors.begin(), errors.end(), std::back_inserter(scores), [&](const auto& error) {
        return error_scores[error.last_symbol];
    });
    return std::reduce(scores.begin(), scores.end(), 0, [](auto a, auto b) {return a+b;});
}

std::vector<SyntaxCheckerState> check_input(const std::vector<std::string>& input) {
    std::vector<SyntaxCheckerState> states = {};
    std::transform(input.begin(), input.end(), std::back_inserter(states), run_checker);
    return states;
}

std::string to_string(const SyntaxCheckerState& state) {
    std::stringstream sstream;
    if (state.corrupted) {
        sstream << "Syntax error in column " << state.position << ": Expected '"
                  << state.expected_symbol << "', but found '" << state.last_symbol << "'.";
    } else {
        sstream << "Ok.";
    }
    return sstream.str();
}

std::vector<char> to_vector(std::stack<char> stack) {
    std::vector<char> stack_content;
    while (not stack.empty()) {
        stack_content.push_back(stack.top());
        stack.pop();
    }
    return stack_content;
}

void solve_part1(const std::vector<std::string>& input) {
    std::vector<SyntaxCheckerState> states = check_input(input);
    std::vector<SyntaxCheckerState> errors;

    for (int i = 0; i < input.size(); i++) {
        const auto& state = states[i];
        std::cout << "| Line " << i << ": " << to_string(state) << std::endl;
    }

    std::copy_if(states.begin(), states.end(), std::back_inserter(errors), [](const auto& state) {
        return state.corrupted;
    });
    std::cout << "| Part 1 Score: " << compute_score(errors) << std::endl << std::endl;
}

void solve_part2(const std::vector<std::string>& input) {
    std::map<char, int> error_scores = {
            {')', 1},
            {']', 2},
            {'}', 3},
            {'>', 4}
    };
    std::vector<SyntaxCheckerState> incompleted = {};
    std::vector<SyntaxCheckerState> states = check_input(input);
    std::copy_if(states.begin(), states.end(), std::back_inserter(incompleted), [](const auto& state) {
        return not state.corrupted and state.stack.size() > 0;
    });
    for (const auto& state: incompleted) {
        const auto remaining = to_vector(state.stack);
        std::string contents(remaining.begin(), remaining.end());
        std::cout << state.line << " - Complete by adding: " << contents << std::endl;
    }

    std::vector<ulong> scores = {};
    std::transform(incompleted.begin(), incompleted.end(), std::back_inserter(scores), [&error_scores](const auto& state) {
       ulong score = 0;
       for (char c: to_vector(state.stack)) {
           score = score * 5 + error_scores[c];
       }
       return score;
    });
    std::sort(scores.begin(), scores.end());
    std::cout << "Part 2 Score: " << scores[scores.size() / 2];
}

int main() {
    auto input = read_input("./input/large.txt");
    solve_part1(input);
    solve_part2(input);
}