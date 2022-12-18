#include <iostream>
#include <fstream>
#include <vector>

using storage = std::vector<std::vector<int>>;

void fill(std::istream &input, storage &forest)
{
    std::string line;
    while (getline(input, line)) {
        std::vector<int> row;
        for (char tree_char : line) {
            int tree = tree_char - '0';
            row.push_back(tree);
        }
        forest.push_back(row);
    }
}

bool visible_up(storage &forest, int i, int j) {
    for (int otherI = i - 1 ; otherI >= 0 ; otherI--) {
        if (forest[i][j] <= forest[otherI][j]) {
            return false;
        }
    }
    return true;
}

bool visible_down(storage &forest, int i, int j) {
    for (int otherI = i + 1 ; otherI < forest.size() ; otherI++) {
        if (forest[i][j] <= forest[otherI][j]) {
            return false;
        }
    }
    return true;
}

bool visible_left(storage &forest, int i, int j) {
    for (int otherJ = j - 1 ; otherJ >= 0 ; otherJ--) {
        if (forest[i][j] <= forest[i][otherJ]) {
            return false;
        }
    }
    return true;
}

bool visible_right(storage &forest, int i, int j) {
    for (int otherJ = j + 1 ; otherJ < forest.size() ; otherJ++) {
        if (forest[i][j] <= forest[i][otherJ]) {
            return false;
        }
    }
    return true;
}

bool visible(storage &forest, int i, int j) {
    if (i == 0 || i == forest.size() - 1 || j == 0 || j == forest.size() - 1) {
        return true;
    }

    return visible_up(forest, i, j) || visible_down(forest, i, j) || visible_left(forest, i, j) || visible_right(forest, i, j);
}

int task1(storage &forest)
{
    int number_visible = 0;
    for (int i = 0 ; i < forest.size(); i++) {
        for (int j = 0 ; j < forest.size() ; j++) {
            if (visible(forest, i, j)) {
                number_visible++;
            }
        }
    }
    return number_visible;
}

int number_up(storage &forest, int i, int j) {
    int number = 0;
    for (int otherI = i - 1 ; otherI >= 0 ; otherI--) {
        if (forest[i][j] <= forest[otherI][j]) {
            return ++number;
        }
        else {
            number++;
        }
    }
    return number;
}

int number_down(storage &forest, int i, int j) {
    int number = 0;
    for (int otherI = i + 1 ; otherI < forest.size() ; otherI++) {
        if (forest[i][j] <= forest[otherI][j]) {
            return ++number;
        }
        else {
            number++;
        }
    }
    return number;
}

int number_left(storage &forest, int i, int j) {
    int number = 0;
    for (int otherJ = j - 1 ; otherJ >= 0 ; otherJ--) {
        if (forest[i][j] <= forest[i][otherJ]) {
            return ++number;
        }
        else {
            number++;
        }
    }
    return number;
}

int number_right(storage &forest, int i, int j) {
    int number = 0;
    for (int otherJ = j + 1 ; otherJ < forest.size() ; otherJ++) {
        if (forest[i][j] <= forest[i][otherJ]) {
            return ++number;
        }
        else {
            number++;
        }
    }
    return number;
}

int tree_score(storage &forest, int i, int j) {
    if (i == 0 || i == forest.size() - 1 || j == 0 || j == forest.size() - 1) {
        return 0;
    }

    return number_up(forest, i, j) * number_down(forest, i, j) * number_left(forest, i, j) * number_right(forest, i, j);
}

int task2(storage &forest) {
    int max_score = 0;
    for (int i = 0 ; i < forest.size() ; i++) {
        for (int j = 0; j < forest.size(); j++) {
            max_score = std::max(max_score, tree_score(forest, i, j));
        }
    }
    return max_score;
}

int main()
{
    storage forest;
    std::ifstream input_file;
    input_file.open("input");
    fill(input_file, forest);
    input_file.close();

    std::cout << task1(forest) << "\n";
    std::cout << task2(forest) << "\n";
    return 0;
}
