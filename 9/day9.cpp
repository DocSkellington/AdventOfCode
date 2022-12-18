#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <array>

using number_times_visited_storage = std::map<int, std::map<int, int>>;
using moves_storage = std::vector<std::pair<char, int>>;

template <typename T>
int sign(T value)
{
    return (T(0) < value) - (value < T(0));
}

void fill(std::istream &input, moves_storage &moves)
{
    std::string line;
    while (getline(input, line))
    {
        const char direction = line[0];
        const int quantity = std::stoi(line.substr(2));
        moves.emplace_back(direction, quantity);
    }
}

void move_head(int &headX, int &headY, const char &direction)
{
    if (direction == 'U')
    {
        headY++;
    }
    else if (direction == 'D')
    {
        headY--;
    }
    else if (direction == 'R')
    {
        headX++;
    }
    else if (direction == 'L')
    {
        headX--;
    }
}

bool move_tail(const int &headX, const int &headY, int &tailX, int &tailY)
{
    const int diffX = headX - tailX;
    const int diffY = headY - tailY;
    if (std::abs(diffX) == 2 || std::abs(diffY) == 2)
    {
        tailX += sign(diffX);
        tailY += sign(diffY);
        return true;
    }
    return false;
}

void applyMoves(const moves_storage &moves, number_times_visited_storage &numberTimesVisited, const unsigned int numberKnots)
{
    std::vector<std::pair<int, int>> knots(numberKnots, std::make_pair<>(0, 0));
    int &headX = knots[0].first;
    int &headY = knots[0].second;

    numberTimesVisited[0][0] = 1;

    for (const auto &move : moves)
    {
        for (int i = 0; i < move.second; i++)
        {
            move_head(headX, headY, move.first);
            for (std::size_t knot = 1; knot < numberKnots; knot++)
            {
                const std::pair<int, int> &previous = knots[knot - 1];
                std::pair<int, int> &current = knots[knot];
                if (move_tail(previous.first, previous.second, current.first, current.second) && knot == numberKnots - 1)
                {
                    numberTimesVisited[current.first][current.second]++;
                }
            }
        }
    }
}

int compute(const moves_storage &moves, int numberKnots)
{
    number_times_visited_storage numberTimesVisited;
    applyMoves(moves, numberTimesVisited, numberKnots);

    int nCells = 0;
    for (const auto &first : numberTimesVisited)
    {
        nCells += first.second.size();
    }
    return nCells;
}

int main()
{
    moves_storage moves;
    std::ifstream input_file;
    input_file.open("input");
    fill(input_file, moves);
    input_file.close();

    std::cout << compute(moves, 2) << "\n";
    std::cout << compute(moves, 10) << "\n";
    return 0;
}
