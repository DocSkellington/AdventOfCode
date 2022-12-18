#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <array>

enum OperationType
{
    NOOP = 0,
    ADD
};

struct Operation
{
    const OperationType type;
    const int value;

    Operation() : type(NOOP), value(0)
    {
    }

    Operation(int val) : type(ADD), value(val) {}
};

using operations_storage = std::vector<Operation>;

void fill(std::istream &input, operations_storage &operations)
{
    std::string line;
    while (getline(input, line))
    {
        int space = line.find(' ');
        std::string operation = line.substr(0, space);
        if (operation == "noop")
        {
            operations.emplace_back();
        }
        else
        {
            operations.emplace_back(std::stoi(line.substr(space + 1)));
        }
    }
}

bool updateStrength(unsigned int &strength, const unsigned int &cycleCount, const unsigned int cyclesForStrength[], unsigned int &index, const unsigned int &size, const int &registerX)
{
    if (cycleCount == cyclesForStrength[index])
    {
        strength += cycleCount * registerX;
        index++;
    }

    return index == size;
}

int signalStrength(const operations_storage &operations, std::initializer_list<unsigned int> cyclesForStrengthList)
{
    unsigned int strength = 0;
    unsigned int cycleCount = 1;
    const unsigned int *cyclesForStrength = cyclesForStrengthList.begin();
    unsigned int indexCycles = 0;
    long int registerX = 1;

    for (const Operation &operation : operations)
    {
        switch (operation.type)
        {
        case NOOP:
            cycleCount++;
            if (updateStrength(strength, cycleCount, cyclesForStrength, indexCycles, cyclesForStrengthList.size(), registerX))
            {
                return strength;
            }
            break;
        case ADD:
            cycleCount++;
            if (updateStrength(strength, cycleCount, cyclesForStrength, indexCycles, cyclesForStrengthList.size(), registerX))
            {
                return strength;
            }
            registerX += operation.value;
            cycleCount++;
            if (updateStrength(strength, cycleCount, cyclesForStrength, indexCycles, cyclesForStrengthList.size(), registerX))
            {
                return strength;
            }
            break;
        }
    }

    return strength;
}

void drawPixel(const int &registerX, const int &spriteWidth, int &screenPosition, const int &screenWidth)
{
    bool found = false;
    for (int i = registerX - spriteWidth / 2; i <= registerX + spriteWidth / 2; i++) {
        if (i == screenPosition) {
            std::cout << "#";
            found = true;
            break;
        }
    }
    if (!found) {
        std::cout << ".";
    }

    if (++screenPosition == screenWidth) {
        screenPosition = 0;
        std::cout << "\n";
    }
}

void drawPixelsForOperations(const operations_storage &operations, const int spriteWidth, const int screenWidth, const int screenHeight)
{
    int cycleCount = 1;
    const int maxCycleCount = screenWidth * screenHeight;
    long int registerX = 1;
    int screenPosition = 0;

    drawPixel(registerX, spriteWidth, screenPosition, screenWidth);
    for (const Operation &operation : operations)
    {
        switch (operation.type)
        {
        case NOOP:
            cycleCount++;
            drawPixel(registerX, spriteWidth, screenPosition, screenWidth);
            if (cycleCount == maxCycleCount) {
                return;
            }
            break;
        case ADD:
            cycleCount++;
            drawPixel(registerX, spriteWidth, screenPosition, screenWidth);
            if (cycleCount == maxCycleCount) {
                return;
            }
            registerX += operation.value;
            cycleCount++;
            drawPixel(registerX, spriteWidth, screenPosition, screenWidth);
            if (cycleCount == maxCycleCount) {
                return;
            }
            break;
        }
    }
}

int main()
{
    operations_storage operations;
    std::ifstream input_file;
    input_file.open("input");
    fill(input_file, operations);
    input_file.close();

    std::cout << signalStrength(operations, {20, 60, 100, 140, 180, 220}) << "\n";
    drawPixelsForOperations(operations, 3, 40, 6);
    return 0;
}
