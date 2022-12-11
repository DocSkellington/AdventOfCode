#include <iostream>
#include <unordered_map>
#include <memory>
#include <string>
#include <fstream>
#include <limits>

#define TASK1_LIMIT 100000
#define TASK2_REQUIRED 30000000
#define TASK2_TOTAL 70000000

class Node
{
public:
    using children_storage = std::unordered_map<std::string, std::unique_ptr<Node>>;

public:
    void setSize(int size)
    {
        m_size = size;
    }

    void setParent(Node *parent)
    {
        m_parent = parent;
    }

    Node *getOrCreateChild(std::string name)
    {
        if (auto itr = m_children.find(name); itr != m_children.end())
        {
            return itr->second.get();
        }

        m_children[name] = std::make_unique<Node>();
        return m_children[name].get();
    }

    int getSize() const
    {
        if (m_children.empty())
        {
            return m_size;
        }
        else
        {
            int size = 0;
            for (auto itr = m_children.begin(); itr != m_children.end(); itr++)
            {
                size += itr->second->getSize();
            }
            return size;
        }
    }

    Node *getParent() const
    {
        return m_parent;
    }

    bool isFile() const
    {
        return m_children.empty();
    }

    children_storage::const_iterator cbegin()
    {
        return m_children.cbegin();
    }

    children_storage::const_iterator cend()
    {
        return m_children.cend();
    }

private:
    children_storage m_children;
    Node *m_parent;
    int m_size;
};

std::unique_ptr<Node> createTree(std::istream &input)
{
    std::unique_ptr<Node> root = std::make_unique<Node>();
    Node *current = root.get();

    std::string line;
    while (getline(input, line))
    {
        if (line[0] == '$')
        {
            if (line[2] == 'c')
            {
                std::string path = line.substr(5);
                if (path == "/")
                {
                    std::cout << "Back to root\n";
                    current = root.get();
                }
                else if (path == "..")
                {
                    std::cout << "Back to parent\n";
                    current = current->getParent();
                }
                else
                {
                    std::cout << "Child " << path << "\n";
                    Node *child = current->getOrCreateChild(path);
                    child->setParent(current);
                    current = child;
                }
            }
        }
        else
        {
            if (auto itr = line.find(" "); itr != std::string::npos)
            {
                std::string filename = line.substr(itr + 1);
                Node *child = current->getOrCreateChild(filename);
                if (line.substr(0, itr) == "dir")
                {
                    child->setSize(0);
                }
                else
                {
                    int size = std::stoi(line.substr(0, itr));
                    std::cout << filename << " " << size << "\n";
                    child->setSize(size);
                }
            }
        }
    }
    return root;
}

void task1(Node *current, long long int &sumSizes)
{
    int size = current->getSize();
    if (!current->isFile() && size <= TASK1_LIMIT)
    {
        std::cout << size << "\n";
        sumSizes += size;
    }
    for (auto itr = current->cbegin(); itr != current->cend(); itr++)
    {
        task1(itr->second.get(), sumSizes);
    }
}

int task2(Node *current, int minimalSize)
{
    if (current->isFile() || current->getSize() < minimalSize) {
        return std::numeric_limits<int>::max();
    }

    int bestFound = current->getSize();
    for (auto itr = current->cbegin() ; itr != current->cend(); itr++) {
        int sizeChild = task2(itr->second.get(), minimalSize);
        bestFound = std::min(sizeChild, bestFound);
    }
    return bestFound;
}

int main()
{
    std::ifstream inputFile;
    inputFile.open("input");
    auto tree = createTree(inputFile);
    inputFile.close();

    long long int sumSizes = 0;
    task1(tree.get(), sumSizes);
    std::cout << sumSizes << "\n";

    int freeSpace = TASK2_TOTAL - tree->getSize();
    int sizeToClear = TASK2_REQUIRED - freeSpace;
    std::cout << task2(tree.get(), sizeToClear) << "\n";

    return 0;
}
