from typing import *

def parse_line_stack(line: str) -> List[str]:
    current_level = []
    for i in range(0, len(line) - 1, 4):
        if line[i+1].isalpha():
            current_level.append(line[i+1])
        else:
            current_level.append(None)
    return current_level


def parse_line_move(line: str) -> Tuple[int, int, int]:
    splitted = line.strip().split(" ")
    return int(splitted[1]), int(splitted[3]) - 1, int(splitted[5]) - 1

task1 = False

with open("input", "r") as input_file:
    lines = input_file.readlines()
    parsing_stack = True
    stacks = []
    for line_id in range(len(lines)):
        print(stacks)
        line = lines[line_id]

        if line == "\n":
            parsing_stack = False
        elif parsing_stack:
            parsed = parse_line_stack(line)
            if len(stacks) == 0:
                for stack in parsed:
                    if stack is None:
                        stacks.append([])
                    else:
                        stacks.append([stack])
            else:
                assert len(stacks) == len(parsed), "{} {}".format(len(stacks), len(parsed))
                for i in range(len(stacks)):
                    if parsed[i] != None:
                        stacks[i].insert(0, parsed[i])
        else:
            quantity, from_stack, to_stack = parse_line_move(line)
            if task1:
                for i in range(quantity):
                    stacks[to_stack].append(stacks[from_stack].pop())
            else:
                temp = []
                for i in range(quantity):
                    temp.append(stacks[from_stack].pop())
                for i in range(quantity):
                    stacks[to_stack].append(temp.pop())
                    

print("".join([stack.pop() for stack in stacks]))