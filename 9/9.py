from __future__ import annotations
import sys
import re
from dataclasses import dataclass
from functools import *
from itertools import *
from typing import *
from math import *

sys.path.append("..")
import util

number_regex = re.compile(r"(-?\d+)")
def parse_line(line: str) -> list[int]:
    return list(map(lambda v: int(v), re.findall(number_regex, line)))

def generate_next(line: list[int]) -> list[int]:
    if len(line) == 1:
        return [0]
    res = [0] * (len(line) - 1)
    prev = line[0]
    for i in range(1, len(line)):
        res[i-1] = line[i] - prev
        prev = line[i]
    return res

def all_zero(line: list[int]) -> bool:
    return not any(x != 0 for x in line)

def part1(data: list[int]) -> int:
    sum = 0
    for line in data:
        for_line = line[-1]
        while not all_zero(line):
            line = generate_next(line)
            for_line += line[-1]
        print(for_line)
        sum += for_line

    return sum

def part2(data: list[int]) -> int:
    sum = 0
    for line in data:
        first_of_each = [line[0]]
        while not all_zero(line):
            line = generate_next(line)
            first_of_each.append(line[0])

        print(first_of_each)
        head = 0
        for i in range(len(first_of_each) - 2, -1, -1):
            head = first_of_each[i] - head
            print(head)
        print("FOR LINE", head)
        sum += head
    return sum


util.setup(9)
print("PART 1")
print(util.solve_question(parse_line, part1, 114))
print("PART 2")
print(util.solve_question(parse_line, part2, 2))
