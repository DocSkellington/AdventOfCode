from __future__ import annotations
import sys
import re
from dataclasses import dataclass
from functools import *
from itertools import *
from typing import *
from math import *
import multiprocessing

sys.path.append("..")
import util

class LineParser:
    number_regex = re.compile(r"(\d+)")
    to_regex = re.compile(r"(\w*)-to-(\w*)")
    last = ""

    def parse_line(self, line: str) -> list[int] | list[str]:
        if len(line) ==  0:
            self.last = ""
        elif len(line) != 0:
            if line.startswith("seeds:"):
                return list(map(lambda s: int(s), re.findall(self.number_regex, line)))
            elif len(self.last) == 0:
                self.last = line
                src, dst = re.findall(self.to_regex, line)[0]
                return [src, dst]
            else:
                dst, src, length = re.findall(self.number_regex, line)
                return [int(dst), int(src), int(length)]

@dataclass
class Range:
    dst: int
    src: int
    length: int

    def get_value(self, value: int) -> Optional[int]:
        if self.src <= value and value < self.src + self.length:
            return self.dst + (value - self.src)
        return None

@dataclass
class Map:
    src_str: str
    dst_str: str
    ranges: list[Range]

    def get_value(self, value: int) -> int:
        for r in self.ranges:
            val = r.get_value(value)
            if val is not None:
                return val
        return value

@dataclass
class Problem:
    seeds: list[int]
    maps: list[Map]

    def load(data: list[list[int] | list[str]]) -> Problem:
        problem = Problem(data[0], [])

        current_map = Map(data[1][0], data[1][1], [])
        for current_range in data[2:]:
            if len(current_range) == 2: # Start of a new map
                problem.maps.append(current_map)
                current_map = Map(current_range[0], current_range[1], [])
            else:
                current_map.ranges.append(Range(current_range[0], current_range[1], current_range[2]))
        problem.maps.append(current_map)
        return problem

    def through_all_maps(self, value: int) -> int:
        for m in self.maps:
            value = m.get_value(value)
        return value

def part1(data: list[str]) -> int:
    problem = Problem.load(data)
    return min(map(lambda seed: problem.through_all_maps(seed), problem.seeds))

def for_one_range(problem: Problem, start_length: tuple[int, int]) -> int:
    start, length = start_length
    answer = float("inf")
    for seed in range(start, start + length):
        answer = min(problem.through_all_maps(seed), answer)
    print(start_length, answer)
    return answer

def part2(data: list[str]) -> int:
    problem = Problem.load(data)
    all_seeds = []
    for i in range(0, len(problem.seeds), 2):
        all_seeds.append((problem.seeds[i], problem.seeds[i+1]))

    with multiprocessing.Pool(10) as p:
        return min(p.map(partial(for_one_range, problem), all_seeds))


util.setup(5)
print("PART 1")
print(util.solve_question(LineParser().parse_line, part1, 35))
print("PART 2")
print(util.solve_question(LineParser().parse_line, part2, 46))
