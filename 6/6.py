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

number_regex = re.compile(r"(\d+)")
def parse_line(line: str) -> list[str]:
    return list(map(lambda v: v, re.findall(number_regex, line)))

@dataclass
class Race:
    time: int
    threshold: int

    def get_distance(self, wait: int) -> int:
        return wait * (self.time - wait)

    def above_threshold(self, wait: int) -> bool:
        return self.get_distance(wait) > self.threshold

    def binary_search(self, low: int, high: int) -> int:
        if low == high:
            return low
        if high - low == 1:
            if self.above_threshold(low):
                return low
            return high
            
        mid = low + (high - low) // 2

        if self.above_threshold(mid):
            return self.binary_search(low, mid)
        return self.binary_search(mid, high)

    def smallest_index_above_threshold(self) -> int:
        low = 0
        high = floor(self.time / 2.)
        if self.above_threshold(low):
            return 0
        return self.binary_search(low, high)

    def number_of_beating_ways(self) -> int:
        smallest = self.smallest_index_above_threshold()
        highest = self.time - smallest
        return highest - smallest + 1


@dataclass
class Races:
    races: list[Race]

    def number_of_beating_ways(self) -> list[int]:
        return list(map(lambda race: race.number_of_beating_ways(), self.races))

def part1(data: list[str]) -> int:
    races = Races([])
    for i in range(len(data[0])):
        race = Race(int(data[0][i]), int(data[1][i]))
        races.races.append(race)
    return reduce(lambda acc, cur: acc * cur, races.number_of_beating_ways(), 1)

def part2(data: list[str]) -> int:
    races = Races([Race(int("".join(data[0])), int("".join(data[1])))])
    return races.number_of_beating_ways()[0]

print(util.setup(6))
print("PART 1")
print(util.solve_question(parse_line, part1, 288))
print("PART 2")
print(util.solve_question(parse_line, part2, 71503))
