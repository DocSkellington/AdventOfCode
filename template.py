from __future__ import annotations
import sys
import re
from dataclasses import dataclass
from functools import *
from itertools import *
from typing import *

sys.path.append("..")
import util

def parse_line(line: str) -> str:
    return line

def part1(data: list[str]) -> int:
    return 0

def part2(data: list[str]) -> int:
    return 0

print("PART 1")
print(util.solve_question(parse_line, part1, 0))
print("PART 2")
print(util.solve_question(parse_line, part2, 0))
