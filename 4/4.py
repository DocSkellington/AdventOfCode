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

@dataclass
class Card:
    id: int
    winning: list[int]
    have: list[int]
    count: int = 1

    def get_number_matching(self) -> int:
        return len(list(filter(
            lambda x: x in self.winning,
            self.have
        )))

    def get_points(self) -> int:
        m = self.get_number_matching()
        if m == 0:
            return 0
        return 2 ** (m - 1)

id_regex = re.compile(r"Card (\d*)")
number_regex = re.compile(r"(\d+)")
def parse_line(line: str) -> str:
    card = Card(re.findall(id_regex, line)[0], [], [])
    winning, have = line.split(":")[1].split("|")
    card.winning = re.findall(number_regex, winning)
    card.have = re.findall(number_regex, have)
    return card

def part1(data: list[Card]) -> int:
    return reduce(lambda acc, card: acc + card.get_points(), data, 0)

def part2(cards: list[Card]) -> int:
    for i in range(len(cards)):
        card = cards[i]
        for shift in range(card.get_number_matching()):
            cards[i + shift + 1].count += card.count

    return reduce(lambda acc, card: acc + card.count, cards, 0)

print("PART 1")
print(util.solve_question(parse_line, part1, 13))
print("PART 2")
print(util.solve_question(parse_line, part2, 30))
