from __future__ import annotations
import sys
import re
from dataclasses import dataclass
from functools import *
from itertools import *
from typing import *
from math import *
from enum import Enum

sys.path.append("..")
import util

@total_ordering
class Type(Enum):
    HIGH = 0
    ONE = 1
    TWO = 2
    THREE = 3
    FULL = 4
    FOUR = 5
    FIVE = 6

    def __eq__(self, other: Type) -> bool:
        return self.value == other.value

    def __lt__(self, other: Type) -> bool:
        return self.value < other.value


@total_ordering
@dataclass
class Hand:
    cards: list[int]
    bid: int
    hand_type: int = -1

    def get_type(self) -> Type:
        cards_set = set(self.cards)
        # Jokers
        n_jokers = self.cards.count(1)
        if n_jokers == 5:
            return Type.FIVE

        if 1 in cards_set:
            cards_set.remove(1)

        n_occurrences = list(map(lambda v: (v, self.cards.count(v)), cards_set))
        max_value, max_occ = max(n_occurrences, key=lambda p: p[1])
        n_occurrences.remove((max_value, max_occ))

        if max_occ + n_jokers == 5:
            return Type.FIVE
        elif max_occ + n_jokers == 4:
            return Type.FOUR
        elif max_occ + n_jokers == 3:
            _, max_occ = max(n_occurrences, key=lambda p: p[1])
            if max_occ == 2:
                return Type.FULL
            return Type.THREE
        elif max_occ + n_jokers == 2:
            _, max_occ = max(n_occurrences, key=lambda p: p[1])
            if max_occ == 2:
                return Type.TWO
            return Type.ONE
        elif max_occ + n_jokers == 1:
            return Type.HIGH

    def __post_init__(self) -> None:
        self.hand_type = self.get_type()

    def __eq__(self, other: Hand) -> bool:
        return self.hand_type == other.hand_type and self.cards == other.cards

    def __lt__(self, other: Hand) -> bool:
        if self.hand_type < other.hand_type:
            return True
        if self.hand_type > other.hand_type:
            return False
        return self.cards < other.cards

def parse_line(line: str) -> Hand:
    hand, bid = line.split(" ")
    letter_to_int = {"J": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9, "T": 10, "Q": 12, "K": 13, "A": 14}
    return Hand(list(map(lambda c: letter_to_int[c], hand)), int(bid))

def part1(data: list[Hand]) -> int:
    sorted_game = sorted(data)
    return sum(map(lambda p: (p[0] + 1) * p[1].bid, enumerate(sorted_game)))

def part2(data: list[Hand]) -> int:
    print(data)
    print(sorted(data))
    return 0

util.setup(7)
print("PART 2")
print(util.solve_question(parse_line, part1, 5905))
