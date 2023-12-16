from __future__ import annotations
import sys
from dataclasses import dataclass
from functools import total_ordering, reduce
import itertools
import re
from typing import Optional, Iterable

sys.path.append("..")
import util


@dataclass
class Cell:
    data: str

    def is_number(self) -> bool:
        return self.data.isnumeric()

    def is_empty(self) -> bool:
        return self.data == "."

    def is_symbol(self) -> bool:
        return not self.is_number() and not self.is_empty()


@dataclass
class Grid:
    lines: list[list[Cell]]

    def __post_init__(self):
        self.n_lines = len(self.lines)
        self.n_columns = len(self.lines[0])

    def is_adjacent_to_symbol(self, i: int, j: int) -> bool:
        for adjacent_i in range(i-1, i+2):
            if adjacent_i < 0 or adjacent_i >= self.n_lines:
                continue
            for adjacent_j in range(j-1, j+2):
                if adjacent_j < 0 or adjacent_j >= self.n_columns:
                    continue
                if self.get(adjacent_i, adjacent_j).is_symbol():
                    return True
        return False

    def get(self, i: int, j: int) -> Cell:
        return self.lines[i][j]

    def get_all_part_numbers(self) -> Iterable[int]:
        # This solution assumes that each part number is adjacent to a unique symbol
        # And there are no duplicate part numbers around a symbol
        return reduce(
            lambda acc, l: acc + l,
            map(
                lambda p: self.get_part_numbers_adjacent_to(p[0], p[1]),
                filter(
                    lambda p: self.get(p[0], p[1]).is_symbol(),
                    itertools.product(range(self.n_lines), range(self.n_columns))
                )
            )
        )

    def get_part_number(self, i: int, j: int) -> Optional[int]:
        if not self.get(i, j).is_number():
            return None

        start = j
        while start >= 0 and self.get(i, start).is_number():
            start -= 1
        start += 1

        part = ""
        for position in range(start, self.n_columns):
            cell = self.get(i, position)
            if cell.is_number():
                part += cell.data
            else:
                return int(part)
        
        return int(part)

    def get_part_numbers_adjacent_to(self, i: int, j: int) -> List[int]:
        parts = []
        for adjacent_i in range(i-1, i+2):
            if adjacent_i < 0 or adjacent_i >= self.n_lines:
                continue
            for adjacent_j in range(j-1, j+2):
                if adjacent_j < 0 or adjacent_j >= self.n_columns:
                    continue

                part = self.get_part_number(adjacent_i, adjacent_j)
                if part is not None and part not in parts:
                    parts.append(part)
        return parts

    def get_all_gear_ratios(self) -> list[int]:
        return list(
            map(
                lambda parts: parts[0] * parts[1],
                filter(
                    lambda parts: len(parts) == 2,
                    map(
                        lambda p: self.get_part_numbers_adjacent_to(p[0], p[1]),
                        filter(
                            lambda p: self.get(p[0], p[1]).data == "*",
                            itertools.product(range(self.n_lines), range(self.n_columns))
                        )
                    )
                )
            )
        )



def parse_line(line: str) -> list[Cell]:
    res = []
    for cell in line:
        res.append(Cell(cell))
    return res

print(util.solve_question(parse_line, lambda grid: sum(Grid(grid).get_all_part_numbers()), 4361))
print(util.solve_question(parse_line, lambda grid: sum(Grid(grid).get_all_gear_ratios()), 467835))
