from __future__ import annotations
import sys

sys.path.append("..")
from util import load

from dataclasses import dataclass
from functools import total_ordering, reduce
import re


@dataclass
@total_ordering
class Round:
    red: int
    green: int
    blue: int

    def __eq__(self, other: Round) -> bool:
        return (
            self.red == other.red
            and self.green == other.green
            and self.blue == other.blue
        )

    def __le__(self, other: Round) -> bool:
        return (
            self.red <= other.red
            and self.green <= other.green
            and self.blue <= other.blue
        )


@dataclass
class Game:
    id: int
    rounds: list[Round]


id_regex = re.compile(r"Game (\d*)")
red_regex = re.compile(r"(\d*) red")
green_regex = re.compile(r"(\d*) green")
blue_regex = re.compile(r"(\d*) blue")


def parse_line(line: str) -> Game:
    game = Game(int(re.findall(id_regex, line)[0]), [])
    for round in line.split(";"):
        red = re.findall(red_regex, round)
        red = int(red[0]) if len(red) != 0 else 0
        green = re.findall(green_regex, round)
        green = int(green[0]) if len(green) != 0 else 0
        blue = re.findall(blue_regex, round)
        blue = int(blue[0]) if len(blue) != 0 else 0
        game.rounds.append(Round(red, green, blue))
    return game


games = load(False, parse_line)

maximum = Round(12, 13, 14)


def is_game_possible(game: Game) -> bool:
    return all(map(lambda round: round <= maximum, game.rounds))


def question_1() -> int:
    def process_game(game: Game) -> int:
        return game.id if is_game_possible(game) else 0

    return reduce(lambda acc, game: acc + process_game(game), games, 0)


print(question_1())


def question_2() -> int:
    def process_game(game: Game) -> int:
        maxi = Round(0, 0, 0)
        for round in game.rounds:
            maxi.red = max(round.red, maxi.red)
            maxi.green = max(round.green, maxi.green)
            maxi.blue = max(round.blue, maxi.blue)
        return maxi.red * maxi.green * maxi.blue

    return reduce(lambda acc, game: acc + process_game(game), games, 0)


print(question_2())
