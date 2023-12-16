from typing import Callable, Optional
import os

# From https://gist.github.com/MathisHammel/43aa722469a626504de40744dfe0a3da
import requests

AOC_COOKIE = "53616c7465645f5f5ee8bf3b11f520e8fce5766cc52d79f084f9d2755f407ba8e8d14e72c6803397db21536b2a72dafd2442550ced85292cbc3f2301af3da6e8"
YEAR = 2023

def get_input(day):
    req = requests.get(f'https://adventofcode.com/{YEAR}/day/{day}/input', 
                       headers={'cookie':'session='+AOC_COOKIE})
    return req.text

def get_example(day,offset=0):
    req = requests.get(f'https://adventofcode.com/{YEAR}/day/{day}',
                       headers={'cookie':'session='+AOC_COOKIE})
    return req.text.split('<pre><code>')[offset+1].split('</code></pre>')[0]

# Own functions

def setup(day: int) -> None:
    if os.path.exists("input"):
        return
    with open("test", "w") as f:
        f.write(get_example(day))
    with open("input", "w") as f:
        f.write(get_input(day))

def load[T](file_name: str, parse: Callable[[str], T]) -> list[T]:
    lines = []
    with open(file_name, "r", encoding="UTF-8") as inF:
        for line in inF.readlines():
            p = parse(line.strip())
            if p is not None:
                lines.append(p)
    return lines


def solve_question[I, O](parse_input: Callable[[str], I], question_solver: Callable[[I], O], expected_test: O, test_name: str = "test") -> Optional[O]:
    data = load(test_name, parse_input)
    output = question_solver(data)
    if output == expected_test:
        print("Example ok")
        data = load("input", parse_input)
        return question_solver(data)
    print(f"Example wrong. Expected {expected_test} but got {output}")
    return None
