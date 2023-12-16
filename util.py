from typing import Callable

def load[T](test: bool, parse: Callable[[str], T]) -> list[T]:
    lines = []
    with open("test" if test else "input", "r", encoding="UTF-8") as inF:
        for line in inF.readlines():
            lines.append(parse(line.strip()))
    return lines