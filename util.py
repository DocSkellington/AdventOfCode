from typing import Callable, Optional

def load[T](test: bool, parse: Callable[[str], T]) -> list[T]:
    lines = []
    with open("test" if test else "input", "r", encoding="UTF-8") as inF:
        for line in inF.readlines():
            lines.append(parse(line.strip()))
    return lines


def solve_question[I, O](parse_input: Callable[[str], I], question_solver: Callable[[I], O], expected_test: O) -> Optional[O]:
    data = load(True, parse_input)
    output = question_solver(data)
    if output == expected_test:
        print("Example ok")
        data = load(False, parse_input)
        return question_solver(data)
    print(f"Example wrong. Expected {expected_test} but got {output}")
    return None
