from functools import reduce
import re

lines = []

with open("input", "r", encoding="UTF-8") as inF:
    for line in inF.readlines():
        lines.append(line.strip())

def line_to_number_1(line: str) -> int:
    l = list(filter(lambda c: c in "0123456789", line))
    return int(l[0] + l[-1])


print(reduce(lambda acc, curr: acc + line_to_number_1(curr), lines, 0))

letter_digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
regex = r"(?=(one|two|three|four|five|six|seven|eight|nine|\d))"

def get_digit(value: str) -> str:
    if value.isnumeric():
        return value
    return str(letter_digits.index(value) + 1)

def line_to_number_2(line: str) -> int:
    digits = re.findall(regex, line)
    return int(get_digit(digits[0]) + get_digit(digits[-1]))

print(reduce(lambda acc, curr: acc + line_to_number_2(curr), lines, 0))
