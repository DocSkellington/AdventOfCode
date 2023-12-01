from functools import reduce
import re

lines = []

with open("input", "r", encoding="UTF-8") as inF:
    for line in inF.readlines():
        lines.append(line.strip())

def line_to_number_1(line: str) -> int:
    l = list(filter(lambda c: c in "0123456789", line))
    return int(l[0] + l[-1])


# print(reduce(lambda acc, curr: acc + line_to_number_1(curr), lines, 0))

str_to_number = {
    "zero": 0,
    "one": 1,
    "two": 2,
    "three": 3,
    "four": 4,
    "five": 5,
    "six": 6,
    "seven": 7,
    "eight": 8,
    "nine": 9,
}
number_to_str = {
    0: "zero",
    1: "one",
    2: "two",
    3: "three",
    4: "four",
    5: "five",
    6: "six",
    7: "seven",
    8: "eight",
    9: "nine",
}

def argmin(lst: list[int]) -> int:
    index = -1
    mini = float("inf")
    for i in range(1, len(lst)):
        if lst[i] != -1 and lst[i] < mini:
            mini = lst[i]
            index = i
    return index
def argmax(lst: list[int]) -> int:
    index = -1
    maxi = float("-inf")
    for i in range(1, len(lst)):
        if lst[i] != -1 and lst[i] > maxi:
            maxi = lst[i]
            index = i
    return index


def line_to_number_2(line: str) -> int:
    to_replace = argmin([line.find(key) for key in str_to_number.keys()])
    if to_replace != -1:
        line = line.replace(number_to_str[to_replace], str(to_replace))
        to_replace = argmax([line.rfind(key) for key in str_to_number.keys()])
        if to_replace != -1:
            line = line.replace(number_to_str[to_replace], str(to_replace))
    return line_to_number_1(line)


print(reduce(lambda acc, curr: acc + line_to_number_2(curr), lines, 0))
