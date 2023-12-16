from __future__ import annotations
import sys
import re
from dataclasses import dataclass
from functools import *
from itertools import *
from typing import *
from math import *
import multiprocessing

sys.path.append("..")
import util

@dataclass
class Node:
    label: str
    left: str | Node
    right: str | Node

    def get_next(self, action: str) -> Node:
        if action == "L":
            return self.left
        return self.right

    def __str__(self) -> str:
        return f"{self.label}: ({self.left.label}, {self.right.label})"

    def __repr__(self) -> str:
        return str(self)

def parse_line(line: str) -> str | Node:
    if len(line) == 0:
        return None

    splitted = line.split(" = ")
    if len(splitted) == 1:
        return line
    
    label = splitted[0]
    left, right = re.findall(r"\w+", splitted[1])
    return Node(label, left, right)

def part1(data: list[str | Node]) -> int:
    sequence = data[0]
    label_to_node = {}
    for node in data[1:]:
        label_to_node[node.label] = node
    for node in data[1:]:
        node.left = label_to_node[node.left]
        node.right = label_to_node[node.right]

    current = label_to_node["AAA"]
    in_sequence = 0
    n_steps = 0
    while current != label_to_node["ZZZ"]:
        current = current.get_next(sequence[in_sequence])
        in_sequence = in_sequence + 1 if in_sequence < len(sequence) - 1 else 0
        n_steps += 1
    
    return n_steps

def part2(data: list[str | Node]) -> int:
    def must_continue(nodes: list[str]) -> bool:
        return any(map(lambda node: not node.label.endswith("Z"), nodes))

    assert not must_continue([Node("ABZ", None, None), Node("ZZZ", None, None), Node("ZBZ", None, None)])
    assert must_continue([Node("ABZ", None, None), Node("ZZZ", None, None), Node("ZCV", None, None)])
    sequence = data[0]
    label_to_node = {}
    for node in data[1:]:
        label_to_node[node.label] = node
    for node in data[1:]:
        node.left = label_to_node[node.left]
        node.right = label_to_node[node.right]

    start = list(filter(lambda node: node.label[-1] == "A", data[1:]))
    offsets_cycles = []
    for current in start:
        in_sequence = 0
        n_steps = 0
        offset, cycle = 0, 0
        seen = []
        while True:
            current = current.get_next(sequence[in_sequence])
            if current.label.endswith("Z"):
                already_in = list(filter(lambda t: t[0] == current.label and t[1] == in_sequence, seen))
                if len(already_in) != 0:
                    offset = already_in[0][2]
                    cycle = n_steps - offset
                    break
                seen.append((current.label, in_sequence, n_steps))
            in_sequence = in_sequence + 1 if in_sequence < len(sequence) - 1 else 0
            n_steps += 1
        offsets_cycles.append((offset, cycle))

    # I don't understand why this is enough...
    return lcm(*list(map(lambda p: p[1], offsets_cycles)))

util.setup(8)
print("PART 1")
print(util.solve_question(parse_line, part1, 2))
print("PART 2")
print(util.solve_question(parse_line, part2, 6, "test2"))
