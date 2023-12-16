# pyright: strict

from dataclasses import dataclass
from enum import Enum
from typing import Dict, List, Tuple
from itertools import cycle
from math import lcm


class Direction(Enum):
    LEFT = 1
    RIGHT = 2


# 'L' -> LEFT
# 'R' -> RIGHT
def parse_direction(char: str) -> Direction:
    match char:
        case 'L': return Direction.LEFT
        case _: return Direction.RIGHT


# 'LRLRL' -> [LEFT, RIGHT, LEFT, RIGHT, LEFT]
def parse_directions(line: str) -> List[Direction]:
    return list(map(parse_direction, line))


@dataclass
class Node:
    left: str
    right: str


# 'AAA = (BBB, CCC)' -> ('AAA', Node('BBB', 'CCC'))
def parse_node(line: str) -> Tuple[str, Node]:
    [name, rest] = line.split(' = ')
    [left, right] = rest[1:-1].split(', ')
    return (name, Node(left=left, right=right))


def parse_nodes(paragraph: str) -> Dict[str, Node]:
    lines = paragraph.splitlines()
    nodes = map(parse_node, lines)
    return dict(nodes)


def parse_input(input: str) -> Tuple[List[Direction], Dict[str, Node]]:
    [line, paragraph] = input.split('\n\n')
    return (parse_directions(line), parse_nodes(paragraph))


def steps_to_ZZZ(nodes: Dict[str, Node], curr: str, directions: List[Direction]) -> int:
    instructions = cycle(directions)
    step = 0

    # I HATE THIS
    while curr != 'ZZZ':
        direction = next(instructions)
        node = nodes[curr]
        curr = node.left if direction == Direction.LEFT else node.right
        step += 1

    return step
    

def main_():
    handle = open('input.txt', 'r')
    text = handle.read()
    handle.close()
    (directions, nodes) = parse_input(text)
    steps = steps_to_ZZZ(nodes, 'AAA', directions)
    print(steps)
    

# part 2

def steps_to_z(nodes: Dict[str, Node], curr: str, directions: List[Direction]) -> int:
    instructions = cycle(directions)
    step = 0

    while curr[-1] != 'Z':
        direction = next(instructions)
        node = nodes[curr]
        curr = node.left if direction == Direction.LEFT else node.right
        step += 1

    return step


def main():
    handle = open('input.txt', 'r')
    text = handle.read()
    handle.close()
    (directions, nodes) = parse_input(text)
    names = nodes.keys()
    a_nodes = filter(lambda name: name[-1] == 'A', names)
    stepss = map(lambda curr: steps_to_z(nodes, curr, directions), a_nodes)
    print(lcm(*stepss))



if __name__ == '__main__':
    main()
