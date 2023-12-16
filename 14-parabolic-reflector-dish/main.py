from enum import Enum
from typing import List, Set, Tuple
from dataclasses import dataclass, replace
from itertools import count, repeat
from functools import reduce
from copy import copy
from collections.abc import Callable
import sys


# magic
def tailrec(g: Callable) -> Callable:

    class TailRecurseException(BaseException):
        def __init__(self, args, kwargs):
            self.args = args
            self.kwargs = kwargs

    def func(*args, **kwargs):
        f = sys._getframe()
        if f.f_back and f.f_back.f_back and f.f_back.f_back.f_code == f.f_code:
            raise TailRecurseException(args, kwargs)
        else:
            while True:
                try: return g(*args, **kwargs)
                except TailRecurseException as e:
                    args = e.args
                    kwargs = e.kwargs

    func.__doc__ = g.__doc__
    return func


class Direction(Enum):
    NORTH = 1
    SOUTH = 2
    EAST = 3
    WEST = 4


Coords = Tuple[int, int]


@dataclass
class Dish:
    rounded: Set[Coords]  # 'O'
    cubed: Set[Coords]    # '#'
    height: int
    width: int

    def has_obstacle_at(self, coords: Coords) -> bool:
        return (coords in self.rounded) or (coords in self.cubed)


# 1, 'O.OO#....#' -> O: (1, 0), (1, 2), (1, 3)...
def parse_line(n: int, line: str) -> Tuple[Set[Coords], Set[Coords]]:
    o_indices = [e for e, c in enumerate(line) if c == 'O']
    hash_indices = [e for e, c in enumerate(line) if c == '#']
    o_coords = zip(repeat(n), o_indices)
    hash_coords = zip(repeat(n), hash_indices)
    return (set(o_coords), set(hash_coords))


def parse_dish(txt: str) -> Dish:
    lines = txt.splitlines()
    [head, *_] = lines
    width = len(head)
    height = len(lines)
    pairs = map(parse_line, count(), lines)
    round: Tuple[Set[Coords]]
    cube: Tuple[Set[Coords]]
    round, cube = zip(*pairs)
    empty: Set[Coords] = set()
    round_coords = reduce(lambda acc, x: acc.union(x), round, empty)
    cube_coords = reduce(lambda acc, x: acc.union(x), cube, empty)
    return Dish(width=width, height=height, rounded=round_coords, cubed=cube_coords)


def next_coords(dir: Direction, coords: Coords) -> Coords:
    (row, col) = coords
    match dir:
        case Direction.NORTH: return (row - 1, col)
        case Direction.SOUTH: return (row + 1, col)
        case Direction.EAST: return (row, col + 1)
        case Direction.WEST: return (row, col - 1)


def out_of_bounds(dish: Dish, coords: Coords) -> bool:
    (row, col) = coords
    return row < 0 or col < 0 or row >= dish.height or col >= dish.width


@tailrec
def move(dir: Direction, dish: Dish, coords: Coords) -> Coords:
    new = next_coords(dir, coords)
    if dish.has_obstacle_at(new) or out_of_bounds(dish, new):
        return coords
    else:
        return move(dir, dish, new)


def after_move(dir: Direction, dish: Dish, coords: Coords) -> Dish:
    replica = copy(dish.rounded)
    replica.remove(coords)
    new = move(dir, dish, coords)
    replica.add(new)
    return replace(dish, rounded=replica)


def move_order(dir: Direction, coordss: Set[Coords]) -> List[Coords]:
    l = list(coordss)
    match dir:
        case Direction.NORTH: return sorted(l)
        case Direction.SOUTH: return sorted(l, reverse=True)
        case Direction.WEST: return sorted(l, key=lambda t: t[1])
        case Direction.EAST: return sorted(l, key=lambda t: t[1], reverse=True)


def tilt(dir: Direction, dish: Dish) -> Dish:
    order = move_order(dir, dish.rounded)

    @tailrec
    def aux(dir: Direction, dish: Dish, ord: List[Coords]) -> Dish:
        match ord:
            case [c, *cs]:
                new_dish = after_move(dir, dish, c)
                return aux(dir, new_dish, cs)
            case _:
                return dish

    return aux(dir, dish, order)


def load(dish: Dish) -> int:
    
    def load1(c: Coords) -> int:
        (row, _) = c
        return dish.height - row

    return sum(map(load1, dish.rounded))


def tilt_cycle(dish: Dish) -> Dish:
    return tilt(Direction.EAST, tilt(Direction.SOUTH, tilt(Direction.WEST, tilt(Direction.NORTH, dish))))


def main_():
    handle = open('input.txt', 'r')
    input = handle.read()
    handle.close()
    dish = parse_dish(input)
    tilted = tilt(Direction.NORTH, dish)
    total = load(tilted)
    print(total)


def main():
    handle = open('input.txt', 'r')
    input = handle.read()
    handle.close()
    dish = parse_dish(input)

    out = open('out.txt', 'w')
    out.write('')
    out.close()

    out = open('out.txt', 'a')
    for i in range(1000):
        out.write(str(i) + ' ' + str(load(dish)))
        out.write('\n')
        out.flush()
        dish = tilt_cycle(dish)

    # now find the pattern


if __name__ == '__main__':
    main()
