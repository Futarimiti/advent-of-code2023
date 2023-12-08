# part 1
# pyright: strict

from collections.abc import Iterable
from typing import Dict, List, Tuple, TypeVar
from itertools import dropwhile, takewhile


def is_symbol(c: str) -> bool:
    return not c.isnumeric() and c != '.'


def symbols_pos(line: str) -> Iterable[int]:
    enumerated = enumerate(line)
    symbols = filter(lambda x: is_symbol(x[1]), enumerated)
    locations = map(lambda x: x[0], symbols)
    return locations


A = TypeVar('A')
def neighbours(lines: Iterable[A]) -> Iterable[Iterable[A]]:

    def every3(lines: Iterable[A]) -> Iterable[Iterable[A]]:
        match lines:
            case [a, b, c, *xs]:
                return [[a, b, c], *every3([b, c, *xs])]
            case [a, b]:
                return [[a, b]]
            case _:
                return []

    match lines:
        case [a, b, *xs]:
            return [[a, b], *every3([b, *xs])]
        case _:
            return []


# inclusive rang
def loc_to_range(loc: int) -> Tuple[int, int]:
    match loc:
        case 0:
            return (0, 1)
        case 139:  # endl
            return (138, 139)
        case _:
            return (loc - 1, loc + 1)


def overlapping(range1: Tuple[int, int], range2: Tuple[int, int]) -> bool:
    match range1, range2:
        case (a, b), (c, d):
            return a <= d and c <= b


def numbers_in_es(e: Iterable[Tuple[int, str]],
                  ranges: Iterable[Tuple[int, int]]) -> Iterable[int]:
    enum = dropwhile(lambda e_: not e_[1].isnumeric(), e)
    match list(enum):
        case []: return []
        case es:
            num_e = list(takewhile(lambda e: e[1].isnumeric(), es))
            start_idx = num_e[0][0]
            end_idx = num_e[-1][0]
            num_range = (start_idx, end_idx)
            next_es = dropwhile(lambda e: e[1].isnumeric(), es)
            if any(map(lambda r: overlapping(num_range, r), ranges)):
                num = int(''.join(map(lambda e: e[1], num_e)))
                return [num, *numbers_in_es(next_es, ranges)]
            else:
                return numbers_in_es(next_es, ranges)


# inclusive range
# find the number(s) in which part of it is the given ranges
def numbers_in_ranges(line: str,
                      ranges: Iterable[Tuple[int, int]]) -> Iterable[int]:
    enumerated = enumerate(line)
    return numbers_in_es(enumerated, ranges)


Pos = Tuple[int, int]


# what's the ranges for numbers to be looked up in a line
# given context?
def get_ranges(row: int,
               e_ranges: Dict[int, List[Pos]]) -> Iterable[Pos]:
    match row:
        case 0:
            return [*e_ranges[0], *e_ranges[1]]
        case 139:
            return [*e_ranges[138], *e_ranges[139]]
        case _:
            return [*e_ranges[row - 1], *e_ranges[row], *e_ranges[row + 1]]


# get parts within a line provided context
# e_ranges: ranges per line
def get_parts(line: str,
              row: int,
              e_ranges: Dict[int, List[Pos]]) -> Iterable[int]:
    ranges = get_ranges(row, e_ranges)
    nums = numbers_in_ranges(line, ranges)
    return nums


def main():
    handle = open('input.txt', 'r')
    text = handle.read()
    handle.close()
    lines = text.splitlines()
    enumerated = dict(enumerate(lines))
    e_symbols_locs = map(lambda e: (e[0], symbols_pos(e[1])), enumerated.items())
    e_ranges = map(lambda e: (e[0], list(map(loc_to_range, e[1]))), e_symbols_locs)
    ranges_dict = dict(e_ranges)  # context
    parts = map(lambda e: get_parts(e[1], e[0], ranges_dict), enumerated.items())
    print(sum(map(sum, parts)))
