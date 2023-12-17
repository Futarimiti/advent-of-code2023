# pyright: strict

from typing import Tuple
from dataclasses import dataclass
from functools import cache


@dataclass
class Spring:
    pattern: str
    groups: Tuple[int, ...]


def parse_spring(line: str) -> Spring:
    [pat, nums] = line.split()
    groups = tuple(int(n) for n in nums.split(','))
    return Spring(pattern=pat, groups=groups)


@cache
def arrangements_(chars: Tuple[str, ...], groups: Tuple[int, ...]) -> int:
    match chars, groups:
        case [], []: return 1
        case [], _: return 0
        case ['.' | '?'], []: return 1
        case ['.'], _: return 0
        case ['.', *cs], _: return arrangements_(tuple(cs), groups)
        case ['#' | '?'], [1]: return 1
        case ['#'], _: return 0
        case ['#', *_], []: return 0
        case ['#', '.' | '?', *cs], [1, *xs]: return arrangements_(tuple(cs), tuple(xs))
        case ['#', '#' | '?', *cs], [x, *xs]: return arrangements_(('#', *cs), (x - 1, *xs))
        case ['#', '.', *_], _: return 0
        case ['?'], _: return 0
        case ['?', *cs], _: return arrangements_(('#', *cs), groups) + arrangements_(('.', *cs), groups)
        case _, _: raise ValueError(f'invalid pattern: {chars}, {groups}')


def arrangements(spring: Spring) -> int:
    chars = tuple(spring.pattern)
    return arrangements_(chars, spring.groups)


def main_():
    handle = open('input.txt')
    lines = handle.read().splitlines()
    handle.close()
    springs = map(parse_spring, lines)
    arrss = map(arrangements, springs)
    print(sum(arrss))


# part 2

def fivefold(spring: Spring) -> Spring:
    pattern = '?'.join([spring.pattern] * 5)
    groups = spring.groups * 5
    return Spring(pattern=pattern, groups=groups)


def main():
    handle = open('input.txt')
    lines = handle.read().splitlines()
    handle.close()
    springs = map(parse_spring, lines)
    fivefolded = map(fivefold, springs)
    arrss = map(arrangements, fivefolded)
    print(sum(arrss))


if __name__ == "__main__":
    main()
