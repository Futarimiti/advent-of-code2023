from functools import cache
from typing import Iterable, Tuple


def parse_line(line: str) -> Tuple[str, Iterable[int]]:
    [pat, list_str] = line.split()
    nums = map(int, list_str.split(','))
    return (pat, nums)


def arrangements(pat: Iterable[str], specs: Iterable[int]) -> int:
    match pat, specs:
        case [], []: return 1
        case [], _: return 0
        case _, [] if '#' in pat: return 0
        case _, []: return 1
        case ['.', *cs], _: return arrangements(cs, specs)
        case ['#'], [1]: return 1
        case ['#'], _: return 0
        case ['#', '.', *cs], [1, *spec]: return arrangements(cs, spec)
        case ['#', '.', *_], _: return 0
        case ['#', '#', *_], [1, *_]: return 0
        case ['#', '#', *cs], [n, *spec]: return arrangements(['#', *cs], [n - 1, *spec])
        case ['#', '?', *cs], [1, *spec]: return arrangements(cs, spec)
        case ['#', '?', *cs], [n, *spec]: return arrangements(['#', *cs], [n - 1, *spec])
        case ['?'], [1]: return 1
        case ['?'], _: return 0
        case ['?', c, *cs], _:
            this = arrangements(['#', c, *cs], specs)
            next = arrangements(['.', c, *cs], specs)
            return this + next
        case _:
            raise Exception('arrangements: unexpected pattern: ', (pat, specs))


# @cache
# def arrangements_(pat: Tuple[str, ...], specs: Tuple[int, ...]) -> int:
#     return arrangements(list(pat), list(specs))

@cache
def arrangements_(pat: Tuple[str, ...], specs: Tuple[int, ...]) -> int:
    match pat, specs:
        case [], []: return 1
        case [], _: return 0
        case _, [] if '#' in pat: return 0
        case _, []: return 1
        case ['.', *cs], _: return arrangements_(tuple(cs), specs)
        case ['#'], [1]: return 1
        case ['#'], _: return 0
        case ['#', '.', *cs], [1, *spec]: return arrangements_(tuple(cs), tuple(spec))
        case ['#', '.', *_], _: return 0
        case ['#', '#', *_], [1, *_]: return 0
        case ['#', '#', *cs], [n, *spec]: return arrangements_(('#', *cs), (n - 1, *spec))
        case ['#', '?', *cs], [1, *spec]: return arrangements_(tuple(cs), tuple(spec))
        case ['#', '?', *cs], [n, *spec]: return arrangements_(('#', *cs), (n - 1, *spec))
        case ['?'], [1]: return 1
        case ['?'], _: return 0
        case ['?', c, *cs], _:
            this = arrangements_(('#', c, *cs), specs)
            next = arrangements_(('.', c, *cs), specs)
            return this + next
        case _:
            raise Exception('arrangements: unexpected pattern: ', (pat, specs))

def main():
    handle = open('input.txt', 'r')
    lines = handle.read().splitlines()
    handle.close()
    pairs = map(parse_line, lines)
    times5 = [('?'.join([pat] * 5), tuple(specs) * 5) for pat, specs in pairs]
    arrss = [arrangements_(tuple(pat), specs) for pat, specs in times5]
    print(sum(arrss))


if __name__ == "__main__":
    main()
