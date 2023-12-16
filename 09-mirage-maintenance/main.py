# pyright: strict

from typing import List


def parse_line(line: str) -> List[int]:
    num_strs = line.split()
    nums = map(int, num_strs)
    return list(nums)


def find_next(xs: List[int]) -> int:
    if all(map(lambda x: x == 0, xs)):
        return 0
    else:
        next_seq = map(lambda x, y: y - x, xs, xs[1:])
        next_ = find_next(list(next_seq))
        return next_ + xs[-1]


def main():
    handle = open('input.txt', 'r')
    lines = handle.read().splitlines()
    handle.close()
    numss = map(parse_line, lines)
    nexts = map(find_next, numss)
    # nexts = map(lambda nums: find_next(list(reversed(nums))), numss)  # part 2
    total = sum(nexts)
    print(total)

if __name__ == '__main__':
    main()
