# pyright: strict

from typing import List
from functools import reduce


def parse_times(line: str) -> List[int]:
    [_, *nums_str] = line.split()
    return list(map(int, nums_str))


def parse_distances(line: str) -> List[int]:
    [_, *nums_str] = line.split()
    return list(map(int, nums_str))


def travelled(button: int, travel: int) -> int:
    speed = button
    dist = travel * speed
    return dist


def margin(time: int, record: int) -> int:
    forward = range(0, time + 1)
    backward = reversed(forward)
    dists = map(travelled, forward, backward)
    breaker = filter(lambda d: d > record, dists)
    return len(list(breaker))
    

def main_():
    handle = open('input.txt', 'r')
    [time_str, distance_str] = handle.read().splitlines()
    handle.close()
    times = parse_times(time_str)
    dists = parse_distances(distance_str)
    margins = map(margin, times, dists)
    print(reduce(lambda a, b: a * b, margins))


# part 2

def parse_time(line: str) -> int:
    [_, *times_str] = line.split()
    time_str = ''.join(times_str)
    return int(time_str)


parse_distance = parse_time


def main():
    handle = open('input.txt', 'r')
    [time_str, distance_str] = handle.read().splitlines()
    handle.close()
    time = parse_time(time_str)
    distance = parse_distance(distance_str)
    print(margin(time, distance))


if __name__ == '__main__':
    main()
