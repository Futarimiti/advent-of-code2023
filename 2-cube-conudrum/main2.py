from enum import Enum
from itertools import dropwhile, takewhile
from typing import List, Tuple
from dataclasses import dataclass, replace
from functools import reduce

class Colour(Enum):
    RED = 1
    GREEN = 2
    BLUE = 3

@dataclass
class Take:
    green: int
    red: int
    blue: int

@dataclass
class Game:
    id: int
    takes: List[Take]

# 'red' -> RED
# 'green' -> GREEN
# 'blue' -> BLUE
def parse_colour(s: str) -> Colour:
    match s:
        case 'red':
            return Colour.RED
        case 'green':
            return Colour.GREEN
        case 'blue':
            return Colour.BLUE
        case _:
            raise ValueError(f'Invalid colour: "{s}"')

# '3 blue' -> (BLUE, 3)
# '4 red' -> (RED, 4)
def parse_cubes(s: str) -> Tuple[Colour, int]:
    [amount_str, colour_str] = s.split(' ')
    amount = int(amount_str)
    colour = parse_colour(colour_str)
    return colour, amount

# '3 blue, 4 red' -> Take(blue=3, red=4, green=0)
# '1 red, 2 green, 6 blue' -> Take(red=1, green=2, blue=6)
def parse_take(s: str) -> Take:
    cubes_str = s.split(', ')
    cubes = map(parse_cubes, cubes_str)
    
    def f(acc: Take, x: Tuple[Colour, int]) -> Take:
        (colour, amount) = x
        match colour:
            case Colour.RED:
                return replace(acc, red=acc.red + amount)
            case Colour.GREEN:
                return replace(acc, green=acc.green + amount)
            case Colour.BLUE:
                return replace(acc, blue=acc.blue + amount)

    return reduce(f, cubes, Take(green=0, red=0, blue=0))

# '3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green' -> [Take(blue=3, red=4, green=0), Take(red=1, green=2, blue=6), Take(green=2, red=0, blue=0)]
def parse_takes(s: str) -> List[Take]:
    takes_str = s.split('; ')
    takes = map(parse_take, takes_str)
    return list(takes)

# 'Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green'
# -> Game(id=1, takes=[Take(blue=3, red=4, green=0), Take(red=1, green=2, blue=6), Take(green=2, red=0, blue=0)])
def parse_line(line: str) -> Game:
    line_without_game = line[5:]
    id_str = ''.join(takewhile(lambda c: c.isnumeric(), line_without_game))
    id = int(id_str)

    until_colon = ''.join(dropwhile(lambda c: c != ':', line))
    takes_str = until_colon[2:]
    takes = parse_takes(takes_str)

    return Game(id=id, takes=takes)

def is_possible_take(take: Take) -> bool:
    return take.red <= 12 and take.green <= 13 and take.blue <= 14

def is_possible_game(game: Game) -> bool:
    return all(map(is_possible_take, game.takes))


# part 2

def fewest(takes: List[Take]) -> Take:
    most_red = max(map(lambda t: t.red, takes))
    most_green = max(map(lambda t: t.green, takes))
    most_blue = max(map(lambda t: t.blue, takes))
    return Take(red=most_red, green=most_green, blue=most_blue)

def power(take: Take) -> int:
    return take.red * take.green * take.blue

def main():
    handle = open('input.txt', 'r')
    text = handle.read()
    lines = text.splitlines()
    games = map(parse_line, lines)
    # possible_games = filter(is_possible_game, games)
    # total = sum(map(lambda g: g.id, possible_games))
    # print(total)
    takes = map(lambda g: g.takes, games)
    fewests = map(fewest, takes)
    powers = map(power, fewests)
    print(sum(powers))

if __name__ == '__main__':
    main()
