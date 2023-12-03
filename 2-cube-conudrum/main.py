# pyright: strict

from enum import Enum
from dataclasses import dataclass, replace
from typing import List, Tuple
from itertools import dropwhile, takewhile
from functools import reduce

class Colour(Enum):
    RED = 1
    GREEN = 2
    BLUE = 3

@dataclass
class Take:
    red: int
    green: int
    blue: int

@dataclass
class Game:
    id: int
    takes: List[Take]

# parse one line of game
def parse_line(line: str) -> Game:
    id = parse_id(line)
    without_game_label = ''.join(list(dropwhile(lambda c: c != ':', line))[2:])  # drop ': '
    takes_str = without_game_label.split('; ')
    takes = list(map(parse_take, takes_str))
    return Game(id=id, takes=takes)

def parse_id(line: str) -> int:
    game_label = takewhile(lambda c: c != ':', line)
    game_id_char_list = list(game_label)[5:]  # drop 'Game '
    game_id_str = ''.join(game_id_char_list)
    return int(game_id_str)

# parse one take of cubes
# '3 blue, 4 red' -> Take(blue=3, red=4, green=0)
def parse_take(s: str) -> Take:
    cubes_str = s.split(', ')
    cubes = map(parse_cubes, cubes_str)

    def temp(take: Take, cubes: Tuple[int, Colour]) -> Take:
        (amount, colour) = cubes
        match colour:
            case Colour.RED:
                return replace(take, red=take.red + amount)
            case Colour.GREEN:
                return replace(take, green=take.green + amount)
            case Colour.BLUE:
                return replace(take, blue=take.blue + amount)

    return reduce(temp, cubes, Take(red=0, green=0, blue=0))

# parse something like '3 blue', '4 red'
# '20 red' -> (20, RED)
def parse_cubes(s: str) -> Tuple[int, Colour]:
    [amount_str, colour_str] = s.split(' ')
    amount = int(amount_str)
    colour = read_colour(colour_str)
    return (amount, colour)

bag: Take = Take(red=12, green=13, blue=14)

def possible_take(bag: Take, take: Take) -> bool:
    return all([bag.red >= take.red, bag.green >= take.green, bag.blue >= take.blue])

def possible_game(bag: Take, game: Game) -> bool:
    return all(map(lambda take: possible_take(bag, take), game.takes))

def read_colour(s: str) -> Colour:
    match s:
        case 'red':
            return Colour.RED
        case 'green':
            return Colour.GREEN
        case 'blue':
            return Colour.BLUE
        case _:
            raise Exception(f'Invalid colour: "{s}"')

def main():
    with open('input.txt', 'r') as f:
        lines = [line[:-1] for line in f.readlines()]

    games = map(parse_line, lines)
    possible_games = filter(lambda game: possible_game(bag, game), games)
    possible_game_ids = map(lambda game: game.id, possible_games)
    id_sum = sum(possible_game_ids)

    print(id_sum)

if __name__ == '__main__':
    main()
