# pyright: strict

from dataclasses import dataclass
from typing import Dict, List


@dataclass
class Card:
    cid: int
    win_nums: List[int]
    my_nums: List[int]


def parse_card(line: str) -> Card:
    [label, rest] = line.split(': ')
    [_, cid_str] = label.split()
    cid = int(cid_str)
    [win, my] = rest.split(' | ')
    wins = map(int, win.split())
    mine = map(int, my.split())
    return Card(cid=cid, win_nums=list(wins), my_nums=list(mine))


# <Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53> -> 8
def card_point(card: Card) -> int:
    total_wins = len(list(filter(lambda n: n in card.win_nums, card.my_nums)))
    return 0 if total_wins == 0 else 2 ** (total_wins - 1)


def main_():
    handle = open('input.txt', 'r')
    lines = handle.read().splitlines()
    handle.close()
    cards = map(parse_card, lines)
    points = map(card_point, cards)
    total = sum(points)
    print(total)


# part 2


# card id and its number
Pile = Dict[int, int]

# id to card
Cards = Dict[int, Card]


def cards_won(card: Card) -> List[int]:
    total_wins = len(list(filter(lambda n: n in card.win_nums, card.my_nums)))
    return list(range(card.cid + 1, card.cid + total_wins + 1))


# welp, i tried to be more functional but this is not haskell
# and everything python offers about dict are in place mutations
def update_sequantially(pile: Pile, cards: Cards):
    ids = pile.keys()
    for cid in ids:
        number = pile[cid]
        card = cards[cid]
        wins = cards_won(card)
        for w in wins: pile[w] += number


def main():
    handle = open('input.txt', 'r')
    lines = handle.read().splitlines()
    handle.close()

    pile = { c.cid: 1 for c in map(parse_card, lines) }
    cards = { c.cid: c for c in map(parse_card, lines) }
    update_sequantially(pile, cards)
    print(sum(pile.values()))


if __name__ == '__main__':
    main()
