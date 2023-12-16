# pyright: strict

from dataclasses import dataclass
from enum import Enum, IntEnum
from typing import List, Tuple
from itertools import count, groupby

class HandType(IntEnum):
    HIGH_CARD = 1
    ONE_PAIR = 2
    TWO_PAIR = 3
    THREE_OF_A_KIND = 4
    FULL_HOUSE = 5
    FOUR_OF_A_KIND = 6
    FIVE_OF_A_KIND = 7


class Ordering(Enum):
    LT = 1
    EQ = 2
    GT = 3


char = str
@dataclass
class Card:
    val: char

    def __lt__(self, other: 'Card') -> bool:
        strengths = '23456789TJQKA'
        return strengths.index(self.val) < strengths.index(other.val)


@dataclass
class Hand:
    cards: List[Card]

    def __lt__(self, other: 'Hand') -> bool:
        type1 = hand_type(self)
        type2 = hand_type(other)
        if type1 < type2:
            return True
        elif type1 > type2:
            return False
        else:
            return self.cards < other.cards


def hand_type(hand: Hand) -> HandType:
    match sorted([list(g) for _,g in groupby(sorted(hand.cards))], key=len, reverse=True):
        case [_]:
            return HandType.FIVE_OF_A_KIND
        case [[_,_,_,_],_]:
            return HandType.FOUR_OF_A_KIND
        case [[_,_,_],_]:
            return HandType.FULL_HOUSE
        case [[_,_,_],*_]:
            return HandType.THREE_OF_A_KIND
        case [[_,_],[_,_],_]:
            return HandType.TWO_PAIR
        case [[_,_],*_]:
            return HandType.ONE_PAIR
        case _:
            return HandType.HIGH_CARD


Bid = int
def parse_line(line: str) -> Tuple[Hand, Bid]:
    [hand, bid] = line.split()
    return (Hand(cards=list(map(Card, hand))), int(bid))


def main_():
    handle = open('input.txt', 'r')
    lines = handle.read().splitlines()
    handle.close()
    hand_bid_pairs = map(parse_line, lines)
    sorted_pairs = sorted(hand_bid_pairs, key=lambda x: x[0])
    bids = map(lambda x: x[1], sorted_pairs)
    winnings = map(lambda x, y: x * y, bids, count(1))
    print(sum(winnings))


# part 2
@dataclass
class Card_:
    val: char

    def __lt__(self, other: 'Card') -> bool:
        strengths = 'J23456789TQKA'  # J is the lowest
        return strengths.index(self.val) < strengths.index(other.val)


@dataclass
class Hand_:
    cards: List[Card_]

    def __lt__(self, other: 'Hand_') -> bool:
        type1 = hand_type_(self)
        type2 = hand_type_(other)
        if type1 < type2:
            return True
        elif type1 > type2:
            return False
        else:
            return self.cards < other.cards


def hand_type_(hand: Hand_) -> HandType:
    without_j = [c for c in hand.cards if c.val != 'J']
    js = len([c for c in hand.cards if c.val == 'J'])
    match sorted([list(g) for _,g in groupby(sorted(without_j))], key=len, reverse=True):
        case []:
            return HandType.FIVE_OF_A_KIND
        case xx:
            [xs, *rest] = xx
            match len(xs) + js:
                case 5:
                    return HandType.FIVE_OF_A_KIND
                case 4:
                    return HandType.FOUR_OF_A_KIND
                case 3:
                    [ys, *_] = rest
                    match len(ys):
                        case 2:
                            return HandType.FULL_HOUSE
                        case _:
                            return HandType.THREE_OF_A_KIND
                case 2:
                    [ys, *_] = rest
                    match len(ys):
                        case 2:
                            return HandType.TWO_PAIR
                        case _:
                            return HandType.ONE_PAIR
                case _:
                    return HandType.HIGH_CARD


def parse_line_(line: str) -> Tuple[Hand_, Bid]:
    [hand, bid] = line.split()
    return (Hand_(cards=list(map(Card_, hand))), int(bid))


def main():
    handle = open('input.txt', 'r')
    lines = handle.read().splitlines()
    handle.close()
    hand_bid_pairs = map(parse_line_, lines)
    sorted_pairs = sorted(hand_bid_pairs, key=lambda x: x[0])
    bids = map(lambda x: x[1], sorted_pairs)
    winnings = map(lambda x, y: x * y, bids, count(1))
    print(sum(winnings))


if __name__ == '__main__':
    main()
