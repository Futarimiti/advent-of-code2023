# pyright: strict

from typing import Iterable, Set, Tuple
from dataclasses import dataclass, replace
from functools import reduce


Coords = Tuple[int, int]


@dataclass
class Image:
    galaxies: Set[Coords]
    height: int
    width: int


def parse_image(txt: str) -> Image:
    lines = txt.splitlines()
    width = len(lines[0])
    height = len(lines)
    return Image(galaxies={ (row, idx) for (row, line) in enumerate(lines)
                                       for idx in galaxy_indices(line) },
                 width=width,
                 height=height)


def galaxy_indices(line: str) -> Set[int]:
    return { i for (i, c) in enumerate(line) if c == '#' }


def dist_btw(g1: Coords, g2: Coords) -> int:
    (x1, y1), (x2, y2) = g1, g2
    return abs(x1 - x2) + abs(y1 - y2)


def is_empty_row(image: Image, row: int) -> bool:
    return row not in { r for (r, _) in image.galaxies }


def is_empty_col(image: Image, col: int) -> bool:
    return col not in { c for (_, c) in image.galaxies }


# expand empty rows in the image, substituting them by <factor> empty rows
def expand_horizontal(image: Image, factor: int) -> Image:
    empty_rows = { row for row in range(0, image.height) if is_empty_row(image, row) }
    descending = sorted(empty_rows, reverse=True)

    def expand_one_row(image: Image, row: int) -> Image:
        affected = { (r, c) for (r, c) in image.galaxies if r > row }
        stationary = image.galaxies - affected
        moved = { (r + factor - 1, c) for (r, c) in affected }
        return replace(image, galaxies=stationary | moved)

    return reduce(expand_one_row, descending, image)


# expand empty columns in the image, substituting them by <factor> empty columns
def expand_vertical(image: Image, factor: int) -> Image:
    empty_cols = { col for col in range(0, image.width) if is_empty_col(image, col) }
    descending = sorted(empty_cols, reverse=True)

    def expand_one_col(image: Image, col: int) -> Image:
        affected = { (r, c) for (r, c) in image.galaxies if c > col }
        stationary = image.galaxies - affected
        moved = { (r, c + factor - 1) for (r, c) in affected }
        return replace(image, galaxies=stationary | moved)

    return reduce(expand_one_col, descending, image)


def expand(image: Image, factor: int) -> Image:
    return expand_vertical(expand_horizontal(image, factor), factor)


Pair = Tuple[Coords, Coords]
def galaxy_pairs(image: Image) -> Iterable[Pair]:
    return { (g1, g2) for g1 in image.galaxies
                      for g2 in image.galaxies
                      if g1 < g2 }


def main():
    handle = open('input.txt', 'r')
    text = handle.read()
    handle.close()
    image = parse_image(text)
    expanded = expand(image, factor=2)
    pairs = galaxy_pairs(expanded)
    dists = [dist_btw(g1, g2) for g1, g2 in pairs]
    total = sum(dists)
    print(total)


if __name__ == '__main__':
    main()
