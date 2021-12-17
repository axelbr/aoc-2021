import sys
from typing import Tuple, List

import numpy as np


def load_problem(path: str) -> Tuple[np.ndarray, List[Tuple[str, int]]]:
    with open(path, 'r') as f:
        section = 0
        coordinates, instructions = [], []
        for line in f.readlines():
            if line == "\n":
                section += 1
                continue
            if section == 0:
                coordinates.append([int(line.split(',')[0]), int(line.split(',')[1])])
            elif section == 1:
                line = line.replace('fold along ', '')
                instructions.append((line.split('=')[0], int(line.split('=')[1])))
        coordinates = np.array(coordinates)
        sheet = np.zeros((np.max(coordinates[:, 1])+1, np.max(coordinates[:, 0])+1), dtype=int)
        for c in coordinates:
            sheet[c[1], c[0]] = 1
        return sheet, instructions

def fold(sheet: np.ndarray, instructions: List[Tuple[str, int]]) -> np.ndarray:
    for instruction in instructions:
        axis, index = instruction
        if axis == 'x':
            sheet = sheet[:, :index] + sheet[:, index+1:][:, ::-1]
        elif axis == 'y':
            sheet = sheet[:index] + sheet[index+1:][::-1]
    return np.minimum(sheet, 1)

def main(args):
    sheet, instructions = load_problem('./input/large.txt')
    folded = fold(sheet, instructions[:1])
    print(f'Part 1 | Number of dots: {folded.sum()}')
    print(f'Part 2 | Folded sheet: ')
    folded = fold(sheet, instructions)
    with np.printoptions(linewidth=1000):
        code = np.zeros_like(folded, dtype=str)
        code[folded == 1] = '#'
        code[folded == 0] = ' '
        print(code)

if __name__ == '__main__':
    main(sys.argv)