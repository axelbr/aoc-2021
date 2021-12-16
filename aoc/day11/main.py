import sys
from io import StringIO
from typing import Callable, Tuple

import numpy as np


def load_map(path: str) -> np.ndarray:
    with open(path, 'r') as f:
        text = '\n'.join([' '.join(list(s)) for s in f.readlines()])
    map = np.loadtxt(StringIO(text), dtype=int)
    return map


def flash(pos: np.ndarray, map: np.ndarray):
    for coordinate in pos:
        x, y = coordinate[0], coordinate[1]
        map[x - 1:x + 2, y - 1:y + 2] += 1


def evolve(population: np.ndarray, steps: int, terminate_when_synched=False) -> Tuple[np.ndarray, int, int]:
    flashes = 0
    t = 1
    for t in range(1, steps+1):
        population = np.pad(population, pad_width=1)
        population += 1
        already_flashed = np.zeros_like(population)
        should_flash = population > 9
        while should_flash.sum() > 0:
            pos = np.argwhere(should_flash)
            flash(pos, population)
            flashes += should_flash.sum()
            already_flashed = np.logical_or(already_flashed, should_flash)
            should_flash = np.logical_and(population > 9, 1 - already_flashed)
        population[population > 9] = 0
        population = population[1:-1, 1:-1]
        if np.all(population == 0) and terminate_when_synched:
            break
    return population, flashes, t


def main(args):
    initial = load_map(path='./input/puzzle.txt')
    steps = 1000
    population, flashes, t_synch = evolve(population=initial, steps=steps, terminate_when_synched=True)
    print(f'Population after {steps} steps:\n')
    print(population)
    print()
    print(f'Part 1 | Total number of flashes: {flashes}')
    print(f'Part 2 | Timestep of first synchronization: {t_synch}')



if __name__ == '__main__':
    main(sys.argv)
