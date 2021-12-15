import sys
from io import StringIO
from typing import Tuple

import numpy as np
from scipy.ndimage import label


def load_map(path: str) -> np.ndarray:
    with open(path, 'r') as f:
        text = '\n'.join([' '.join(list(s)) for s in f.readlines()])
    heightmap = np.loadtxt(StringIO(text), dtype=int)
    return heightmap


def find_local_minima(map: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
    pm = np.pad(map, pad_width=[(0, 0), (0, 1)], constant_values=10)
    minima = np.sign(pm[:, :-1] - pm[:, 1:])
    pm = np.pad(map, pad_width=[(0, 0), (1, 0)], constant_values=10)
    minima += np.sign(pm[:, 1:] - pm[:, :-1])
    pm = np.pad(map, pad_width=[(0, 1), (0, 0)], constant_values=10)
    minima += np.sign(pm[:-1] - pm[1:])
    pm = np.pad(map, pad_width=[(1, 0), (0, 0)], constant_values=10)
    minima += np.sign(pm[1:] - pm[:-1])
    return minima == -4


def find_basins(map: np.ndarray) -> np.ndarray:
    fences = map != 9
    labels, count = label(fences)
    return labels


def get_basin_sizes(basins: np.ndarray) -> np.ndarray:
    sizes = []
    count = basins.max()
    for i in range(1, count + 1):
        sizes.append(np.sum(basins == i))
    sizes.sort(reverse=True)
    return np.array(sizes)


def main(argv):
    heightmap = load_map(argv[1])
    minima = find_local_minima(map=heightmap)
    risk_levels = (heightmap + 1) * minima
    print(f'Part 1 | Risk level of all minima: {risk_levels.sum()}')
    basins = find_basins(heightmap)
    print(f'Part 2 | Product of three largest basins: {np.product(get_basin_sizes(basins)[:3])}')


if __name__ == '__main__':
    main(sys.argv)
