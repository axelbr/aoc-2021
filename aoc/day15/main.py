import itertools
import sys
from collections import defaultdict
from io import StringIO
import queue
from typing import Callable, Tuple, List, Any

import numpy as np

class PriorityQueue:
    def __init__(self, min_weight=False):
        self._minimize = min_weight
        self._elements = set()
        self._queue = queue.PriorityQueue()

    def __contains__(self, item):
        return item in self._elements

    def get(self):
        weight, item = self._queue.get()
        if self._minimize:
            weight = weight
        self._elements.remove(item)
        return weight, item

    def put(self, weight: float, item: Any):
        if self._minimize:
            weight = weight
        self._elements.add(item)
        return self._queue.put((weight, item))

    def empty(self):
        return self._queue.empty()

def load_map(path: str) -> np.ndarray:
    with open(path, 'r') as f:
        text = '\n'.join([' '.join(list(s)) for s in f.readlines()])
    map = np.loadtxt(StringIO(text), dtype=int)
    return map

def get_neighbours(map: np.ndarray, position: Tuple[int, int]) -> List[Tuple[int, int]]:
    x, y = position
    neighbours = []
    for i in range(-1, 2):
        for j in range(-1, 2):
            if abs(i) + abs(j) == 1 and 0 <= x + i < map.shape[0] and 0 <= y + j < map.shape[1]:
                neighbours.append((x+i, y+j))
    return neighbours

def find_safest_path(map: np.ndarray, start: Tuple[int, int], goal: Tuple[int, int]) -> List[Tuple[int, int]]:
    risk = np.zeros_like(map) + map.sum()
    risk[start[0], start[1]] = 0
    parents = dict()
    Q = PriorityQueue(min_weight=True)
    Q.put(weight=0, item=start)
    parents[start] = None
    while not Q.empty():
        total_risk, (x, y) = Q.get()
        for xn, yn in get_neighbours(map, position=(x, y)):
            risk_n = map[xn, yn] + total_risk
            if (xn, yn) in Q and risk[xn, yn] >= risk_n:
                risk[xn, yn] = risk_n
                parents[(xn, yn)] = (x, y)
            elif (xn, yn) not in parents:
                risk[xn, yn] = risk_n
                parents[(xn, yn)] = (x, y)
                Q.put(weight=risk_n, item=(xn, yn))
    path = []
    node = goal
    while node in parents:
        path.append(node)
        node = parents[node]
    return list(reversed(path))

def compute_risk(map: np.ndarray, path: List[Tuple[int, int]]):
    return sum(map[x, y] for x, y in path)

def print_path(riskmap: np.ndarray, path: List[Tuple[int, int]]):
    with np.printoptions(linewidth=1000):
        map = np.zeros_like(riskmap, dtype=str)
        map[:] = ' '
        for x, y in path:
            map[x, y] = riskmap[x, y]
        print(map)

def expand_map(map: np.ndarray) -> np.ndarray:
    map = np.concatenate([map + i for i in range(5)], axis=1)
    map = np.concatenate([map + i for i in range(5)], axis=0)
    map[map > 9] %= 9
    return map

def main(args):
    riskmap = load_map(path='./input/large.txt')
    path = find_safest_path(riskmap, start=(0, 0), goal=(riskmap.shape[0]-1, riskmap.shape[1]-1))
    print_path(riskmap, path)
    print(f'Part 1 | Total risk: {compute_risk(riskmap, path[1:])}')
    riskmap = expand_map(map=riskmap)
    path = find_safest_path(riskmap, start=(0, 0), goal=(riskmap.shape[0] - 1, riskmap.shape[1] - 1))
    print(f'Part 2 | Total risk: {compute_risk(riskmap, path[1:])}')


if __name__ == '__main__':
    main(sys.argv)
