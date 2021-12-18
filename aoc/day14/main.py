import itertools
from typing import Tuple, List

import numpy as np


def load(path: str) -> Tuple[str, List[Tuple[str, str]]]:
    with open(path, 'r') as file:
        template = file.readline().strip()
        file.readline()
        rules = [(line.split(' -> ')[0], line.split(' -> ')[1].strip()) for line in file.readlines()]
    return template, rules

def get_pairs(template: str) -> List[str]:
    return list(a+b for a, b in zip(template[:-1], template[1:]))

def list_components(template: str, rules: List[Tuple[str, str]]) -> List[str]:
    elements = set(list(template)).union(list(''.join([a + b for (a, b) in rules])))
    return sorted(list(elements))

def encode_pairs(template: str, mapping: List[str]) -> np.ndarray:
    n = len(mapping)
    encoding = np.zeros((n*n,), dtype=int)
    pairs = sorted([a + b for (a, b) in itertools.product(mapping, mapping)])
    for pair in get_pairs(template):
        index = pairs.index(pair)
        encoding[index] += 1
    return encoding

def compute_dynamics(rules: List[Tuple[str, str]], components: List[str]) -> Tuple[np.ndarray, np.ndarray]:
    n = len(components)
    population_dynamics = np.zeros((n*n, n*n), dtype=int)
    count_dynamics = np.zeros((n*n, n), dtype=int)

    pairs = [a + b for (a, b) in itertools.product(components, components)]
    applied_rules = [f'{pattern[0]}{result}{pattern[1]}' for pattern, result in sorted(rules, key=lambda r: r[0])]

    for i, resolution in enumerate(applied_rules):
        for j, pair in enumerate(pairs):
            population_dynamics[i, j] = sum(int(pair == p) for p in get_pairs(resolution))
        for j, element in enumerate(components):
            count_dynamics[i, j] = 1 if applied_rules[i][1] == element else 0
    return population_dynamics.T, count_dynamics.T

def main():
    template, rules = load('./input/large.txt')
    components = list_components(template=template, rules=rules) # maps elements to indices
    assert len(rules) == len(components) ** 2 # every pair of components should have a rule
    pair_dynamics, count_dynamics = compute_dynamics(rules=rules, components=components)
    component_counts = np.array([template.count(e) for e in components], dtype=int)
    template = encode_pairs(template, mapping=components)
    for i in range(40):
        component_counts += count_dynamics @ template
        template = pair_dynamics @ template

        amax, amin = np.argmax(component_counts), np.argmin(component_counts)
        max_count, min_count = np.amax(component_counts), np.amin(component_counts)
        max_comp, min_comp = components[amax], components[amin]
        score = component_counts[amax]-component_counts[amin]

        if i == 9:
            print(f'Part 1 | Max: {max_comp} = {max_count}, Min: {min_comp} = {min_count}, Result: {score}')
        if i == 39:
            print(f'Part 2 | Max: {max_comp} = {max_count}, Min: {min_comp} = {min_count}, Result: {score}')


if __name__ == '__main__':
    main()