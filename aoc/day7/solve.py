import sys
import cvxpy as cp
import numpy as np


def load(problem: str) -> np.ndarray:
    with open(problem, 'r') as file:
        return np.array([int(n) for n in file.readline().split(',')])


def solve_part1(positions: np.ndarray):
    n = positions.shape[0]
    u = cp.Variable(shape=n, integer=True)
    y = positions + u

    objective = cp.Minimize(cp.sum(cp.abs(u)))
    subject_to = [
        y - y[0] == 0
    ]
    problem = cp.Problem(objective, subject_to)
    problem.solve()
    return problem.objective.value, u.value


def solve_part2(positions: np.ndarray):
    '''
    unfortunately no MIQP solver available
    '''
    def objective(x):
        return np.sum(np.abs(x) * (np.abs(x) + 1) / 2, axis=1)

    min, max = np.min(positions), np.max(positions)
    moves = np.stack([i - positions for i in range(min, max+1)], axis=0)
    costs = objective(moves)
    return np.amin(costs), moves[np.argmin(costs)]


def main(argv):
    positions = load(argv[2])
    if argv[1] == '1':
        costs, solution = solve_part1(positions)
    else:
        costs, solution = solve_part2(positions)

    print(f'Optimal position: {(positions + solution)[0]}')
    print(f'Optimal costs: {costs}')


if __name__ == '__main__':
    main(sys.argv)
