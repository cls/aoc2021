import numpy as np
import re
import sys

def origami(paper, fold):
    axis, n = fold
    if axis:
        fold1 = paper[:, :n]
        fold2 = paper[:, n+1:]
    else:
        fold1 = paper[:n, :]
        fold2 = paper[n+1:, :]
    y, x = np.maximum(np.array(fold2.shape) - fold1.shape, (0, 0))
    fold1 = np.pad(fold1, [(y, 0), (x, 0)])
    y, x = np.maximum(np.array(fold1.shape) - fold2.shape, (0, 0))
    fold2 = np.pad(fold2, [(0, y), (0, x)])
    fold2 = np.flip(fold2, axis=axis)
    paper = np.logical_or(fold1, fold2)
    return paper

if __name__ == '__main__':
    input_s = sys.stdin.read()
    dots_s, folds_s = input_s.split('\n\n')
    dots = np.array([tuple(map(int, reversed(dot_s.split(',')))) for dot_s in dots_s.splitlines()])
    fold_re = re.compile(r'fold along ([xy])=(\d+)')
    folds = [(1 if axis == 'x' else 0, int(n_s)) for axis, n_s in fold_re.findall(folds_s)]

    axes = np.max(dots, axis=0) + 1
    paper = np.zeros(axes, dtype=bool)

    for y, x in dots:
        paper[y, x] = True

    print(np.sum(origami(paper, folds[0])))

    for fold in folds:
        paper = origami(paper, fold)

    for row in paper:
        for cell in row:
            print('#' if cell else '.', end='')
        print()
