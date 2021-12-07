import numpy as np
import sys

def linear_fuel(crabs):
    distances = np.abs(crabs - np.median(crabs))
    return int(np.sum(distances))

def rounded_triangular_fuel(crabs, rounding):
    distances = np.abs(crabs - rounding(np.mean(crabs)))
    fuel_used = distances * (distances + 1) / 2
    return int(np.sum(fuel_used))

def triangular_fuel(crabs):
    # I don't know if there's a way to determine whether to round up or down without trying both.
    return min(rounded_triangular_fuel(crabs, np.floor), rounded_triangular_fuel(crabs, np.ceil))

if __name__ == '__main__':
    input_s = sys.stdin.read()
    crabs = np.array([int(crab_s) for crab_s in input_s.split(',')])

    print(linear_fuel(crabs))
    print(triangular_fuel(crabs))
