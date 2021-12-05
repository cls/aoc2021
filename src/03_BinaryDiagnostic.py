import numpy as np
import sys

def to_binary(array):
    return np.sum(np.power(2, np.nonzero(array)))

def power_consumption(data):
    gamma_rate = np.median(data, axis=0)
    epsilon_rate = np.logical_not(gamma_rate)
    return to_binary(gamma_rate) * to_binary(epsilon_rate)

def find_rating(data, bit_criteria):
    i = np.size(data, axis=1) - 1
    while np.size(data, axis=0) > 1:
        data = data[np.nonzero(bit_criteria(data.T[i]))]
        i -= 1
    return data[0]

def life_support_rating(data):
    oxygen_generator_rating = find_rating(data, lambda x: x != np.argmin(np.bincount(x)))
    co2_scrubber_rating     = find_rating(data, lambda x: x == np.argmin(np.bincount(x)))
    return to_binary(oxygen_generator_rating) * to_binary(co2_scrubber_rating)

if __name__ == '__main__':
    input_s = sys.stdin.read()
    data = np.array([[int(bit_s) for bit_s in num_s[::-1]] for num_s in input_s.splitlines()])

    print(power_consumption(data))
    print(life_support_rating(data))
