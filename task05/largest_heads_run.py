#!/usr/bin/env python3
import random


def get_max_run(flips):
    cur_run = 0
    max_run = 0
    for flip in flips:
        if flip:
            cur_run += 1
        else:
            cur_run = 0
        max_run = max(max_run, cur_run)
    return max_run


ITERS = 1000
FLIPS = 100


def main():
    random.seed(123456)
    s = 0
    total = 0
    for _ in range(ITERS):
        s += get_max_run(random.choice([0, 1]) for _ in range(FLIPS))
        total += 1
    print(s, total, s / total)


if __name__ == "__main__":
    main()
