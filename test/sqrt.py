#!/bin/python3

import math

N = 0
W = 16
SQRT_LUT = [
    (
        math.floor(math.sqrt(n << (W - N))),
        math.floor(math.sqrt(((n + 1) << (W - N)) - 1))
    )
    for n in range(2**N)
]


def sqrt(n: int) -> (int, int):
    """
    Return the number r such that r*r <= n but (r+1)*(r+1) > n.
    Also return the number of steps required to find it.
    """
    # Lower and upper bounds are both inclusive
    printed_overflow = False
    wrap = lambda x: x % 2**W
    i = n >> (W - N)
    if i >= len(SQRT_LUT):
        raise RuntimeError(f"Index {i} out of range (for n = {n})")
    lo, hi = SQRT_LUT[i]
    steps = -1
    for i in range(W - N):
        if lo == hi and steps < 0:
            steps = i
        mid = (lo + hi + 1) // 2
        mid_sq = wrap(mid * mid)
        if mid_sq != mid*mid and not printed_overflow:
            print(f"mid*mid overflowed (n={n}, mid={mid})")
            printed_overflow = True
        if mid_sq <= n:
            lo = mid
        else: # mid*mid > n
            hi = mid - 1
    assert lo == hi, f"for n = {n}, lo = {lo}, hi = {hi}"
    return lo, steps


def find_steps(i: int) -> int:
    lo, hi = SQRT_LUT[i]
    return math.ceil(math.log2(hi - lo + 1))


def find_max_steps() -> int:
    return max(find_steps(i) for i in range(len(SQRT_LUT)))


def main() -> None:
    print(f"LUT has {len(SQRT_LUT)} elements")
    max_steps = 0
    for n in range(2**W):
        expected = math.floor(math.sqrt(n))
        actual, steps = sqrt(n)
        max_steps = max(max_steps, steps)
        if actual != expected:
            print(f"n = {n}: {expected} vs {actual}")
    print(f"measured max steps: {max_steps}")
    print(f"theoretical max steps: {find_max_steps()}")


if __name__ == "__main__":
    main()
