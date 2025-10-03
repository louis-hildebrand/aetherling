#!/bin/python3

import math


def sqrt(n: int, w: int) -> int:
    """
    Return the number r such that r*r <= n but (r+1)*(r+1) > n.
    """
    # Lower and upper bounds are both inclusive
    wrap = lambda x: x % 2**w
    lo = 0
    hi = n // 2 + 1
    for _ in range(w):
        mid = (lo + hi) // 2
        if mid == lo:
            mid = mid + 1
        mid_sq = wrap(mid * mid)
        if mid_sq != mid*mid:
            print(f"mid*mid overflowed (n={n}, mid={mid})")
        if mid_sq <= n:
            lo = mid
        else: # mid*mid > n
            hi = mid - 1
    assert lo == hi, f"for n = {n}, lo = {lo}, hi = {hi}"
    return lo


def main() -> None:
    w = 16
    for n in range(1024):
        expected = math.floor(math.sqrt(n))
        actual = sqrt(n, w)
        if actual != expected:
            print(f"n = {n}: {expected} vs {actual}")


if __name__ == "__main__":
    main()
