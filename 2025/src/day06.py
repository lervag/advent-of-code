from dataclasses import dataclass
import functools
from itertools import accumulate


example = """
123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +
"""

with open("inputs/day06.txt", encoding="utf-8") as file:
    actual = file.read()


@dataclass
class Problem:
    nums: list[int]
    operator: str

    def solve(self) -> int:
        match self.operator:
            case "*":
                return functools.reduce(lambda x, y: x * y, self.nums)
            case "+":
                return sum(self.nums)
            case _:
                return 0


def parse_p1(input: str) -> list[Problem]:
    rows = [line.split() for line in input.splitlines() if line.strip()]
    columns: list[list[str]] = list(map(list, zip(*rows)))
    n = len(rows)

    return list(
        map(
            lambda column: Problem(
                nums=[int(e) for e in column[0 : n - 1]], operator=column[n - 1]
            ),
            columns,
        )
    )


def parse_p2(input: str) -> list[Problem]:
    lines = [line for line in input.splitlines() if line.strip()]
    rows_raw = [line.split() for line in lines]
    columns_raw: list[list[str]] = list(map(list, zip(*rows_raw)))
    n = len(rows_raw)
    operators = [col[n - 1] for col in columns_raw]

    sizes = list(map(lambda col: max(map(lambda s: len(s), col)), columns_raw))
    offsets = [0] + list(accumulate(s + 1 for s in sizes))[:-1]
    rows = [
        [f"{line[i : i + size]:<{size}}" for i, size in zip(offsets, sizes)]
        for line in lines[:-1]
    ]
    columns: list[list[str]] = list(map(list, zip(*rows)))

    return list(
        map(
            lambda x: Problem(nums=transposed_numbers(x[0]), operator=x[1]),
            zip(columns, operators),
        )
    )


def transposed_numbers(column: list[str]) -> list[int]:
    x = [
        int(
            "".join(
                [
                    digits[i]
                    for digits in column
                    if len(digits) >= -i and digits[i] != " "
                ]
            )
        )
        for i in reversed(range(-len(column[0]), 0))
    ]
    return x


input = actual
problems = parse_p1(input)
print(f"Part 1: {sum(p.solve() for p in problems)}")

problems = parse_p2(input)
print(f"Part 2: {sum(p.solve() for p in problems)}")
