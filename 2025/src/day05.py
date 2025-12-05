from collections.abc import Iterable
from dataclasses import dataclass


@dataclass
class IntRanges:
    ranges: list[range]

    @classmethod
    def from_overlapping_intervals(cls, intervals: Iterable[tuple[int, int]]):
        ranges = sorted(intervals, key=lambda r: r[0])
        result = [ranges[0]]
        for rng in ranges[1:]:
            if rng[0] <= result[-1][1]:
                result[-1] = (result[-1][0], max(result[-1][1], rng[1]))
            else:
                result.append(rng)

        return cls([range(rng[0], rng[1] + 1) for rng in result])

    def __len__(self) -> int:
        return sum(len(rng) for rng in self.ranges)

    def __contains__(self, item: int) -> bool:
        for rng in self.ranges:
            if item in rng:
                return True
        return False


def create_interval(line: str):
    rng = line.split("-")
    return (int(rng[0]), int(rng[1]))


def parse_input(input: str):
    fresh_ranges_str, available_str = input.strip().split("\n\n")

    fresh_ranges = IntRanges.from_overlapping_intervals(
        (create_interval(line) for line in fresh_ranges_str.split())
    )

    available = {int(id) for id in available_str.split()}
    return fresh_ranges, available


example = """
3-5
10-14
16-20
12-18

1
5
8
11
17
32
"""

with open("inputs/day05.txt", encoding="utf-8") as file:
    actual = file.read()

fresh, available = parse_input(actual)

available_fresh = list(filter(lambda id: id in fresh, available))
print(f"Part 1: {len(available_fresh)}")
print(f"Part 2: {len(fresh)}")
