import itertools
from dataclasses import dataclass
from functools import reduce

example = """
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
"""

with open("inputs/day08.txt", encoding="utf-8") as file:
    actual = file.read()


@dataclass(frozen=True)
class Position:
    x: int
    y: int
    z: int


def parse_input(input: str):
    return {
        Position(*[int(x) for x in line.split(",")])
        for line in input.strip().splitlines()
    }


def distance(p1: Position, p2: Position) -> int:
    return (p2.x - p1.x) ** 2 + (p2.y - p1.y) ** 2 + (p2.z - p1.z) ** 2


def create_distance_map(
    junctions: set[Position],
) -> dict[tuple[Position, Position], int]:
    dmap: dict[tuple[Position, Position], int] = {}
    for p1, p2 in itertools.product(junctions, junctions):
        if p1 == p2 or (p1, p2) in dmap or (p2, p1) in dmap:
            continue
        d = distance(p1, p2)
        dmap[(p1, p2)] = d

    return dmap


def find_circuits(
    junctions: set[Position],
    distance_map: dict[tuple[Position, Position], int],
    slice: int = 3,
) -> list[frozenset[Position]]:
    circuit_map: dict[Position, set[Position]] = {p: {p} for p in junctions}
    sorted_junctions = sorted(distance_map.items(), key=lambda t: t[1])

    for (j1, j2), _ in sorted_junctions[:slice]:
        combined = circuit_map[j1] | circuit_map[j2]
        for p in combined:
            circuit_map[p] = combined

    return list({frozenset(circuit_map[p]) for p in junctions})


def find_last_connected_junctions(
    junctions: set[Position], distance_map: dict[tuple[Position, Position], int]
) -> tuple[Position, Position]:
    circuit_map: dict[Position, set[Position]] = {p: {p} for p in junctions}
    sorted_junctions = sorted(distance_map.items(), key=lambda t: t[1])

    for (j1, j2), _ in sorted_junctions:
        combined = circuit_map[j1] | circuit_map[j2]
        for p in combined:
            circuit_map[p] = combined

        if len(combined) == len(junctions):
            return j1, j2

    # Fallback
    return sorted_junctions[0][0]


test = False
input, slice = (example, 10) if test else (actual, 1000)
junctions = parse_input(input)
distance_map = create_distance_map(junctions)
circuits = find_circuits(junctions, distance_map, slice)
part1 = reduce(
    lambda x, y: x * y,
    sorted(map(lambda s: len(s), circuits), reverse=True)[:3],
)
print(f"Part 1: {part1}")

(j1, j2) = find_last_connected_junctions(junctions, distance_map)
print(f"Part 2: {j1.x * j2.x}")
