from dataclasses import dataclass, field
import itertools


@dataclass(frozen=True)
class Point:
    x: int
    y: int


@dataclass
class Edge:
    a: Point
    b: Point
    dx: int
    dy: int
    slope: float
    length: int


def _on_segment(a: Point, b: Point, p: Point) -> bool:
    if not (
        min(a.x, b.x) <= p.x <= max(a.x, b.x)
        and min(a.y, b.y) <= p.y <= max(a.y, b.y)
    ):
        return False

    return (p.y - a.y) * (b.x - a.x) == (p.x - a.x) * (b.y - a.y)


@dataclass
class Polygon:
    points: list[Point]

    edges: list[Edge] = field(default_factory=list, init=False)
    _cache: dict[Point, bool] = field(default_factory=dict, init=False)

    def __post_init__(self):
        for i in range(len(self.points)):
            a = self.points[i]
            b = self.points[(i + 1) % len(self.points)]
            dy = b.y - a.y
            dx = b.x - a.x
            self.edges.append(
                Edge(a, b, dx, dy, dx / dy if dy > 0 else 0.0, dx**2 + dy**2)
            )

    def contains(self, p: Point):
        if p not in self._cache:
            self._cache[p] = self._contains_check(p)

        return self._cache[p]

    def _contains_check(self, p: Point):
        inside = False

        for e in self.edges:
            if _on_segment(e.a, e.b, p):
                return True

            if (e.a.y > p.y) != (e.b.y > p.y):
                x_int = e.a.x + (p.y - e.a.y) * e.slope
                if p.x <= x_int:
                    inside = not inside

        return inside

    def contains_rectangle(self, p1: Point, p2: Point) -> bool:
        if p1.x < p2.x:
            xs = range(p1.x, p2.x + 1)
        else:
            xs = range(p2.x, p1.x + 1)
        if p1.y < p2.y:
            ys = range(p1.y, p2.y + 1)
        else:
            ys = range(p2.y, p1.y + 1)

        edge_points = (
            {Point(p1.x, y) for y in ys}
            | {Point(x, p2.y) for x in xs}
            | {Point(p2.x, y) for y in ys}
            | {Point(x, p1.y) for x in xs}
        )

        return all(map(self.contains, edge_points))


def parse_input(input: str):
    return [
        Point(*[int(x) for x in line.split(",")])
        for line in input.strip().splitlines()
    ]


def area(p1: Point, p2: Point) -> int:
    dx = abs((p2.x - p1.x)) + 1
    dy = abs((p2.y - p1.y)) + 1
    return dx * dy


example = """
7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
"""

with open("inputs/day09.txt", encoding="utf-8") as file:
    actual = file.read()


red_tiles = parse_input(actual)
area_p1 = max(area(p1, p2) for p1, p2 in itertools.combinations(red_tiles, 2))
print(f"Part 1: {area_p1}")

polygon = Polygon(red_tiles)

# Heuristic: The longest edge may be relevant
longest_edge = max(polygon.edges, key=lambda edge: edge.length)
points = [longest_edge.a, longest_edge.b]
area_p2 = max(
    area(p1, p2)
    for p1, p2 in itertools.product(red_tiles, points)
    if polygon.contains_rectangle(p1, p2)
)
print(f"Part 2: {area_p2}")
