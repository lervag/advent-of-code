from dataclasses import dataclass
from operator import countOf


@dataclass
class Region:
    width: int
    length: int
    count_of_presents: list[int]
    area_region: int
    area_presents: int

    @classmethod
    def from_line(cls, line: str, shapes: list["Shape"]):
        size, counts_str = line.split(": ", 1)
        width_str, length_str = size.split("x")
        width = int(width_str)
        length = int(length_str)
        counts = [int(n) for n in counts_str.split(" ")]

        return cls(
            width=width,
            length=length,
            count_of_presents=counts,
            area_region=width * length,
            area_presents=sum(n * shapes[i].area for i, n in enumerate(counts)),
        )


@dataclass
class Shape:
    area: int
    string: str

    @classmethod
    def from_block(cls, block: str):
        shape_str = block.split(":\n")[1]
        return cls(string=shape_str, area=countOf(shape_str, "#"))


def parse_input(input: str):
    blocks = input.strip().split("\n\n")
    shapes = [Shape.from_block(b) for b in blocks[:6]]
    regions = [
        Region.from_line(line, shapes) for line in blocks[-1].splitlines()
    ]

    return regions, shapes


example = """
0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2
"""

with open("inputs/day12.txt", encoding="utf-8") as file:
    actual = file.read()

regions, shapes = parse_input(actual)

k2 = 0
for r in regions:
    if r.area_region > r.area_presents:
        k2 += 1

print(f"Part 1: {k2}")

# Why does this work?
# for r in regions:
#     if r.area_region > r.area_presents:
#         simple_area = 9*sum(r.count_of_presents)
#         print(r.area_region, r.area_presents, r.area_region - r.area_presents,
#               simple_area, r.area_region - simple_area)
