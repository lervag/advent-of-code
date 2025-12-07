from dataclasses import dataclass
from queue import Queue


example = """
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
"""

with open("inputs/day07.txt", encoding="utf-8") as file:
    actual = file.read()

position = tuple[int, int]


def parse_input(input: str) -> tuple[position, set[position]]:
    lines = input.strip().splitlines()
    start: position = (0, lines[0].find("S"))
    splitters: set[position] = {
        (i + 1, j)
        for i, line in enumerate(lines[1:])
        for j, char in enumerate(line)
        if char == "^"
    }
    return start, splitters


@dataclass
class Beam:
    pos: position
    splitter: position | None = None

    def move(
        self, splitters: set[position], activated_splitters: dict[position, int]
    ) -> list["Beam"]:
        self.pos = (self.pos[0] + 1, self.pos[1])
        value = activated_splitters[self.splitter] if self.splitter else 1

        if self.pos in activated_splitters:
            activated_splitters[self.pos] += value
            return []

        if self.pos[0] > MAX:
            return []

        if self.pos in splitters:
            activated_splitters[self.pos] = value
            return [
                Beam((self.pos[0], self.pos[1] - 1), self.pos),
                Beam((self.pos[0], self.pos[1] + 1), self.pos),
            ]

        return [self]


start, splitters = parse_input(actual)
activated_splitters: dict[position, int] = {}
MAX = max(map(lambda s: s[0], splitters))

q: Queue[Beam] = Queue()
q.put(Beam(start))
while not q.empty():
    b = q.get()
    for n in b.move(splitters, activated_splitters):
        q.put(n)

print(f"Part 1: {len(activated_splitters)}")
print(f"Part 2: {sum(activated_splitters.values()) + 1}")
