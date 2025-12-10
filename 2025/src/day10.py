from dataclasses import dataclass
import itertools
import re


@dataclass
class Machine:
    diagram: list[bool]
    buttons: list[list[int]]
    joltage_req: list[int]

    @classmethod
    def from_manual(cls, manual_line: str):
        diagram_str = re.search(r"^\[[.#]+\]", manual_line)
        assert diagram_str is not None

        joltage_str = re.search(r"\{.*\}$", manual_line)
        assert joltage_str is not None

        diagram = [chr == "#" for chr in diagram_str[0].strip("[]")]
        joltage = [int(j) for j in joltage_str[0].strip("{}").split(",")]
        buttons = [
            [int(n) for n in b[0].strip("()").split(",")]  # pyright: ignore [reportAny]
            for b in re.findall(r"(\(\d+(,\d+)*\))", manual_line)  # pyright: ignore [reportAny]
        ]

        return cls(diagram, buttons, joltage)

    def find_fewest_presses(self) -> int:
        for n in range(1, len(self.buttons) + 1):
            for buttons in itertools.combinations(self.buttons, n):
                lights = [False for _ in self.diagram]
                for b in buttons:
                    for light in b:
                        lights[light] = not lights[light]
                if lights == self.diagram:
                    return n

        return -1


def parse_input(input: str) -> list[Machine]:
    return [Machine.from_manual(line) for line in input.strip().splitlines()]


example = """
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
"""

with open("inputs/day10.txt", encoding="utf-8") as file:
    actual = file.read()

machines = parse_input(actual)
p1 = sum(m.find_fewest_presses() for m in machines)
print(f"Part 1: {p1}")
