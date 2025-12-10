from collections import Counter
from dataclasses import dataclass, field
import itertools
import re
import time


@dataclass
class Machine:
    diagram: list[bool]
    buttons: list[tuple[int, ...]]
    joltage_target: list[int]
    n_lights: int = field(init=False)
    n_buttons: int = field(init=False)

    def __post_init__(self):
        self.n_lights = len(self.diagram)
        self.n_buttons = len(self.buttons)

    @classmethod
    def from_manual(cls, manual_line: str):
        diagram_str = re.search(r"^\[[.#]+\]", manual_line)
        assert diagram_str is not None

        joltage_str = re.search(r"\{.*\}$", manual_line)
        assert joltage_str is not None

        diagram = [chr == "#" for chr in diagram_str[0].strip("[]")]
        joltage = [int(j) for j in joltage_str[0].strip("{}").split(",")]
        buttons = [
            tuple([int(n) for n in b[0].strip("()").split(",")])  # pyright: ignore [reportAny]
            for b in re.findall(r"(\(\d+(,\d+)*\))", manual_line)  # pyright: ignore [reportAny]
        ]

        return cls(diagram, buttons, joltage)

    def find_fewest_presses(self) -> int:
        for n in range(1, self.n_buttons + 1):
            for buttons in itertools.combinations(self.buttons, n):
                lights = [False] * self.n_lights
                for b in buttons:
                    for light in b:
                        lights[light] = not lights[light]
                if lights == self.diagram:
                    return n

        return -1

    def find_fewest_presses_p2(self, i: int, n: int) -> int:
        start_time = time.time()
        print(
            f"Checking machine {i}/{n} with {self.n_lights} lights and {self.n_buttons} buttons"
        )
        result = self.find_fewest_presses_p2_aux()
        end_time = time.time()
        print(f"-> Time: {end_time - start_time:.4f} seconds")
        return result

    def find_fewest_presses_p2_aux(self) -> int:
        def check_button_sequence(button_sequence: Counter[tuple[int, ...]]):
            counter = [0] * self.n_lights
            for b, count in button_sequence.items():
                for light in b:
                    counter[light] += count
                    if counter[light] > self.joltage_target[light]:
                        return False
            return counter == self.joltage_target

        target_sum = sum(self.joltage_target)
        max_size = max(map(lambda b: len(b), self.buttons))

        n = 0
        while n < 100:
            n += 1
            if n * max_size < target_sum:
                continue
            print(f"-> Trying {n} buttons")
            for button_sequence in itertools.combinations_with_replacement(
                self.buttons, n
            ):
                increase = sum(map(lambda b: len(b), button_sequence))
                if increase != target_sum:
                    continue

                button_sequence_with_count = Counter(button_sequence)
                if check_button_sequence(button_sequence_with_count):
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

p2 = sum(
    m.find_fewest_presses_p2(i + 1, len(machines))
    for i, m in enumerate(machines)
)
print(f"Part 1: {p2}")
