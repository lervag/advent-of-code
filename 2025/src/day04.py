position = tuple[int, int]


def parse_input(input: str) -> set[position]:
    return {
        (i, j)
        for i, line in enumerate(filter(None, input.splitlines()))
        for j, char in enumerate(line)
        if char == "@"
    }


def count_neighbours(roll: position, rolls: set[position]) -> int:
    neighbours = {(roll[0] + i - 1, roll[1] + j - 1) for i in range(3) for j in range(3)}

    return 8 - len(neighbours - rolls)


def locate_accessible_rolls(rolls: set[position]) -> set[position]:
    return set(filter(lambda r: count_neighbours(r, rolls) < 4, rolls))

example = """
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
"""

with open("inputs/day04.txt", encoding="utf-8") as file:
    actual = file.read()

rolls = parse_input(actual)
accessible = locate_accessible_rolls(rolls)
print(f"Part 1: {len(accessible)}")

current_rolls = rolls
next_rolls = current_rolls - locate_accessible_rolls(current_rolls)
while next_rolls != current_rolls or len(next_rolls) == 0:
    current_rolls = next_rolls
    next_rolls = current_rolls - locate_accessible_rolls(current_rolls)

print(f"Part 2: {len(rolls) - len(next_rolls)}")
