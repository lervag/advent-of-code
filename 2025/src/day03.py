def find_max(bank: str, digits: int = 2) -> int:
    numbers = [int(n) for n in bank]
    collected = ""

    while digits > 0:
        digits -= 1
        relevant = numbers[:-digits] if digits > 0 else numbers

        pos = 0
        max = 0
        for i, number in enumerate(relevant):
            if number > max:
                pos = i
                max = number

        collected += str(max)
        numbers = numbers[pos + 1 :]

    return int(collected)


with open("inputs/day03.txt", encoding="utf-8") as file:
    input = file.read()

banks = input.split("\n")

part1 = sum(find_max(b) for b in banks)
part2 = sum(find_max(b, 12) for b in banks)

print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
