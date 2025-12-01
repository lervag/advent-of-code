def parse(line: str) -> int:
    distance = int(line[1:])
    return distance if line[0] == "R" else -distance


current = 50
n_zeros = 0
n_clicks = 0
with open("inputs/day01.txt", encoding="utf-8") as file:
    lines = (parse(line.strip()) for line in file)
    for signed_distance in lines:
        next = current + signed_distance
        if next <= 0:
            n_clicks += -((next - 1) // 100)
            if current == 0:
                n_clicks -= 1
        else:
            n_clicks += next // 100
        current = next % 100
        if current == 0:
            n_zeros += 1

print(f"Day 1 part 1: {n_zeros}")
print(f"Day 1 part 1: {n_clicks}")
