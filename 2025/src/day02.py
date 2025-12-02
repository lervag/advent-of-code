import re


def to_range(string: str) -> range:
    limits = string.split("-")
    return range(int(limits[0]), int(limits[1]) + 1)


def check_invalid(n: int) -> bool:
    s = str(n)
    d = len(s) // 2
    return s[:d] == s[d:]


def check_invalid_2(n: int) -> bool:
    s = str(n)
    match = re.fullmatch(r"(\d+)\1{1,}", s)
    return match is not None


# input = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
with open("inputs/day02.txt", encoding="utf-8") as file:
    input = file.read()

ranges = (to_range(r) for r in input.split(","))
ids = (number for r in ranges for number in r)
invalids = (i for i in ids if check_invalid(i))
invalids_2 = (i for i in ids if check_invalid_2(i))

print(f"Part 1: {sum(invalids)}")
print(f"Part 2: {sum(invalids_2)}")
