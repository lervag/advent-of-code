from functools import lru_cache


def find_paths(dmap: dict[str, list[str]], p2: bool = False) -> int:
    @lru_cache(maxsize=None)
    def dfs(device: str, has_dac: bool = False, has_fft: bool = False) -> int:
        if device == "out":
            return 1 if has_dac and has_fft else 0

        count = 0
        for output in dmap[device]:
            new_has_dac = has_dac or output == "dac"
            new_has_fft = has_fft or output == "fft"
            count += dfs(output, new_has_dac, new_has_fft)

        return count

    if p2:
        return dfs("svr")

    return dfs("you", True, True)


def parse_input(input: str) -> dict[str, list[str]]:
    device_map: dict[str, list[str]] = {}
    for line in input.strip().splitlines():
        id, outputs_str = line.split(": ", 1)
        device_map[id] = outputs_str.split()

    return device_map


example = """
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
"""

example2 = """
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
"""

with open("inputs/day11.txt", encoding="utf-8") as file:
    actual = file.read()

device_map = parse_input(actual)
print(f"Part 1: {find_paths(device_map)}")
print(f"Part 2: {find_paths(device_map, p2=True)}")
