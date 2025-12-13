from dataclasses import dataclass
from functools import lru_cache
import queue


@dataclass
class Device:
    id: str
    outputs: list[str]

    @classmethod
    def from_line(cls, line: str):
        id, outputs_str = line.split(": ", 1)

        return cls(id, outputs_str.split())

def find_paths_p1(dmap: dict[str, list[str]]) -> int:
    visited = set[str]("you")
    q = queue.Queue[str]()
    [q.put(d) for d in dmap["you"]]

    n = 0
    counter = 0
    while not q.empty() and n < 100:
        current = q.get()
        if current == "out":
            counter += 1
        else:
            visited |= {current}
            [q.put(d) for d in dmap[current] if d not in visited]

    return counter

def find_paths_p2(dmap: dict[str, list[str]]) -> int:
    @lru_cache(maxsize=None)
    def dfs(path: frozenset[str], device: str) -> int:
        if device == "out":
            return 1 if "dac" in path and "fft" in path else 0

        count = 0
        for output in dmap[device]:
            if output not in path:
                count += dfs(path | {output}, output)
        return count

    return dfs(frozenset[str](), "svr")

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
print(f"Part 1: {find_paths_p1(device_map)}")
print(f"Part 2: {find_paths_p2(device_map)}")
