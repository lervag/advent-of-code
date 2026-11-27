# Advent of Code

My [Advent of Code](https://adventofcode.com/) solutions.
Each year is a self-contained project.
I use [`mise`](https://mise.jdx.dev/) to fetch the relevant tools.
And I use [`elf`](https://github.com/snally-dev/elf) as a AoC helper tool.

```sh
cd 2025
mise run input [day]     # fetch input to inputs/dayNN.txt (default: today)
mise run leaderboard     # show private leaderboards for the year
elf answer $AOC_YEAR DAY PART ANSWER
elf status $AOC_YEAR
```

`elf` requires a session cookie in the root `.session` file.
See <https://github.com/snally-dev/elf#configure-your-aoc-session> for how to get this.

## Starting a new year

Create `YYYY/` with its project files and a `mise.toml` that sets
`AOC_YEAR = "YYYY"` and the tools it needs.

