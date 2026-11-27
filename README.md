# Advent of Code

My [Advent of Code](https://adventofcode.com/) solutions.
Each year is a self-contained project.
I use [`mise`](https://mise.jdx.dev/) to fetch the relevant tools.

```sh
cd 2025
mise run input [day]     # fetch input to inputs/dayNN.txt (default: today)
mise run leaderboard     # show private leaderboards for the year
elf answer $AOC_YEAR DAY PART ANSWER
elf status $AOC_YEAR
```

## Starting a new year

Create `YYYY/` with its project files and a `mise.toml` that sets
`AOC_YEAR = "YYYY"` and the tools it needs.

