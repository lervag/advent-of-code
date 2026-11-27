# Advent of Code 2026

This year I'll have fun with Rust.
The rig is based on [kaaveland/advent-of-code-rs](https://github.com/kaaveland/advent-of-code-rs).

```sh
mise run new 7    # Create src/days/day07.rs from templates/day.rs
mise run input 7  # Fetch inputs/day07.txt
mise run test 7   # Run the tests for day 7
mise run run 7    # Run both parts with timing
mise run runall   # Run all implemented days
mise run bench    # Benchmark all days with hyperfine (LTO build)
mise run fmt      # Format the code
mise run check    # fmt --check + clippy (pedantic) + tests

cargo run --release -- run 7 --input example.txt
```
