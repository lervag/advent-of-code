use anyhow::{Context, Result};
use std::fmt::Write;
use std::fs;
use std::time::{Duration, Instant};

pub mod days;

pub type Part = fn(&str) -> Result<String>;

pub struct Solution {
    pub part_1: Part,
    pub part_2: Part,
}

pub fn solution(day: u8) -> Option<&'static Solution> {
    days::SOLUTIONS
        .iter()
        .find(|(d, _)| *d == day)
        .map(|(_, s)| s)
}

pub fn available_days() -> impl Iterator<Item = u8> {
    days::SOLUTIONS.iter().map(|(d, _)| *d)
}

pub fn input_path(day: u8) -> String {
    format!("inputs/day{day:02}.txt")
}

pub fn read_input(day: u8) -> Result<String> {
    let path = input_path(day);
    fs::read_to_string(&path)
        .with_context(|| format!("Could not read {path}; fetch it with `mise run input {day}`"))
}

pub fn format_duration(d: Duration) -> String {
    if d.as_millis() > 2 {
        format!("{}ms", d.as_millis())
    } else {
        format!("{}μs", d.as_micros())
    }
}

/// Run both parts of a day and return a report and the total time spent.
pub fn timed_solution(day: u8, input: &str) -> Result<(String, Duration)> {
    let sol = solution(day).with_context(|| format!("No solution for day {day} yet"))?;
    let mut report = String::new();
    let mut total = Duration::ZERO;
    for (part, f) in [(1, sol.part_1), (2, sol.part_2)] {
        let now = Instant::now();
        let answer = f(input);
        let elapsed = now.elapsed();
        total += elapsed;
        match answer {
            Ok(a) => writeln!(
                report,
                "Day {day} part {part}: {}\n{a}",
                format_duration(elapsed)
            )?,
            Err(e) => writeln!(report, "Day {day} part {part}: {e:#}")?,
        }
    }
    Ok((report, total))
}

/// Parse a grid of characters into `((x, y), T)` items, skipping cells where
/// `f` returns `None`.
pub fn grid_parser<'a, T, F>(input: &'a str, f: F) -> impl Iterator<Item = ((i32, i32), T)> + 'a
where
    F: Fn(char) -> Option<T> + Copy + 'a,
{
    input
        .lines()
        .filter(|line| !line.is_empty())
        .enumerate()
        .flat_map(move |(y, line)| {
            line.chars()
                .enumerate()
                .filter_map(move |(x, ch)| f(ch).map(|r| ((x as i32, y as i32), r)))
        })
}
