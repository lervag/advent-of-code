use anyhow::{Context, Result, bail};
use aoc::{available_days, format_duration, read_input, timed_solution};
use clap::{Parser, Subcommand};
use rayon::prelude::*;
use std::fs;
use std::time::Instant;

/// Advent of Code 2026 runner
#[derive(Parser)]
#[command(arg_required_else_help = true)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Run both parts of a day, with timing
    Run {
        /// Day of the puzzle (default: today)
        day: Option<u8>,
        /// Read input from this file instead of inputs/dayNN.txt
        #[arg(short, long)]
        input: Option<String>,
    },
    /// Run all implemented days, with individual and total timing
    Runall,
}

fn today() -> Result<u8> {
    let out = std::process::Command::new("date").arg("+%d").output()?;
    Ok(String::from_utf8(out.stdout)?.trim().parse()?)
}

fn main() -> Result<()> {
    match Cli::parse().command {
        Command::Run { day, input } => {
            let day = match day {
                Some(d) => d,
                None => today().context("Could not determine today's date")?,
            };
            let input = match input {
                Some(path) => {
                    fs::read_to_string(&path).with_context(|| format!("Could not read {path}"))?
                }
                None => read_input(day)?,
            };
            print!("{}", timed_solution(day, &input)?.0);
        }
        Command::Runall => {
            let days: Vec<u8> = available_days().collect();
            if days.is_empty() {
                bail!("No days implemented yet; start one with `mise run new`");
            }
            let now = Instant::now();
            let reports: Vec<_> = days
                .par_iter()
                .map(|&day| timed_solution(day, &read_input(day)?))
                .collect::<Result<_>>()?;
            let wall = now.elapsed();
            for (report, _) in &reports {
                print!("{report}");
            }
            let cpu = reports.iter().map(|(_, t)| *t).sum();
            println!(
                "All implemented solutions took: {} (sum of parts: {})",
                format_duration(wall),
                format_duration(cpu)
            );
        }
    }
    Ok(())
}
