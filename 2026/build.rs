//! Generates the day registry from the files in `src/days/`, so a new day is
//! picked up just by creating `src/days/dayNN.rs`.
use std::{env, fmt::Write, fs, path::Path};

fn main() {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let days_dir = Path::new(&manifest_dir).join("src/days");
    println!("cargo:rerun-if-changed={}", days_dir.display());

    let mut days: Vec<(u8, String)> = fs::read_dir(&days_dir)
        .unwrap()
        .filter_map(|entry| {
            let name = entry.ok()?.file_name().into_string().ok()?;
            let stem = name.strip_suffix(".rs")?;
            let day = stem.strip_prefix("day")?.parse().ok()?;
            Some((day, stem.to_string()))
        })
        .collect();
    days.sort();

    let mut code = String::new();
    for (_, module) in &days {
        let path = days_dir.join(format!("{module}.rs"));
        write!(
            code,
            "#[path = {:?}]\npub mod {module};\n",
            path.display().to_string()
        )
        .unwrap();
    }
    code += "\npub const SOLUTIONS: &[(u8, crate::Solution)] = &[\n";
    for (day, module) in &days {
        write!(
            code,
            "    ({day}, crate::Solution {{\n        \
             part_1: |s| {module}::part_1(s).map(|a| a.to_string()),\n        \
             part_2: |s| {module}::part_2(s).map(|a| a.to_string()),\n    }}),\n"
        )
        .unwrap();
    }
    code += "];\n";

    let out = Path::new(&env::var("OUT_DIR").unwrap()).join("days.rs");
    fs::write(out, code).unwrap();
}
