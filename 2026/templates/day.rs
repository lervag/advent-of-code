use anyhow::Result;

fn parse(input: &str) -> Vec<&str> {
    input.lines().collect()
}

pub fn part_1(input: &str) -> Result<usize> {
    let lines = parse(input);
    Ok(lines.len())
}

pub fn part_2(_input: &str) -> Result<usize> {
    anyhow::bail!("not implemented yet")
}

#[cfg(test)]
mod tests {
    use super::*;

    const EX: &str = "\
";

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(EX).unwrap(), 0);
    }

    #[test]
    #[ignore = "part 2 is not solved yet"]
    fn test_part_2() {
        assert_eq!(part_2(EX).unwrap(), 0);
    }
}
