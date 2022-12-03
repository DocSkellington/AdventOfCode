use std::{
    collections::HashSet,
    fs::{File},
    io::{BufRead, BufReader},
};

fn priority(character: char) -> u32 {
    if character.is_lowercase() {
        return character as u32 - 'a' as u32 + 1;
    }
    else if character.is_uppercase() {
        return character as u32 - 'A' as u32 + 27;
    }
    else {
        return 0;
    }
}

fn task1(reader: BufReader<File>) {
    let mut sum_priorities = 0;

    for line in reader.lines() {
        let line = line.expect("Line");
        let length = line.len() / 2;

        let first_pack = &line[0..length];
        let second_pack = &line[length..];

        let mut character_set_first: HashSet<char> = HashSet::new();
        let mut already_done: HashSet<char> = HashSet::new();

        for character in first_pack.chars() {
            character_set_first.insert(character);
        }

        for character in second_pack.chars() {
            if character_set_first.contains(&character) && !already_done.contains(&character) {
                already_done.insert(character);

                sum_priorities += priority(character);
            }
        }
    }

    println!("{sum_priorities}");
}

fn task2(reader: BufReader<File>) {
    let mut elf = 0;
    let mut common: HashSet<char> = HashSet::new();
    let mut sum_priorities = 0;
    
    for line in reader.lines() {
        let line = line.expect("Line");

        if elf == 0 {
            common.clear();
            for character in line.chars() {
                common.insert(character);
            }
        }
        else {
            let mut for_elf: HashSet<char> = HashSet::new();
            for character in line.chars() {
                for_elf.insert(character);
            }
            common.retain(|&e| for_elf.contains(&e));

        }
            
        if elf == 2 {
            for character in &common {
                sum_priorities += priority(character.to_owned());
            }
        }

        elf = (elf + 1) % 3;
    }

    println!("{sum_priorities}");
}

fn main() {
    let file_path = "input";
    let reader = BufReader::new(File::open(file_path).unwrap());
    task1(reader);

    let reader = BufReader::new(File::open(file_path).unwrap());
    task2(reader);
}
