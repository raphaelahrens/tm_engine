use std::path::PathBuf;

use clap::{Parser, Subcommand};
use eyre::{eyre, Result};

use tm_engine::{
    threat::load_threats,
    build_model,
};


#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,

    model: PathBuf,
    threats: PathBuf,
}

#[derive(Subcommand)]
enum Command {
    Check,
}


fn main() -> Result<()> {
    let args = Args::parse();


    let parent_dir = args
        .model
        .parent()
        .ok_or(eyre!("Could not find parent directory"))?;
    let model = build_model(&args.model, parent_dir)?;
    let threats = load_threats(&args.threats, &model.types)?;

    dbg!(model.query(&threats[0].condition));
    Ok(())
}
