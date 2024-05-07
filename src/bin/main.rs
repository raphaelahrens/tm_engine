use std::path::{Path, PathBuf};

use clap::{Parser, Subcommand};
use eyre::{eyre, Result};
use petgraph::dot::Dot;

use tm_engine::ModelCompiler;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,

    model: PathBuf,
}

#[derive(Subcommand)]
enum Command {
    Check,
}

fn build_model(model: &Path, path: PathBuf) -> Result<()> {
    let mut builder = ModelCompiler::new(vec![path]);

    builder.compile_file(model)?;

    let model = builder.build();
    println!("{:?}", Dot::with_config(&model.graph, &[]));

    Ok(())
}

fn main() -> Result<()> {
    let args = Args::parse();

    let parent_dir = args
        .model
        .parent()
        .ok_or(eyre!("Could not find parent directory"))?;
    build_model(&args.model, parent_dir.to_path_buf())?;
    Ok(())
}
