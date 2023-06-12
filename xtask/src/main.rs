use std::process::Command;
use xtask_wasm::{anyhow::Result, clap, default_dist_dir};

#[derive(clap::Parser)]
enum Opt {
    Dist(xtask_wasm::Dist),
    Watch(xtask_wasm::Watch),
    Start(xtask_wasm::DevServer),
}

fn main() -> Result<()> {
    let opt: Opt = clap::Parser::parse();

    match opt {
        Opt::Dist(dist) => {
            println!("Generating package...");

            dist.dist_dir_path("dist")
                // .static_dir_path("my-project/static")
                .app_name("lama-web")
                .run_in_workspace(true)
                .run("lama-web")?;
        }
        Opt::Watch(watch) => {
            println!("Watching for changes and check...");

            let mut command = Command::new("cargo");
            command.arg("check");

            watch.run(command)?;
        }
        Opt::Start(dev_server) => {
            println!("Starting the development server...");

            dev_server.arg("dist").start(default_dist_dir(false))?;
        }
    }

    Ok(())
}

// fn main() -> Result<(), anyhow::Error> {
//     xtaskops::tasks::main()
// }
