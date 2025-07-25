use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
};

fn main() {
    // The workspace root directory is not available without walking up the tree
    // https://github.com/rust-lang/cargo/issues/3946
    let ruff_workspace_root = Path::new(&std::env::var("CARGO_MANIFEST_DIR").unwrap())
        .join("..")
        .join("..");
    let ty_workspace_root = ruff_workspace_root.join("..");

    version_info(&ty_workspace_root);

    // If not in a git repository, do not attempt to retrieve commit information
    let git_dir = ty_workspace_root.join(".git");
    if git_dir.exists() {
        commit_info(&git_dir, &ty_workspace_root, false);
    } else {
        // Try if we're inside the ruff repository and, if so, use that commit hash.
        let git_dir = ruff_workspace_root.join(".git");

        if git_dir.exists() {
            commit_info(&git_dir, &ruff_workspace_root, true);
        }
    }

    let target = std::env::var("TARGET").unwrap();
    println!("cargo::rustc-env=RUST_HOST_TARGET={target}");
}

/// Retrieve the version from the `dist-workspace.toml` file and set `TY_VERSION`.
fn version_info(workspace_root: &Path) {
    let dist_file = workspace_root.join("dist-workspace.toml");
    if !dist_file.exists() {
        return;
    }

    println!("cargo:rerun-if-changed={}", dist_file.display());

    let dist_file = fs::read_to_string(dist_file);
    if let Ok(dist_file) = dist_file {
        let lines = dist_file.lines();
        for line in lines {
            if line.starts_with("version =") {
                let (_key, version) = line.split_once('=').unwrap();
                println!(
                    "cargo::rustc-env=TY_VERSION={}",
                    version.trim().trim_matches('"')
                );
                break;
            }
        }
    }
}

/// Retrieve commit information from the Git repository.
fn commit_info(git_dir: &Path, workspace_root: &Path, is_ruff: bool) {
    if let Some(git_head_path) = git_head(git_dir) {
        println!("cargo:rerun-if-changed={}", git_head_path.display());

        let git_head_contents = fs::read_to_string(git_head_path);
        if let Ok(git_head_contents) = git_head_contents {
            // The contents are either a commit or a reference in the following formats
            // - "<commit>" when the head is detached
            // - "ref <ref>" when working on a branch
            // If a commit, checking if the HEAD file has changed is sufficient
            // If a ref, we need to add the head file for that ref to rebuild on commit
            let mut git_ref_parts = git_head_contents.split_whitespace();
            git_ref_parts.next();
            if let Some(git_ref) = git_ref_parts.next() {
                let git_ref_path = git_dir.join(git_ref);
                println!("cargo:rerun-if-changed={}", git_ref_path.display());
            }
        }
    }

    let output = match Command::new("git")
        .arg("log")
        .arg("-1")
        .arg("--date=short")
        .arg("--abbrev=9")
        .arg("--format=%H %h %cd %(describe:tags)")
        .current_dir(workspace_root)
        .output()
    {
        Ok(output) if output.status.success() => output,
        _ => return,
    };
    let stdout = String::from_utf8(output.stdout).unwrap();
    let mut parts = stdout.split_whitespace();
    let mut next = || parts.next().unwrap();
    let _commit_hash = next();
    println!("cargo::rustc-env=TY_COMMIT_SHORT_HASH={}", next());
    println!("cargo::rustc-env=TY_COMMIT_DATE={}", next());

    // Describe can fail for some commits
    // https://git-scm.com/docs/pretty-formats#Documentation/pretty-formats.txt-emdescribeoptionsem
    if let Some(describe) = parts.next() {
        let mut describe_parts = describe.split('-');
        let last_tag = describe_parts.next().unwrap();

        println!(
            "cargo::rustc-env=TY_LAST_TAG={ruff}{last_tag}",
            ruff = if is_ruff { "ruff/" } else { "" }
        );

        // If this is the tagged commit, this component will be missing
        println!(
            "cargo::rustc-env=TY_LAST_TAG_DISTANCE={}",
            describe_parts.next().unwrap_or("0")
        );
    }
}

fn git_head(git_dir: &Path) -> Option<PathBuf> {
    // The typical case is a standard git repository.
    let git_head_path = git_dir.join("HEAD");
    if git_head_path.exists() {
        return Some(git_head_path);
    }
    if !git_dir.is_file() {
        return None;
    }
    // If `.git/HEAD` doesn't exist and `.git` is actually a file,
    // then let's try to attempt to read it as a worktree. If it's
    // a worktree, then its contents will look like this, e.g.:
    //
    //     gitdir: /home/andrew/astral/uv/main/.git/worktrees/pr2
    //
    // And the HEAD file we want to watch will be at:
    //
    //     /home/andrew/astral/uv/main/.git/worktrees/pr2/HEAD
    let contents = fs::read_to_string(git_dir).ok()?;
    let (label, worktree_path) = contents.split_once(':')?;
    if label != "gitdir" {
        return None;
    }
    let worktree_path = worktree_path.trim();
    Some(PathBuf::from(worktree_path))
}
