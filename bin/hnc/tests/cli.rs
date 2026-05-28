use assert_cmd::cargo_bin_cmd;
use assert_fs::prelude::*;
use predicates::prelude::*;

#[test]
fn bytecode_overrides_constant() {
    let mut cmd = cargo_bin_cmd!("hnc");
    let tmp = assert_fs::TempDir::new().unwrap();
    let file = tmp.child("code.huff");
    file.write_str(
        r#"
#define constant TEST_1 = 0x42

#define macro CONSTRUCTOR() = {
  [TEST_1] 0x00 mstore
  0x20 0x00 return
}

#define macro MAIN() = {}
"#,
    )
    .unwrap();

    cmd.current_dir(&tmp)
        .args(["code.huff", "--bytecode", "-c", "TEST_1=0xff"])
        .assert()
        .success()
        .stdout(predicate::str::is_match(r"^60ff5f5260205ff3$").unwrap())
        .stderr(predicate::str::is_empty());

    tmp.close().unwrap();
}

#[test]
fn bytecode_overrides_constant_invalid() {
    let mut cmd = cargo_bin_cmd!("hnc");
    let tmp = assert_fs::TempDir::new().unwrap();
    let file = tmp.child("code.huff");
    file.write_str(
        r#"
#define constant TEST1 = 0x42

#define macro CONSTRUCTOR() = {
  [TEST1] 0x00 mstore
  0x20 0x00 return
}

#define macro MAIN() = {}
"#,
    )
    .unwrap();

    cmd.current_dir(&tmp)
        .args(["code.huff", "--bytecode", "-c", "1=0xff"])
        .assert()
        .failure()
        .stdout(predicate::str::is_empty())
        .stderr(predicate::str::contains("must start with letter or underscore"));

    tmp.close().unwrap();
}

#[test]
fn bin_runtime_errors_when_constructor_returns_custom_bytecode() {
    let mut cmd = cargo_bin_cmd!("hnc");
    let tmp = assert_fs::TempDir::new().unwrap();
    let file = tmp.child("code.huff");
    file.write_str(
        r#"
#define macro CONSTRUCTOR() = {
    0x0 0x0 return
}

#define macro MAIN() = {
    __VERBATIM(0xff)
}
"#,
    )
    .unwrap();

    cmd.current_dir(&tmp)
        .args(["code.huff", "--bin-runtime"])
        .assert()
        .failure()
        .stdout(predicate::str::is_empty())
        .stderr(predicate::str::contains("CONSTRUCTOR").and(predicate::str::contains("--bytecode")));

    tmp.close().unwrap();
}

#[test]
fn bin_runtime_succeeds_with_state_init_constructor() {
    let mut cmd = cargo_bin_cmd!("hnc");
    let tmp = assert_fs::TempDir::new().unwrap();
    let file = tmp.child("code.huff");
    file.write_str(
        r#"
#define constant OWNER = 0x0

#define macro CONSTRUCTOR() = {
    caller [OWNER] sstore
}

#define macro MAIN() = {
    __VERBATIM(0xff)
}
"#,
    )
    .unwrap();

    cmd.current_dir(&tmp)
        .args(["code.huff", "--bin-runtime"])
        .assert()
        .success()
        .stdout(predicate::str::is_match(r"^ff$").unwrap())
        .stderr(predicate::str::is_empty());

    tmp.close().unwrap();
}

#[test]
fn bin_runtime_succeeds_without_constructor() {
    let mut cmd = cargo_bin_cmd!("hnc");
    let tmp = assert_fs::TempDir::new().unwrap();
    let file = tmp.child("code.huff");
    file.write_str(
        r#"
#define macro MAIN() = {
    __VERBATIM(0xff)
}
"#,
    )
    .unwrap();

    cmd.current_dir(&tmp)
        .args(["code.huff", "--bin-runtime"])
        .assert()
        .success()
        .stdout(predicate::str::is_match(r"^ff$").unwrap())
        .stderr(predicate::str::is_empty());

    tmp.close().unwrap();
}
