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
