use assert_cmd::Command;
use assert_fs::prelude::*;
use predicates::prelude::*;

#[test]
fn bytecode_overrides_constant() {
    let mut cmd = Command::cargo_bin("hnc").unwrap();
    let tmp = assert_fs::TempDir::new().unwrap();
    let file = tmp.child("code.huff");
    file.write_str(
        r#"
#define constant TEST = 0x42

#define macro CONSTRUCTOR() = {
  [TEST] 0x00 mstore
  0x20 0x00 return
}

#define macro MAIN() = {}
"#,
    )
    .unwrap();

    cmd.current_dir(&tmp)
        .args(["code.huff", "--bytecode", "-c", "TEST=0xff"])
        .assert()
        .success()
        .stdout(predicate::str::is_match(r"^60ff5f5260205ff3$").unwrap())
        .stderr(predicate::str::is_empty());

    tmp.close().unwrap();
}

#[test]
fn bytecode_overrides_constant_with_numbers() {
    let mut cmd = Command::cargo_bin("hnc").unwrap();
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
        .args(["code.huff", "--bytecode", "-c", "TEST1=0xff"])
        .assert()
        .success()
        .stdout(predicate::str::is_match(r"^60ff5f5260205ff3$").unwrap())
        .stderr(predicate::str::is_empty());

    tmp.close().unwrap();
}
