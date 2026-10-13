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
fn bytecode_overrides_constant_unknown_name() {
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
        .args(["code.huff", "--bytecode", "-c", "TEST_2=0xff"])
        .assert()
        .failure()
        .stdout(predicate::str::is_empty())
        .stderr(predicate::str::contains("Constant override \"TEST_2\" does not match any constant in the source"));

    tmp.close().unwrap();
}

#[test]
fn bytecode_sets_undefined_constant() {
    let mut cmd = cargo_bin_cmd!("hnc");
    let tmp = assert_fs::TempDir::new().unwrap();
    let file = tmp.child("code.huff");
    file.write_str(
        r#"
#define macro STORE(value) = {
  <value> 0x00 mstore
}

#define macro CONSTRUCTOR() = {
  STORE(TEST_1)
  0x20 0x00 return
}

#define macro MAIN() = {}
"#,
    )
    .unwrap();

    // A constant only referenced in the source, never defined, is set by the override
    cmd.current_dir(&tmp)
        .args(["code.huff", "--bytecode", "-c", "TEST_1=0xff"])
        .assert()
        .success()
        .stdout(predicate::str::is_match(r"^60ff5f5260205ff3$").unwrap())
        .stderr(predicate::str::is_empty());

    tmp.close().unwrap();
}

#[test]
fn bytecode_overrides_constant_in_nested_import() {
    let tmp = assert_fs::TempDir::new().unwrap();
    tmp.child("lib/inner/Inner.huff")
        .write_str(
            r#"
#define constant TEST_1 = 0x42

#define macro INNER() = {
  [TEST_1] [TEST_2] add
}
"#,
        )
        .unwrap();
    tmp.child("lib/Outer.huff")
        .write_str(
            r#"
#include "./inner/Inner.huff"

#define macro OUTER() = {
  INNER()
}
"#,
        )
        .unwrap();
    tmp.child("code.huff")
        .write_str(
            r#"
#include "./lib/Outer.huff"

#define macro MAIN() = {
  OUTER()
}
"#,
        )
        .unwrap();

    // Constants defined or only referenced in an import of an import are overridden and set
    let mut cmd = cargo_bin_cmd!("hnc");
    cmd.current_dir(&tmp)
        .args(["code.huff", "--bin-runtime", "-c", "TEST_1=0xff", "TEST_2=0x01"])
        .assert()
        .success()
        .stdout(predicate::str::is_match(r"^60ff600101$").unwrap());

    let mut cmd = cargo_bin_cmd!("hnc");
    cmd.current_dir(&tmp)
        .args(["code.huff", "--bin-runtime", "-c", "TEST_2=0x01", "TEST_3=0x01"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("Constant override \"TEST_3\" does not match any constant in the source"));

    tmp.close().unwrap();
}

#[test]
fn bytecode_overrides_constant_used_by_one_of_many_files() {
    let tmp = assert_fs::TempDir::new().unwrap();
    tmp.child("src/A.huff")
        .write_str(
            r#"
#define constant TEST_1 = 0x42

#define macro MAIN() = {
  [TEST_1] pop
}
"#,
        )
        .unwrap();
    tmp.child("src/B.huff")
        .write_str(
            r#"
#define macro MAIN() = {
  0x01 pop
}
"#,
        )
        .unwrap();

    // An override only has to appear in one of the compiled files
    let mut cmd = cargo_bin_cmd!("hnc");
    cmd.current_dir(&tmp)
        .args(["src", "--bytecode", "-c", "TEST_1=0xff"])
        .assert()
        .success()
        .stdout(predicate::str::contains("60ff50"))
        .stdout(predicate::str::contains("600150"));

    let mut cmd = cargo_bin_cmd!("hnc");
    cmd.current_dir(&tmp)
        .args(["src", "--bytecode", "-c", "TEST_2=0xff"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("Constant override \"TEST_2\" does not match any constant in the source"));

    tmp.close().unwrap();
}

#[test]
fn bin_runtime_does_not_reuse_cached_artifact_with_other_constant_overrides() {
    let tmp = assert_fs::TempDir::new().unwrap();
    let file = tmp.child("code.huff");
    file.write_str(
        r#"
#define constant TEST_1 = 0x42

#define macro MAIN() = {
  [TEST_1] pop
}
"#,
    )
    .unwrap();

    // Write a cached artifact compiled with an override
    let mut cmd = cargo_bin_cmd!("hnc");
    cmd.current_dir(&tmp).args(["./code.huff", "--artifacts", "-c", "TEST_1=0xff"]).assert().success();

    // Each build must use its own overrides, not the ones of the cached artifact
    for (overrides, expected) in [(vec!["-c", "TEST_1=0xff"], "^60ff50$"), (vec!["-c", "TEST_1=0x01"], "^600150$"), (vec![], "^604250$")] {
        let mut cmd = cargo_bin_cmd!("hnc");
        cmd.current_dir(&tmp)
            .args(["./code.huff", "--bin-runtime"])
            .args(overrides)
            .assert()
            .success()
            .stdout(predicate::str::is_match(expected).unwrap());
    }

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

#[test]
fn label_indices_json_includes_scoped_labels() {
    let mut cmd = cargo_bin_cmd!("hnc");
    let tmp = assert_fs::TempDir::new().unwrap();
    let file = tmp.child("code.huff");
    file.write_str(
        r#"
#define macro INNER() = {
    inner jump
    inner:
}

#define macro MAIN() = {
    start jump
    start:
    INNER()
}
"#,
    )
    .unwrap();

    let output = cmd.current_dir(&tmp).args(["code.huff", "-l", "--format", "json"]).output().unwrap();
    assert!(output.status.success());

    let labels: serde_json::Value = serde_json::from_slice(&output.stdout).unwrap();
    assert_eq!(
        labels,
        serde_json::json!([
            { "label": "inner", "offset": 9, "scope": ["MAIN", "INNER_5"] },
            { "label": "start", "offset": 4, "scope": ["MAIN"] },
        ])
    );

    tmp.close().unwrap();
}

#[test]
fn format_requires_label_indices() {
    let mut cmd = cargo_bin_cmd!("hnc");
    cmd.args(["code.huff", "--format", "json"]).assert().failure().stderr(predicate::str::contains("--label-indices"));
}

#[test]
fn test_command_succeeds_when_all_tests_pass() {
    let mut cmd = cargo_bin_cmd!("hnc");
    let tmp = assert_fs::TempDir::new().unwrap();
    let file = tmp.child("code.huff");
    file.write_str(
        r#"
#define test PASSES() = {
  0x01 pop
}
"#,
    )
    .unwrap();

    cmd.current_dir(&tmp)
        .args(["code.huff", "test", "--format", "json"])
        .assert()
        .success()
        .stdout(predicate::str::contains("\"Success\""));

    tmp.close().unwrap();
}

#[test]
fn test_command_fails_when_a_test_reverts() {
    let tmp = assert_fs::TempDir::new().unwrap();
    let file = tmp.child("code.huff");
    file.write_str(
        r#"
#define test PASSES() = {
  0x01 pop
}

#define test REVERTS() = {
  0x00 0x00 revert
}
"#,
    )
    .unwrap();

    // Every report format must signal the failure through the exit code
    for format in ["list", "table", "json"] {
        let mut cmd = cargo_bin_cmd!("hnc");
        cmd.current_dir(&tmp)
            .args(["code.huff", "test", "--format", format])
            .assert()
            .failure()
            .code(1)
            .stdout(predicate::str::contains("REVERTS"));
    }

    tmp.close().unwrap();
}
