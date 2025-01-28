use huff_neo_utils::evm_version::{EVMVersion, SupportedEVMVersions};

#[test]
fn test_has_push0() {
    let evm_version = EVMVersion::new(SupportedEVMVersions::Paris);
    assert!(!evm_version.has_push0());

    let evm_version = EVMVersion::new(SupportedEVMVersions::Shanghai);
    assert!(evm_version.has_push0());

    let evm_version = EVMVersion::new(SupportedEVMVersions::Cancun);
    assert!(evm_version.has_push0());
}
