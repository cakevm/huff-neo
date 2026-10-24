use crate::abi::FunctionParamType;
use crate::prelude::Artifact;
use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

/// Generate solidity interfaces from a vector of artifacts.
///
/// @param artifacts The vector of artifacts to generate interfaces from.
/// @return The vector of generated interfaces.
pub fn gen_sol_interfaces(artifacts: &Vec<Arc<Artifact>>, interface: Option<String>) -> Vec<(PathBuf, String, String)> {
    let mut interfaces = Vec::new();

    for artifact in artifacts {
        if let Some(a) = &artifact.abi {
            let mut defs = Vec::new();
            let mut structs = Vec::new();
            a.events.iter().for_each(|(_, f)| {
                defs.push(format!(
                    "{}event {}({});",
                    "\t",
                    f.name,
                    f.inputs
                        .iter()
                        .map(|i| {
                            format!(
                                "{}{}",
                                sol_type(&i.kind, &mut structs),
                                if i.indexed { String::from(" indexed") } else { String::default() }
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(", "),
                ));
            });
            a.errors.iter().for_each(|(_, e)| {
                defs.push(format!(
                    "{}error {}({});",
                    "\t",
                    e.name,
                    e.inputs.iter().map(|i| param(&i.kind, &mut structs)).collect::<Vec<_>>().join(", "),
                ));
            });
            a.functions.iter().for_each(|(_, f)| {
                defs.push(format!(
                    "{}function {}({}) external{}{};",
                    "\t",
                    f.name,
                    f.inputs.iter().map(|i| param(&i.kind, &mut structs)).collect::<Vec<_>>().join(", "),
                    f.state_mutability.interface_mutability(),
                    if f.outputs.is_empty() {
                        String::default()
                    } else {
                        format!(" returns ({})", f.outputs.iter().map(|o| param(&o.kind, &mut structs)).collect::<Vec<_>>().join(", "))
                    },
                ));
            });

            let interface_name = interface
                .clone()
                .unwrap_or_else(|| format!("I{}", artifact.file.path.split('/').next_back().unwrap().split('.').next().unwrap()));
            let struct_defs = structs.into_iter().map(|(_, def)| def);
            let formatted_str = format!("interface {interface_name} {{\n{}\n}}", struct_defs.chain(defs).collect::<Vec<_>>().join("\n"));
            interfaces.push((Path::new(&artifact.file.path).parent().unwrap().to_path_buf(), interface_name, formatted_str));
        }
    }

    interfaces
}

/// A parameter of a function or error, with the `memory` location its type needs
fn param(kind: &FunctionParamType, structs: &mut Vec<(FunctionParamType, String)>) -> String {
    format!("{}{}", sol_type(kind, structs), if kind.is_memory_type() { " memory" } else { "" })
}

/// The Solidity name of a type. Solidity has no anonymous tuples, so each distinct tuple type is
/// declared once as a struct `TupleN` with the fields `_0`, `_1`, ..., which ABI-encodes the same.
fn sol_type(kind: &FunctionParamType, structs: &mut Vec<(FunctionParamType, String)>) -> String {
    match kind {
        FunctionParamType::Tuple(components) => {
            if let Some(index) = structs.iter().position(|(tuple, _)| tuple == kind) {
                return format!("Tuple{index}");
            }
            // Nested tuples are declared before the struct that contains them
            let fields: Vec<String> =
                components.iter().enumerate().map(|(i, component)| format!("\t\t{} _{i};", sol_type(component, structs))).collect();
            let name = format!("Tuple{}", structs.len());
            structs.push((kind.clone(), format!("\tstruct {name} {{\n{}\n\t}}", fields.join("\n"))));
            name
        }
        FunctionParamType::Array(inner, sizes) => format!(
            "{}{}",
            sol_type(inner, structs),
            sizes.iter().map(|size| if *size == 0 { "[]".to_string() } else { format!("[{size}]") }).collect::<String>()
        ),
        _ => kind.to_string(),
    }
}

/// Export generated solidity interfaces to a file.
///
/// @param interfaces The vector of generated interfaces.
/// @return Unit type if success, error if failure.
pub fn export_interfaces(interfaces: &Vec<(PathBuf, String, String)>) -> Result<(), std::io::Error> {
    for (path, name, interface) in interfaces {
        // A bare file name has an empty parent, so the interface lands next to it
        fs::write(path.join(format!("{name}.sol")), interface)?;
    }
    Ok(())
}
