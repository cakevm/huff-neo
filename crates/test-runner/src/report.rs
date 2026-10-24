use crate::prelude::{HuffFunction, ReportKind, TestResult, TestStatus, decode_huff_internal_calls};
use alloy_primitives::Address;
use comfy_table::{Attribute, Cell, Color, ContentArrangement, Row, Table, modifiers::UTF8_ROUND_CORNERS, presets::UTF8_FULL};
use foundry_evm::decode::decode_console_logs;
use foundry_evm::traces::{
    CallTraceDecoderBuilder, SparsedTraceArena, decode_trace_arena, identifier::SignaturesIdentifier, render_trace_arena_inner,
};
use std::time::Instant;
use yansi::Paint;

/// Print a report of the test results, formatted according to the `report_kind` parameter.
///
/// The verbosity follows forge: call traces at 3 for failing tests and at 4 for every test,
/// with storage changes at 5.
pub async fn print_test_report(results: Vec<TestResult>, report_kind: ReportKind, verbosity: u8, start: Instant) -> eyre::Result<()> {
    // Gather how many of our tests passed *before* generating our report,
    // as we pass ownership of `results` to both the `ReportKind::Table`
    // and `ReportKind::List` arms.
    let n_passed = results.iter().filter(|r| std::mem::discriminant(&r.status) == std::mem::discriminant(&TestStatus::Success)).count();
    let n_results = results.len();

    // Generate and print a report of the test results, formatted based on
    // the `report_kind` input.
    match report_kind {
        ReportKind::Table => {
            let mut table = Table::new();
            table.load_preset(UTF8_FULL).apply_modifier(UTF8_ROUND_CORNERS);
            table.set_header(Row::from(vec![
                Cell::new("Name").fg(Color::Magenta),
                Cell::new("Return Data").fg(Color::Yellow),
                Cell::new("Gas").fg(Color::Cyan),
                Cell::new("Status").fg(Color::Blue),
            ]));
            table.set_content_arrangement(ContentArrangement::DynamicFullWidth);
            table.set_width(120);

            for result in results {
                table.add_row(Row::from(vec![
                    Cell::new(result.name).add_attribute(Attribute::Bold).fg(Color::Cyan),
                    Cell::new(result.return_data.unwrap_or_else(|| String::from("None"))),
                    Cell::new(result.gas.to_string()),
                    Cell::from(result.status),
                ]));
            }

            println!("{table}");
        }
        ReportKind::List => {
            for result in results {
                println!(
                    "[{0}] {1: <15} - {2} {3: <20}",
                    String::from(result.status.clone()),
                    result.name,
                    Paint::yellow("Gas used:"),
                    result.gas
                );
                for failure in &result.failures {
                    println!("{}", Paint::red(&format!("├─ {}: {}", failure.expectation, failure.message)));
                }

                let num_logs = result.logs.as_ref().map_or(0, Vec::len);

                if let Some(return_data) = result.return_data.clone() {
                    println!("├─ {}", Paint::cyan("RETURN DATA"));
                    println!("{} {return_data}", if num_logs == 0 { "└─" } else { "├─" });
                }

                if let Some(logs) = &result.logs {
                    let filtered_logs = decode_console_logs(logs);
                    if !filtered_logs.is_empty() {
                        println!("├─ {}", Paint::cyan("CONSOLE LOGS"));
                        for (i, log) in filtered_logs.iter().enumerate() {
                            println!("{} {log}", if filtered_logs.len() - 1 == i { "└─" } else { "├─" });
                        }
                    }
                }

                let show_traces = verbosity >= 4 || (verbosity == 3 && !result.passed);
                if show_traces && let Some(traces) = result.traces {
                    print_call_trace(traces, result.address, &result.functions, verbosity >= 5).await?;
                }
            }
        }
        ReportKind::Json => {
            if let Ok(o) = serde_json::to_string_pretty(&results) {
                println!("{o}");
            } else {
                eprintln!("Error serializing test results into JSON.");
            }
            return Ok(());
        }
    }
    println!(
        "➜ {} tests passed, {} tests failed ({}%). ⏱ : {}",
        Paint::green(&n_passed),
        Paint::red(&(n_results - n_passed)),
        Paint::yellow(&(n_passed * 100 / n_results)),
        Paint::magenta(&format!("{:.4?}", start.elapsed()))
    );

    Ok(())
}

/// Print the call trace for a given test result.
async fn print_call_trace(
    mut arena: SparsedTraceArena,
    address: Option<Address>,
    functions: &[HuffFunction],
    with_storage_changes: bool,
) -> eyre::Result<()> {
    // Create a signature identifier to decode unknown function selectors and events
    let identifier = SignaturesIdentifier::new(false)?;

    // Build a decoder with the signature identifier
    let decoder = CallTraceDecoderBuilder::new().with_signature_identifier(identifier).build();

    // Decode and render the trace
    decode_trace_arena(&mut arena, &decoder).await;
    if let Some(address) = address {
        decode_huff_internal_calls(&mut arena, address, functions);
    }

    println!("├─ {}", Paint::cyan("TRACES"));
    println!("{}", render_trace_arena_inner(&arena, false, with_storage_changes));

    Ok(())
}
