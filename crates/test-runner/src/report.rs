use crate::prelude::{ReportKind, TestResult, TestStatus};
use comfy_table::{modifiers::UTF8_ROUND_CORNERS, presets::UTF8_FULL, Attribute, Cell, Color, ContentArrangement, Row, Table};
use foundry_common::ContractsByArtifact;
use foundry_evm::decode::decode_console_logs;
use foundry_evm::traces::identifier::TraceIdentifiers;
use foundry_evm::traces::{decode_trace_arena, render_trace_arena_inner, CallTraceDecoderBuilder, SparsedTraceArena, TraceKind};
use huff_neo_utils::shell::Verbosity;
use revm_inspectors::tracing::TracingInspector;
use std::time::Instant;
use yansi::Paint;

/// Print a report of the test results, formatted according to the `report_kind` parameter.
pub async fn print_test_report(results: Vec<TestResult>, report_kind: ReportKind, start: Instant, verbosity: Verbosity) {
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
                if result.status == TestStatus::Revert {
                    println!("{}", Paint::red(&format!("└─ Revert: {}", result.revert_reason.clone().unwrap_or_default())));
                }

                let num_logs = match result.inspector.log_collector {
                    Some(ref log_collector) => log_collector.logs.len(),
                    None => 0,
                };

                if let Some(return_data) = result.return_data.clone() {
                    println!("├─ {}", Paint::cyan("RETURN DATA"));
                    println!("{} {return_data}", if num_logs == 0 { "└─" } else { "├─" });
                }

                if let Some(ref log_collector) = result.inspector.log_collector {
                    println!("Console logs:");
                    let filtered_logs = decode_console_logs(&log_collector.logs);
                    for (i, log) in filtered_logs.iter().enumerate() {
                        println!("  {} {log}", if filtered_logs.len() - 1 == i { "└─" } else { "├─" });
                    }
                }

                if let Some(tracer) = result.inspector.tracer.clone() {
                    print_call_trace(verbosity, &result, tracer).await;
                }
            }
        }
        ReportKind::JSON => {
            if let Ok(o) = serde_json::to_string_pretty(&results) {
                println!("{o}");
            } else {
                eprintln!("Error serializing test results into JSON.");
            }
            return;
        }
    }
    println!(
        "➜ {} tests passed, {} tests failed ({}%). ⏱ : {}",
        Paint::green(&n_passed),
        Paint::red(&(n_results - n_passed)),
        Paint::yellow(&(n_passed * 100 / n_results)),
        Paint::magenta(&format!("{:.4?}", start.elapsed()))
    );
}

/// Print the call trace for a given test result.
async fn print_call_trace(verbosity: Verbosity, result: &TestResult, tracer: TracingInspector) {
    // This code is taken and adapted from https://github.com/foundry-rs/foundry

    let known_contracts = ContractsByArtifact::default();

    // Set up trace identifiers.
    let mut identifier = TraceIdentifiers::new().with_local(&known_contracts);

    // Build the trace decoder.
    let builder = CallTraceDecoderBuilder::new().with_known_contracts(&known_contracts).with_verbosity(verbosity);

    let mut decoder = builder.build();

    // We identify addresses if we're going to print *any* trace or gas report.
    let identify_addresses = false;

    let traces = vec![(TraceKind::Execution, SparsedTraceArena { arena: tracer.into_traces(), ignored: Default::default() })];
    // Identify addresses and decode traces.
    let mut decoded_traces = Vec::with_capacity(traces.len());
    for (kind, arena) in &mut traces.clone() {
        if identify_addresses {
            decoder.identify(arena, &mut identifier);
        }

        // verbosity:
        // - 0..3: nothing
        // - 3: only display traces for failed tests
        // - 4: also display the setup trace for failed tests
        // - 5..: display all traces for all tests, including storage changes
        let should_include = match kind {
            TraceKind::Execution => (verbosity == 3 && result.status == TestStatus::Revert) || verbosity >= 4,
            TraceKind::Setup => (verbosity == 4 && result.status == TestStatus::Revert) || verbosity >= 5,
            TraceKind::Deployment => false,
        };

        if should_include {
            decode_trace_arena(arena, &decoder).await.unwrap();
            decoded_traces.push(render_trace_arena_inner(arena, false, verbosity > 4));
        }

        if !decoded_traces.is_empty() {
            println!("Traces:");
            for trace in &decoded_traces {
                println!("{trace}");
            }
        }
    }
}
