#!/usr/bin/env bash

# Fork from https://github.com/paradigmxyz/reth/blob/main/.github/assets/check_wasm.sh

set +e  # Disable immediate exit on error

# Array of crates to compile
crates=($(cargo metadata --format-version=1 --no-deps | jq -r '.packages[].name' | grep '^huff-neo' | sort))

# Array of crates to exclude
# Used with the `contains` function.
# shellcheck disable=SC2034
exclude_crates=(
  "huff-neo-test-runner"
)

# Array to hold the results
results=()
# Flag to track if any command fails
any_failed=0

# Function to check if a value exists in an array
contains() {
  local array="$1[@]"
  local seeking=$2
  local in=1
  for element in "${!array}"; do
    if [[ "$element" == "$seeking" ]]; then
      in=0
      break
    fi
  done
  return $in
}

for crate in "${crates[@]}"; do
  if contains exclude_crates "$crate"; then
    results+=("3:⏭️:$crate")
    continue
  fi

export RUSTFLAGS='--cfg getrandom_backend="wasm_js"'
cmd="cargo +stable build -p $crate --target wasm32-unknown-unknown --no-default-features"

  if [ -n "$CI" ]; then
    echo "::group::$cmd"
  else
    printf "\n%s:\n  %s\n" "$crate" "$cmd"
  fi

  set +e  # Disable immediate exit on error
  # Run the command and capture the return code
  $cmd
  ret_code=$?
  set -e  # Re-enable immediate exit on error

  # Store the result in the dictionary
  if [ $ret_code -eq 0 ]; then
    results+=("1:✅:$crate")
  else
    results+=("2:❌:$crate")
    any_failed=1
  fi

  if [ -n "$CI" ]; then
    echo "::endgroup::"
  fi
done

# Sort the results by status and then by crate name
IFS=$'\n' sorted_results=($(sort <<<"${results[*]}"))
unset IFS

# Print summary
echo -e "\nSummary of build results:"
for result in "${sorted_results[@]}"; do
  status="${result#*:}"
  status="${status%%:*}"
  crate="${result##*:}"
  echo "$status $crate"
done

# Exit with a non-zero status if any command fails
exit $any_failed
