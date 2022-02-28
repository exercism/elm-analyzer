#!/bin/sh

set -euo pipefail

exercise=$1

function installed {
  cmd=$(command -v "${1}")

  [[ -n "${cmd}" ]] && [[ -f "${cmd}" ]]
  return ${?}
}

function die {
  >&2 echo "Fatal: ${@}"
  exit 1
}

function main {
  if [[ ! -f "${exercise}/expected_analysis.json" ]]; then
    echo "🔥 ${exercise}: expected expected_analysis.json to exist 🔥"
    exit 1
  fi

  if [[ ! -f "${exercise}/analysis.json" ]]; then
    echo "🔥 ${exercise}: expected analysis.json to exist on successful run 🔥"
    exit 1
  fi

  jq -S . ${exercise}/expected_analysis.json > /tmp/expected.json
  jq -S . ${exercise}/analysis.json > /tmp/actual.json
  if ! diff /tmp/expected.json /tmp/actual.json ;then
    echo "🔥 ${exercise}: expected ${exercise}/analysis.json to equal ${exercise}/expected_analysis.json on successful run 🔥"
    exit 1
  fi

  echo "🏁 ${exercise}: expected files present and correct after successful run 🏁"
}

# Check for all required dependencies
for dep in diff jq; do
  installed "${dep}" || die "Missing '${dep}'"
done

main "$@"; exit