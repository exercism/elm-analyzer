#!/bin/sh

set -e # Make script exit when a command fail.
set -u # Exit on usage of undeclared variable.
# set -x # Trace what gets executed.
set -o pipefail # Catch failures in pipes.

# Command line arguments
# SLUG="$1"
# Remove trailing slash for elm-review
INPUT_DIR=${2%/}
OUTPUT_DIR=${3%/}

# Check if script running in docker
if [ -f solution_cache.tar ]; then
    # Copy solution cache in temporary folder
    rm -rf /tmp/sol
    cp -r $INPUT_DIR /tmp/sol
    tar xf solution_cache.tar -C /tmp/sol
    INPUT_DIR=/tmp/sol
fi

# Temporarily disable -e mode
set +e

# Run analysis
npx elm-review $INPUT_DIR \
        --elmjson $INPUT_DIR/elm.json \
        --config . \
        --report=json \
        --extract \
  > /tmp/elm-review-report.json

# Get comments
cat /tmp/elm-review-report.json | node ./bin/cli.js > $OUTPUT_DIR/analysis.json

# Get tags
jq '.extracts .Tags' /tmp/elm-review-report.json > $OUTPUT_DIR/tags.json

set -e

echo Finished
