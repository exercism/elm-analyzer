#!/bin/sh

set -e # Make script exit when a command fail.
set -u # Exit on usage of undeclared variable.
# set -x # Trace what gets executed.
set -o pipefail # Catch failures in pipes.

# Command line arguments
SLUG="$1"
INPUT_DIR="$2"
OUTPUT_DIR="$3"

# Run analysis
# Temporarily disable -e mode
set +e
elm-review $INPUT_DIR \
        --elmjson $INPUT_DIR/elm.json \
        --config review \
        --rules $SLUG \
        --report=json \
        > $OUTPUT_DIR/raw_analysis.json

cat $OUTPUT_DIR/raw_analysis.json \
        | jq '[.errors | .[] | .errors | .[] | .message | fromjson]' \
        > $OUTPUT_DIR/message_analysis.json 

cat $OUTPUT_DIR/message_analysis.json \
        | node ./bin/cli.js  \
        > $OUTPUT_DIR/analysis.json
set -e

echo Finished
