#!/usr/bin/env bash

# Synopsis:
# Analyzer run.sh in a docker container
# Takes the same arguments as run.sh
# Builds the Dockerfile
# Runs the docker image passing along the initial arguments

# Arguments:
# $1: exercise slug
# $2: path to solution folder
# $3: path to output directory

# Output:
# Writes the test results to a analysis.json file in the passed-in output directory.

# Example:
# ./run-in-docker.sh two-fer ./relative/path/to/two-fer/solution/folder/ ./relative/path/to/output/directory/

set -e # Make script exit when a command fail.
set -u # Exit on usage of undeclared variable.
# set -x # Trace what gets executed.
set -o pipefail # Catch failures in pipes.

USAGE="bin/run-in-docker.sh <exercise-slug> ./relative/path/to/solution/folder/ ./relative/path/to/output/directory/"

# If arguments not provided, print usage and exit
if [ -z "$1" ] || [ -z "$2" ] || [ -z "$3" ]; then
    echo "Usage: $USAGE"
    exit 1
fi

SLUG="$1"
# Retrieve absolute normalized paths
INPUT_DIR=$(realpath $2)
OUTPUT_DIR=$(realpath $3)

# build docker image
docker build --rm -t elm-analyzer .

# run image passing the arguments
mkdir -p "$OUTPUT_DIR"
docker run \
    --rm \
    --network none \
    --read-only \
    --mount type=bind,src=$INPUT_DIR,dst=/solution \
    --mount type=bind,src=$OUTPUT_DIR,dst=/output \
    --mount type=tmpfs,dst=/tmp \
    elm-analyzer $SLUG /solution/ /output/
