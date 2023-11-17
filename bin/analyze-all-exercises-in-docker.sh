#!/usr/bin/env bash

set -e # Make script exit when a command fail.
set -u # Exit on usage of undeclared variable.
# set -x # Trace what gets executed.
set -o pipefail # Catch failures in pipes.

# Command line argument
elm_repo_dir=$(realpath $1)

# build docker image
docker build --rm -t elm-analyzer .

# run image passing the arguments
docker run \
    --rm \
    --read-only \
    --network none \
    --mount type=bind,src="$elm_repo_dir",dst=/opt/analyzer/elm_repo \
    --mount type=tmpfs,dst=/tmp \
    --volume "${elm_repo_dir}/bin/analyze_all_exercises.sh:/opt/analyzer/bin/analyze_all_exercises.sh" \
    --entrypoint /opt/analyzer/bin/analyze_all_exercises.sh \
    elm-analyzer \
    /opt/analyzer/elm_repo
