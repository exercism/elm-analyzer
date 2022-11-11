#!/bin/bash

set -e # Make script exit when a command fail.
set -u # Exit on usage of undeclared variable.
# set -x # Trace what gets executed.
set -o pipefail # Catch failures in pipes.

# Command line arguments
WEBSITE_COPY_DIR=$1

# Extract comment paths
COMMENT_PATHS=$(echo -n "--extract-comment-paths" | node ./bin/cli.js)

# Check paths
for path in $COMMENT_PATHS
do
  if [ ! -f "$WEBSITE_COPY_DIR/$path" ]; then
    echo "File $path does not exist"
    exit 1
  fi
done

echo "All paths exist"
