#!/bin/sh

set -e # Make script exit when a command fail.
set -u # Exit on usage of undeclared variable.
# set -x # Trace what gets executed.
set -o pipefail # Catch failures in pipes.

# Temporarily disable -e mode
set +e
# running the analysis
./bin/run.sh two-fer ./test_data/two-fer/perfect_solution ./test_data/two-fer/perfect_solution
./bin/run.sh two-fer ./test_data/two-fer/imperfect_solution ./test_data/two-fer/imperfect_solution

# checking the results
./bin/check_files.sh ./test_data/two-fer/perfect_solution
./bin/check_files.sh ./test_data/two-fer/imperfect_solution

set -e

echo Finished
