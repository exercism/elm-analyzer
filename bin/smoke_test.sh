#!/bin/sh

set -e # Make script exit when a command fail.
set -u # Exit on usage of undeclared variable.
# set -x # Trace what gets executed.
set -o pipefail # Catch failures in pipes.

# Temporarily disable -e mode
set +e
# running the analysis
./bin/run.sh two-fer ./test_data/two-fer/perfect_solution ./test_data/two-fer/perfect_solution > /dev/null
./bin/run.sh two-fer ./test_data/two-fer/imperfect_solution ./test_data/two-fer/imperfect_solution > /dev/null
./bin/run.sh strain ./test_data/strain/perfect_solution ./test_data/strain/perfect_solution > /dev/null
./bin/run.sh strain ./test_data/strain/imperfect_solution ./test_data/strain/imperfect_solution > /dev/null

# checking the results
./bin/check_files.sh ./test_data/two-fer/perfect_solution
./bin/check_files.sh ./test_data/two-fer/imperfect_solution
./bin/check_files.sh ./test_data/strain/perfect_solution
./bin/check_files.sh ./test_data/strain/imperfect_solution

set -e

echo Finished
