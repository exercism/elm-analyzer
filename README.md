# Elm Analyzer

This is an Elm application that follows the specifications of the Exercism automated mentor support project.

See the [project docs here](https://github.com/exercism/docs/tree/main/building/tooling/analyzers).

## Overview

The Elm Analyzer runs static analysis on Exercism exercises and provides automated feedback via a json file.

The Elm Analyzer is built on top of `elm-review` and defines `Rule`s for specific exercises (in particular for learning exercises) or for all exercises.

## How to run

To run the Analyzer, you need to have Elm and `elm-review` installed. 
For development, `elm-format` and `elm-test` are also necessary.

First, compile the Analyzer by running
```shell
./bin/build.sh
```

This will build the project and copy relevant files to into `bin`. 
Then, to run the analyzer on a specific exercise, use

```shell
./bin/run.sh exercise-slug path/to/exercise/folder path/to/output/directory
```

A json file, `analysis.json` will be written in the specified output directory.
For example, you may try to run it on a `two-fer` solution:

```shell
./bin/run.sh two-fer test_data/two-fer/imperfect_solution test_data/two-fer/imperfect_solution
```

## Tests

An overall check of the analyzer can be ran with
```shell
./bin/smoke_test.sh
```

This will run the Analyzer on a predefined set of exercises in `test_data` and compare the outputs to expected outputs.

More tests are defined in the `tests` folder and can be ran with `elm-test`.

## Formatting

The Elm Analyzer source code should be formatted with `elm-format`.

## Analyzing the Analyzer

The Elm Analyzer itself can be analyzed by running `elm-review`. The rules are specified in the `review` folder.

As a rule of thumb, if we expect the students to follow specific rules when writing their solutions, we should hold ourselves to the same standards in our source code.

## Design

### `src/ReviewConfig.elm`

This is where all the rules are defined. 
Newly created rules must be listed in `ruleConfigs` to become active.

### `src/Comment.elm`

This is where the `Comment` type is created, along with some helper functions.
A `Comment` is issued when a rule is broken, and contains all the information necessary to display some advice to the student.

The main body of the advice is hosted on the [`exercism/website-copy`][website-copy-comments] repository. 
When a new exercise is proposed, corresponding messages must be added on that repository at the same time.

A `Comment` has four fields: 
- `name`: a description of what went wrong, for internal use only.
- `comment`: a path pointing to the message location on `exercism/website-copy`.
- `commentType`: the gravity of a comment, ranging from essential to celebratory.
- `params`: a `Dict` of parameters that can be injected into the message on `exercism/website-copy`.

### `src/Exercise/`

This is where the rules for individual exercises are defined. 
Each module should have the name of the exercise slug in PascalCase and expose the individual rules (for unit testing) as well as a `ruleConfig`.

Each rule should provide a `Comment` to be communicated to the student when the rule is broken.

Each module should have a corresponding test module in `tests/Exercise/`. 
Example and exemplar solutions should not trigger any comments, and each rule is expected to have a few unit tests showcasing what a student might write that would break the rule. 
Those tests both insure that the rule is working correctly and justify why that rule is important.

### `src/Common/`

This is where rules that should be applied to all exercises are defined.

Custom rules should follow the same specifications as exercise rules.

Some modules import existing `elm-review` rules available in the Elm community. 
In that case, a custom decoder must be provided to transform the `elm-review` output into a `Comment`. 
Ideally, the main body of the advice should be defined on [`exercism/website-copy`][website-copy-comments] rather than extracted from the rule output and passed whole through `params`.

### `src/Analyzer.elm`

This module exports `functionCalls`, a helper function that can enforce or prohibit the use of specific functions in the code, either in the whole module or in a specific, top-level function.

`functionCalls` possesses some non-trivial features (dealing with function imports, module aliases...) and should be used whenever applicable.

Check the documentation within the module for more information.

### `src/Main.elm`

The Elm Analyzer works in two steps.

1. run `elm-review` on some source code and receive the an output in json form.
2. decode the output, analyze its contents and export the comments with a summary in `analysis.json`.

`Main.elm` handles the second step.

[website-copy-comments]: https://github.com/exercism/website-copy/tree/main/analyzer-comments
