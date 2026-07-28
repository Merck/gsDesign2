---
name: write-tests
description: Write R package tests using the testit framework. Use when creating new tests or fixing broken tests.
when_to_use: Writing new tests for R functions or fixing broken tests
paths: tests/**/*.R
allowed-tools: Read Bash Edit Write
---

# Writing Tests with testit

This project uses **testit** for testing. testit assertions are plain R expressions — no DSL, no `expect_*` vocabulary.

## File Structure

```
tests/
├── test-all.R              # Runner: library(testit); test_pkg("gsDesign2")
└── testit/
    ├── helper.R            # Shared setup (auto-sourced before test files)
    ├── helper-*.R          # Additional helpers (also auto-sourced)
    ├── fixtures/           # Test data (.Rdata, .rds, etc.)
    ├── test-*.R            # Test files
    └── test-*.md           # Snapshot files (standalone, no .R file needed)
```

## Core Pattern

```r
assert("description of what is being tested", {
  (expression_that_should_be_TRUE)
  (another_expression_that_should_be_TRUE)
})
```

Every expression wrapped in `()` inside `assert()` must evaluate to a logical value or vector of all `TRUE`. testit supports logical vectors directly, so `all()` is unnecessary — just write `(x > 0)` instead of `(all(x > 0))`.

## Assertion Patterns

### Exact identity (preferred default)

```r
(actual %==% expected)
```

`%==%` is an alias for `identical()` that prints `str()` of both sides on failure. Use this for all comparisons — including numeric — when exact identity holds.

**Watch out for operator precedence**: `%==%` binds more tightly than arithmetic and many other operators. For example, `1+1 %==% 2-0` does NOT work as intended — it parses as `1 + (1 %==% 2) - 0`. When LHS or RHS contains operators or compound expressions, wrap them in `()`: `((1+1) %==% (2-0))`. Better yet, compute both sides beforehand:

```r
res <- 1 + 1
expected <- 2 - 0
(res %==% expected)
```

### Approximate numeric equality (when exact identity is not possible)

```r
(all.equal(actual, expected))
(all.equal(actual, expected, tolerance = 0.005))
```

Only fall back to `all.equal()` when floating-point arithmetic makes exact identity impossible (e.g., results from iterative algorithms, cross-platform differences).

### `==` (last resort for numeric)

Use `==` only when `%==%` cannot work due to type mismatch that cannot be resolved by coercion (e.g., comparing a double result against an integer literal). Prefer `%==%` with a matching type whenever possible — for example, `(nrow(df) %==% 10L)` instead of `(nrow(df) == 10)` since `nrow()` returns an integer.

### Boolean checks

```r
(x > 0)
(is.data.frame(result))
(nrow(df) %==% 10L)
(!is.null(x))
(inherits(obj, "ClassName"))
```

### Error / warning / message checking

```r
(has_error(expr))
(has_error(expr, "partial error message"))
(has_warning(expr))
(has_warning(expr, "partial warning message"))
(!has_error(expr))
```

### Set and containment checks

```r
(setequal(actual_names, expected_names))
(x %in% valid_values)
("column_name" %in% names(df))
```

### String matching

```r
(grepl("pattern", string))
(!grepl("unwanted", string))
```

## Complete Example

```r
assert("AHR results are consistent with simulation", {
  actual <- ahr(
    enroll_rate = enroll_rate,
    fail_rate = fail_rate,
    total_duration = c(12, 24, 36)
  )

  (all.equal(expected$AHR, actual$ahr, tolerance = 0.005))
  (all.equal(expected$Events, actual$event, tolerance = 0.005))
})

assert("input validation rejects bad arguments", {
  (has_error(my_function(x = "not_a_number")))
  (has_error(my_function(x = -1), "must be positive"))
})

assert("output structure is correct", {
  result <- my_function(valid_input)

  (is.data.frame(result))
  (nrow(result) %==% 3L)
  (names(result) %==% c("analysis", "bound", "z"))
  (result$z > 0)
})
```

## Snapshot Tests

A `.md` snapshot file is a standalone test — it does NOT require a paired `.R` file. The `.md` file contains both the code and the expected output. Format:

````markdown
## `function_name()` description

```r
code_to_run()
```

```
expected output here
```
````

testit runs the R code block and compares output to the text block. To initialize, omit the output block and run the tests — testit fills it in automatically.

## Guidelines

1. **Prefer `%==%` for all comparisons** — including numeric — when exact identity holds.
2. **Fall back to `all.equal()` only when exact identity is not possible** (floating-point arithmetic, iterative algorithms, cross-platform differences).
3. **Use `all.equal(..., tolerance = t)` with the tightest tolerance that passes** — don't use overly loose tolerances.
4. **Group related assertions in one `assert()` block** — each block should test one logical concept.
5. **Use descriptive assert messages** — they appear in failure output.
6. **Shared setup goes in `helper*.R` files** — testit auto-sources all `helper*.R` files before test files. Never `source()` them manually.
7. **Load fixture data with `load("fixtures/file.Rdata")`** — paths are relative to `tests/testit/`.
8. **Use `all.equal()` only when exact comparison fails in CI** — typically macOS produces slightly different floating-point results while `identical()` works fine on Windows/Linux.

## Conditional Execution

Skip an entire file early:

```r
if (!requireNamespace("optional_pkg", quietly = TRUE)) return()
```

Skip a single assertion:

```r
if (requireNamespace("pkg", quietly = TRUE)) assert("uses pkg", {
  ...
})
```

## Running Tests

```bash
Rscript tests/*.R
```

Run a subset of tests by passing a regex filter:

```bash
Rscript tests/*.R --filter=independent-ahr
```

## Common Mistakes to Avoid

- **Don't use `expect_*()`** — those are testthat functions. Use plain R expressions in `()`.
- **Don't use `test_that()`** — use `assert()`.
- **Don't wrap assertions in `isTRUE()`** — testit handles this internally.
- **Don't wrap in `all()`** — testit supports logical vectors in `()` directly.
