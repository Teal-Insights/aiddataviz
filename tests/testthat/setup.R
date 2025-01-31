# tests/testthat/setup.R

# This file is run before any tests.
# We set TESTTHAT so .onAttach() knows to skip interactive font checks.

Sys.setenv(TESTTHAT = "true")
