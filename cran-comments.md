## Submission

This is a maintenance release of bigmemory (4.6.5). It fixes compilation
with a signed `char` type to address compiler diagnostics, and migrates the
package-level documentation to the `"_PACKAGE"` sentinel (the previous
`@docType package` roxygen tag is deprecated).

## Test environments

* local macOS (aarch64, R 4.5.3)
* win-builder (R-devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

R CMD check passed cleanly with `--as-cran`.

## Reverse dependencies

We will check reverse dependencies and notify maintainers of any packages
affected by this release.
