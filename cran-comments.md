## R CMD check results

0 errors | 0 warnings | 1 note

* This is an update from the current CRAN release (0.3.0 to 0.3.1).

* Suggests or Enhances not in mainstream repositories:
    hubExamples
  Availability using Additional_repositories specification:
    hubExamples   yes   https://hubverse-org.r-universe.dev/

  Tests that use hubExamples are wrapped in `skip_if_not_installed()`, and the
  remaining uses are in examples (guarded with `@examplesIf`) and an article.

## revdepcheck check

We checked the 1 reverse dependency on CRAN (modelimportance), comparing the
current CRAN version of hubEvals to this release. We saw no new problems and no
failures.
