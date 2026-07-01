# scoringutils' pairwise relative-skill calculation invokes `wilcox.test` on
# score pairs, which emits "cannot compute exact p-value with ties" (and the
# "...with zeroes" variant) whenever the score pairs tie or include zeroes. The
# hubExamples oracle ties heavily, so a relative-skill scoring pass surfaces
# several of these per run, cluttering test output without indicating any real
# problem. This helper muffles only those warnings, so any other warning still
# surfaces (and fails the test), unlike a blanket `suppressWarnings()`.
suppress_wilcox_ties_warnings <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("cannot compute exact p-value", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
