# nolint start
pmf_test_model_out_tbl <- function() {
  structure(list(model_id = c("model_A", "model_B", "model_A", 
  "model_B", "model_A", "model_B", "model_A", "model_B", "model_A", 
  "model_B", "model_A", "model_B", "model_A", "model_B", "model_A", 
  "model_B", "model_A", "model_B", "model_A", "model_B", "model_A", 
  "model_B", "model_A", "model_B", "model_A", "model_B", "model_A", 
  "model_B", "model_A", "model_B", "model_A", "model_B", "model_A", 
  "model_B", "model_A", "model_B", "model_A", "model_B", "model_A", 
  "model_B", "model_A", "model_B", "model_A", "model_B", "model_A", 
  "model_B", "model_A", "model_B", "model_A", "model_B", "model_A", 
  "model_B", "model_A", "model_B", "model_A", "model_B", "model_A", 
  "model_B", "model_A", "model_B", "model_A", "model_B", "model_A", 
  "model_B", "model_A", "model_B", "model_A", "model_B", "model_A", 
  "model_B", "model_A", "model_B"), location = c("US", "US", "01", 
  "01", "US", "US", "01", "01", "US", "US", "01", "01", "US", "US", 
  "01", "01", "US", "US", "01", "01", "US", "US", "01", "01", "US", 
  "US", "01", "01", "US", "US", "01", "01", "US", "US", "01", "01", 
  "US", "US", "01", "01", "US", "US", "01", "01", "US", "US", "01", 
  "01", "US", "US", "01", "01", "US", "US", "01", "01", "US", "US", 
  "01", "01", "US", "US", "01", "01", "US", "US", "01", "01", "US", 
  "US", "01", "01"), reference_date = structure(c(18275, 18275, 
  18275, 18275, 18282, 18282, 18282, 18282, 18275, 18275, 18275, 
  18275, 18282, 18282, 18282, 18282, 18275, 18275, 18275, 18275, 
  18282, 18282, 18282, 18282, 18275, 18275, 18275, 18275, 18282, 
  18282, 18282, 18282, 18275, 18275, 18275, 18275, 18282, 18282, 
  18282, 18282, 18275, 18275, 18275, 18275, 18282, 18282, 18282, 
  18282, 18275, 18275, 18275, 18275, 18282, 18282, 18282, 18282, 
  18275, 18275, 18275, 18275, 18282, 18282, 18282, 18282, 18275, 
  18275, 18275, 18275, 18282, 18282, 18282, 18282), class = "Date"), 
      horizon = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 
      1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 0L, 0L, 0L, 
      0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 
      2L, 2L, 2L, 2L, 2L, 2L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 
      1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L
      ), output_type = c("pmf", "pmf", "pmf", "pmf", "pmf", "pmf", 
      "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", 
      "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", 
      "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", 
      "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", 
      "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", 
      "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", 
      "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", "pmf", 
      "pmf", "pmf", "pmf"), output_type_id = c("cat", "cat", "cat", 
      "cat", "cat", "cat", "cat", "cat", "cat", "cat", "cat", "cat", 
      "cat", "cat", "cat", "cat", "cat", "cat", "cat", "cat", "cat", 
      "cat", "cat", "cat", "dog", "dog", "dog", "dog", "dog", "dog", 
      "dog", "dog", "dog", "dog", "dog", "dog", "dog", "dog", "dog", 
      "dog", "dog", "dog", "dog", "dog", "dog", "dog", "dog", "dog", 
      "bird", "bird", "bird", "bird", "bird", "bird", "bird", "bird", 
      "bird", "bird", "bird", "bird", "bird", "bird", "bird", "bird", 
      "bird", "bird", "bird", "bird", "bird", "bird", "bird", "bird"
      ), target_date = structure(c(18275, 18275, 18275, 18275, 
      18282, 18282, 18282, 18282, 18282, 18282, 18282, 18282, 18289, 
      18289, 18289, 18289, 18289, 18289, 18289, 18289, 18296, 18296, 
      18296, 18296, 18275, 18275, 18275, 18275, 18282, 18282, 18282, 
      18282, 18282, 18282, 18282, 18282, 18289, 18289, 18289, 18289, 
      18289, 18289, 18289, 18289, 18296, 18296, 18296, 18296, 18275, 
      18275, 18275, 18275, 18282, 18282, 18282, 18282, 18282, 18282, 
      18282, 18282, 18289, 18289, 18289, 18289, 18289, 18289, 18289, 
      18289, 18296, 18296, 18296, 18296), class = "Date"), value = c(0.0133333333333333, 
      0.0256410256410256, 0.037037037037037, 0.0476190476190476, 
      0.0574712643678161, 0.0666666666666667, 0.0752688172043011, 
      0.0833333333333333, 0.0909090909090909, 0.0980392156862745, 
      0.104761904761905, 0.111111111111111, 0.117117117117117, 
      0.12280701754386, 0.128205128205128, 0.133333333333333, 0.138211382113821, 
      0.142857142857143, 0.147286821705426, 0.151515151515152, 
      0.155555555555556, 0.159420289855072, 0.163120567375887, 
      0.166666666666667, 0.333333333333333, 0.333333333333333, 
      0.333333333333333, 0.333333333333333, 0.333333333333333, 
      0.333333333333333, 0.333333333333333, 0.333333333333333, 
      0.333333333333333, 0.333333333333333, 0.333333333333333, 
      0.333333333333333, 0.333333333333333, 0.333333333333333, 
      0.333333333333333, 0.333333333333333, 0.333333333333333, 
      0.333333333333333, 0.333333333333333, 0.333333333333333, 
      0.333333333333333, 0.333333333333333, 0.333333333333333, 
      0.333333333333333, 0.653333333333333, 0.641025641025641, 
      0.62962962962963, 0.619047619047619, 0.609195402298851, 0.6, 
      0.591397849462366, 0.583333333333333, 0.575757575757576, 
      0.568627450980392, 0.561904761904762, 0.555555555555556, 
      0.54954954954955, 0.543859649122807, 0.538461538461538, 0.533333333333333, 
      0.528455284552846, 0.523809523809524, 0.51937984496124, 0.515151515151515, 
      0.511111111111111, 0.507246376811594, 0.50354609929078, 0.5
      )), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
  -72L))
}
# nolint end
