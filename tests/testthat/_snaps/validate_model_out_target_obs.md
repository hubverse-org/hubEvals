# validate_model_out_target_obs() throws warning for unexpected columns [plain]

    Code
      val_result <- validate_model_out_target_obs(model_out_tbl = dplyr::rename(
        model_out_tbl_1, loc = location), target_observations = target_observations_1)
    Message
      ! `target_observations` had 1 unexpected column "location"; expected the columns of `target_observations` to be a subset of "loc", "reference_date", "horizon", "target_end_date", "target", "output_type", "output_type_id", and "observation".

# validate_model_out_target_obs() throws warning for unexpected columns [ansi]

    Code
      val_result <- validate_model_out_target_obs(model_out_tbl = dplyr::rename(
        model_out_tbl_1, loc = location), target_observations = target_observations_1)
    Message
      [33m![39m `target_observations` had 1 unexpected column [34m[34m"location"[34m[39m; expected the columns of `target_observations` to be a subset of [34m[34m"loc"[34m[39m, [34m[34m"reference_date"[34m[39m, [34m[34m"horizon"[34m[39m, [34m[34m"target_end_date"[34m[39m, [34m[34m"target"[34m[39m, [34m[34m"output_type"[34m[39m, [34m[34m"output_type_id"[34m[39m, and [34m[34m"observation"[34m[39m.

# validate_model_out_target_obs() throws warning for unexpected columns [unicode]

    Code
      val_result <- validate_model_out_target_obs(model_out_tbl = dplyr::rename(
        model_out_tbl_1, loc = location), target_observations = target_observations_1)
    Message
      ! `target_observations` had 1 unexpected column "location"; expected the columns of `target_observations` to be a subset of "loc", "reference_date", "horizon", "target_end_date", "target", "output_type", "output_type_id", and "observation".

# validate_model_out_target_obs() throws warning for unexpected columns [fancy]

    Code
      val_result <- validate_model_out_target_obs(model_out_tbl = dplyr::rename(
        model_out_tbl_1, loc = location), target_observations = target_observations_1)
    Message
      [33m![39m `target_observations` had 1 unexpected column [34m[34m"location"[34m[39m; expected the columns of `target_observations` to be a subset of [34m[34m"loc"[34m[39m, [34m[34m"reference_date"[34m[39m, [34m[34m"horizon"[34m[39m, [34m[34m"target_end_date"[34m[39m, [34m[34m"target"[34m[39m, [34m[34m"output_type"[34m[39m, [34m[34m"output_type_id"[34m[39m, and [34m[34m"observation"[34m[39m.

