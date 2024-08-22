# transform_point_model_out() throws warning for unexpected columns [plain]

    Code
      transform_point_model_out(model_out_tbl = model_out_tbl_1, target_observations = target_observations_1,
        output_type = "mean")
    Message
      ! `target_observations` had 1 unexpected column "location"; expected the columns of `target_observations` to be a subset of "loc", "reference_date", "horizon", "date", "trgt", "output_type", "output_type_id", and "observation".
      Forecast type: point
      Forecast unit:
      model, loc, reference_date, horizon, date, and trgt
    Output
      
         predicted observed      model    loc reference_date horizon       date
             <int>    <int>     <char> <char>         <char>   <int>     <char>
      1:         4        5 test_model     US     2023-01-14       0 2023-01-14
                    trgt
                  <char>
      1: wk inc flu hosp

# transform_point_model_out() throws warning for unexpected columns [ansi]

    Code
      transform_point_model_out(model_out_tbl = model_out_tbl_1, target_observations = target_observations_1,
        output_type = "mean")
    Message
      [33m![39m `target_observations` had 1 unexpected column [34m[34m"location"[34m[39m; expected the columns of `target_observations` to be a subset of [34m[34m"loc"[34m[39m, [34m[34m"reference_date"[34m[39m, [34m[34m"horizon"[34m[39m, [34m[34m"date"[34m[39m, [34m[34m"trgt"[34m[39m, [34m[34m"output_type"[34m[39m, [34m[34m"output_type_id"[34m[39m, and [34m[34m"observation"[34m[39m.
      [34mForecast type: [39mpoint
      [34mForecast unit:[39m
      model, loc, reference_date, horizon, date, and trgt
    Output
      
         predicted observed      model    loc reference_date horizon       date
             <int>    <int>     <char> <char>         <char>   <int>     <char>
      1:         4        5 test_model     US     2023-01-14       0 2023-01-14
                    trgt
                  <char>
      1: wk inc flu hosp

# transform_point_model_out() throws warning for unexpected columns [unicode]

    Code
      transform_point_model_out(model_out_tbl = model_out_tbl_1, target_observations = target_observations_1,
        output_type = "mean")
    Message
      ! `target_observations` had 1 unexpected column "location"; expected the columns of `target_observations` to be a subset of "loc", "reference_date", "horizon", "date", "trgt", "output_type", "output_type_id", and "observation".
      Forecast type: point
      Forecast unit:
      model, loc, reference_date, horizon, date, and trgt
    Output
      
         predicted observed      model    loc reference_date horizon       date
             <int>    <int>     <char> <char>         <char>   <int>     <char>
      1:         4        5 test_model     US     2023-01-14       0 2023-01-14
                    trgt
                  <char>
      1: wk inc flu hosp

# transform_point_model_out() throws warning for unexpected columns [fancy]

    Code
      transform_point_model_out(model_out_tbl = model_out_tbl_1, target_observations = target_observations_1,
        output_type = "mean")
    Message
      [33m![39m `target_observations` had 1 unexpected column [34m[34m"location"[34m[39m; expected the columns of `target_observations` to be a subset of [34m[34m"loc"[34m[39m, [34m[34m"reference_date"[34m[39m, [34m[34m"horizon"[34m[39m, [34m[34m"date"[34m[39m, [34m[34m"trgt"[34m[39m, [34m[34m"output_type"[34m[39m, [34m[34m"output_type_id"[34m[39m, and [34m[34m"observation"[34m[39m.
      [34mForecast type: [39mpoint
      [34mForecast unit:[39m
      model, loc, reference_date, horizon, date, and trgt
    Output
      
         predicted observed      model    loc reference_date horizon       date
             <int>    <int>     <char> <char>         <char>   <int>     <char>
      1:         4        5 test_model     US     2023-01-14       0 2023-01-14
                    trgt
                  <char>
      1: wk inc flu hosp

