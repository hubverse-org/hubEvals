#' Get data tibble with all output (projection) data to be evaluated
#'
#' @param output_path File path to folder with output data
#' @param loc_data Data file with location information, including variables location and location_name
#' @param weeks.to.eval Sequence of dates to be evaluated
#'
#' @return
#' @export
#'
#' @examples

get_data_tibble <- function(output_path, loc_data, weeks.to.eval){
    require(dplyr)
    require(stringr)
    require(arrow)
    #this chunk reads in all forecasts from all models and dates
    filenames <- list.files(output_path, pattern = "\\.parquet$|\\.csv$", full.names = TRUE, recursive = TRUE)
    filenames <- basename(filenames)
    unique_model_names <- str_split(filenames, "-", simplify = TRUE)

    unique_model_names = data.frame(model = paste0(as.character(unique_model_names[,4]), "-", gsub(".parquet","", as.character(unique_model_names[,5]))),
                                    filename = as.character(filenames),
                                    date.submitted = substr(filenames,1,10),
                                    next.sat = get_next_saturday(as.Date(substr(filenames,1,10)))) %>%
        group_by(model, next.sat) %>%
        summarise(filename = as.character(filename[which.max(as.Date(date.submitted))])) %>%
        ungroup() %>%
        mutate(model = as.character(model),
               next.sat = as.character(next.sat))

    #lists filenames in data-forecasts
    filenames =
        paste0(output_path, "/",
               unique_model_names$model, "/",
               unique_model_names$filename)

    #reads and creates list of all forecasts
    dat_list <- lapply(filenames,
                       FUN = function(x){
                           read_parquet(x, col_types = cols(.default = "c"))
                       }
    )

    models = unique_model_names$model
    unique_models = unique(models)


    all_dat_new <- tibble()
    #create df with forecast data using dat_list created above
    for (i in c(1:length(dat_list))) {
        all_dat_new <- bind_rows(all_dat_new,
                                 dat_list[[i]] %>%
                                     dplyr::select(origin_date, target, horizon, location, age_group, output_type, output_type_id, value) %>%
                                     dplyr::mutate(
                                         target = as.character(target),
                                         model = models[i],
                                         value_inc = as.numeric(value),
                                         output_type_id = as.numeric(output_type_id),
                                         target_end_date = as.Date(origin_date) + (horizon * 7) - 1,
                                         forecast_date = origin_date,
                                         quantile = as.numeric(output_type_id),
                                         type = output_type,
                                         age_group = as.character(age_group)
                                     ))
    }

    all_dat <- all_dat_new %>% filter(origin_date %in% weeks.to.eval) %>% {unique(.)}
    location.names = loc_data %>% select(location, location_name) %>% unique()
    all_dat = left_join(all_dat,location.names, by = c("location"))
    all_dat <- all_dat %>% dplyr::mutate(location_name = as.character(location_name))

    return(all_dat)
}
