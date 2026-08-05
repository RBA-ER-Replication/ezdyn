#' Generate baseline profile. Some of this code is not relevant since we aren't
#' extending the forecast.
#' @param M_ MATLAB M_ object.
#' @param oo_ oo_ object.
#' @param data_start Start date of the data sample inside oo_.
#' @param data_end End date of the data sample inside oo_.
#' @export
getBaseline <- function(M_, oo_, data_start, data_end) {
    # Calculate dates
    data_dates <- seq(as.Date(data_start), as.Date(data_end), by="quarter")

    # Extract smoothed shocks from Dynare output structure.
    smoothed_shocks <- oo_$SmoothedShocks |>
        data.frame(row.names = data_dates)
    colnames(smoothed_shocks) <- stringr::str_replace_all(colnames(smoothed_shocks), "\\.", "_")

    # Extract smoothed variables
    baseline <- oo_$SmoothedVariables |>
        data.frame(row.names = data_dates)
    colnames(baseline) <- stringr::str_replace_all(colnames(baseline), "\\.", "_")
    # Extract smoothed constants
    smoother_constant <- oo_$Smoother$Constant |>
        data.frame(row.names = data_dates)
    colnames(smoother_constant) <- stringr::str_replace_all(colnames(smoother_constant), "\\.", "_")

    list(baseline=baseline,
         smoothed_shocks=smoothed_shocks, smoother_constant=smoother_constant)
}
