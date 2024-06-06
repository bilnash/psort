#' Characteristic-Sorted Portfolios
#'
#' Construct characteristic-sorted portfolios
#'
#' @param factor_data xts object
#' @param forward_returns xts object
#' @param k integer forward returns window in months
#' @param ntiles integer
#'
#' @return list with monthly rankings and portfolios returns
#'
#' @note Formation months are extracted from factor_data.
#' @note Formation date are the beginning of each month.
#' @note Thus, forward_returns must include at least (k-1) months of leading data.
#'
#'
#' @importFrom rlang .data
#' @importFrom zoo index
#' @importFrom purrr set_names
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom zoo coredata
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr ntile
#' @importFrom PerformanceAnalytics Return.calculate
#' @importFrom xts xts
#' @importFrom zoo as.yearmon
#' @importFrom xts merge.xts
#' @importFrom xts cbind.xts
#' @importFrom magrittr %<>%
#' @importFrom utils tail
#' @importFrom stats setNames
#' @importFrom progress progress_bar
#'
#' @export
#'
sort_portfolios <- function(factor_data, forward_returns, k, ntiles = 5) {

    if (!xts::is.xts(factor_data)) stop("invalid argument. not an xts object")

    # For each formation month, rank stock by characteristic factor
    # This is a list FM:TIBBLE
    # Tibble: |stock|factor|rank|
    ranks_by_month <- zoo::index(factor_data) %>%
        purrr::set_names() %>%
        purrr::map(
            function(ym) {
                factor_subset <- factor_data[ym] %>%
                    zoo::coredata() %>%
                    t()
                tibble::tibble(stock = rownames(factor_subset),
                               factor = as.numeric(factor_subset)) %>%
                    dplyr::filter(!is.na(.data$factor)) %>%
                    dplyr::mutate(
                        rank = paste0("Q", dplyr::ntile(x = .data$factor,
                                                        n = ntiles))
                    )
            }
        )

    # Initialize empty portfolios
    sorted_portfolios <- paste0("Q", (1:ntiles)) %>%
        purrr::set_names() %>%
        purrr::map(function(x) xts::xts())

    # Assign stock returns to the corresponding portfolio
    pb <- progress::progress_bar$new(total = length(names(ranks_by_month)))
    for (end_month in names(ranks_by_month)) {
        rankings        <- ranks_by_month[[end_month]]
        holding_start   <- (zoo::as.yearmon(end_month) + (0/12)) %>% format("%Y%m") # SEE NOTE
        holding_end     <- (zoo::as.yearmon(end_month) + ((k-1)/12)) %>% format("%Y%m") # SEE NOTE
        holding_period  <- paste0(holding_start, "/", holding_end)
        for (portfolio_rank in names(sorted_portfolios)) {
            portfolio_elems <- rankings %>%
                dplyr::filter(.data$rank == portfolio_rank) %>%
                .$stock
            sorted_portfolios[[portfolio_rank]] %<>%
                xts::merge.xts(forward_returns[holding_period, portfolio_elems])
        }
        pb$tick()
    }

    # Compute portfolios returns
    sorted_portfolios <- purrr::map2(
        sorted_portfolios,
        names(sorted_portfolios),
        function(portfolio_data, portfolio_rank) {
            portfolio_data %>%
                apply(1, mean, na.rm = TRUE) %>%
                xts::xts(order.by = as.Date(names(.))) %>%
                setNames(portfolio_rank)
        }
    ) %>% do.call(what = xts::cbind.xts, args = .)

    list(

        rankings          = ranks_by_month,
        portfolio_returns = sorted_portfolios

    )

}
