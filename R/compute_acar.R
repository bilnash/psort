#' Compute ACAR
#'
#' See Bondt & Thaler 1985
#'
#' @inheritParams sort_portfolios
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom purrr reduce
#' @importFrom dplyr inner_join
#' @importFrom dplyr select
#'
#' @export
#'
compute_acar <- function(factor_data, forward_returns, k, ntiles = 5) {

    market_returns <- masi %>%
        PerformanceAnalytics::Return.calculate() %>%
        tail(n = -1) %>%
        `names<-`("rm")

    ranks_by_month <- zoo::index(factor_data) %>%
        purrr::set_names() %>%
        purrr::map(
            function(fm) {
                factor_subset <- factor_data[fm] %>%
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

    low_cars <- list()
    high_cars <- list()

    for (end_month in names(ranks_by_month)) {

        rankings        <- ranks_by_month[[end_month]]
        holding_start   <- (zoo::as.yearmon(end_month) + (0/12)) %>% format("%Y%m")
        holding_end     <- (zoo::as.yearmon(end_month) + ((k - 1)/12)) %>% format("%Y%m")
        holding_period  <- paste0(holding_start, "/", holding_end)

        low_stocks <- rankings %>%
            dplyr::filter(.data$rank == "Q1") %>%
            .$stock
        high_stocks <- rankings %>%
            dplyr::filter(.data$rank == paste0("Q", ntiles)) %>%
            .$stock

        low_cars[[end_month]] <- (forward_returns[holding_period, low_stocks] -
                                      as.numeric(market_returns[holding_period])) %>%
            rowMeans(na.rm = TRUE) %>%
            cumsum() %>%
            tibble::as_tibble() %>%
            dplyr::mutate(id = dplyr::row_number()) %>%
            dplyr::rename(!!end_month := .data$value)

        high_cars[[end_month]] <- (forward_returns[holding_period, high_stocks] -
                                       as.numeric(market_returns[holding_period])) %>%
            rowMeans(na.rm = TRUE) %>%
            cumsum() %>%
            tibble::as_tibble() %>%
            dplyr::mutate(id = dplyr::row_number()) %>%
            dplyr::rename(!!end_month := .data$value)

    }

    acars_low <- low_cars %>%
        purrr::reduce(dplyr::inner_join, by = "id")

    acars_high <- high_cars %>%
        purrr::reduce(dplyr::inner_join, by = "id")

    list(
        acars_low = acars_low,
        acars_high = acars_high
    )

}
