#' Chinese Development Finance Commitments by Country
#'
#' A dataset containing Chinese development finance commitments and related
#' country characteristics derived from the Global Chinese Development Finance
#' (GCDF) Dataset 3.0, World Bank World Development Indicators, and IMF World
#' Economic Outlook data. The dataset includes total commitments, weighted
#' average interest rates, and key country characteristics like GDP, population,
#' region and income group.
#'
#' @format A data frame with approximately 150 rows and 8 variables:
#' \describe{
#'   \item{country_name}{Character. Country name}
#'   \item{iso3c}{Character. ISO 3-letter country code}
#'   \item{total_commitments_bn}{Numeric. Total Chinese development finance
#'         commitments in billions of constant 2021 USD}
#'   \item{weighted_interest_rate}{Numeric. Weighted average interest rate
#'         across all loans, weighted by commitment amount}
#'   \item{region_name}{Character. World Bank geographic region}
#'   \item{income_level_name}{Character. World Bank income group classification}
#'   \item{gdp_usd_bn}{Numeric. Nominal GDP in billions of USD (2021)}
#'   \item{population_mn}{Numeric. Population in millions (2021)}
#' }
#'
#' @source
#' \describe{
#'   \item{GCDF 3.0}{Commitment and interest rate data from AidData's Global
#'         Chinese Development Finance Dataset, Version 3.0}
#'   \item{WDI}{Region and income group classifications from World Bank World
#'         Development Indicators}
#'   \item{WEO}{GDP and population data from IMF World Economic Outlook
#'         (Fall 2024)}
#' }
#'
#' @examples
#' # Get top 10 recipients by commitment amount
#' gcdf_country_commitments |>
#'   dplyr::arrange(desc(total_commitments_bn)) |>
#'   head(10)
#'
#' # Calculate average commitment size by region
#' gcdf_country_commitments |>
#'   dplyr::group_by(region_name) |>
#'   dplyr::summarize(
#'     avg_commitment_bn = mean(total_commitments_bn, na.rm = TRUE)
#'   )
#'
#' # Create basic visualization of commitments by income group
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(
#'     gcdf_country_commitments,
#'     ggplot2::aes(x = income_level_name, y = total_commitments_bn)
#'   ) +
#'     ggplot2::geom_boxplot()
#' }
"gcdf_country_commitments"

#' Chinese Development Finance Commitments by Year and Flow Class
#'
#' A time series dataset containing annual Chinese development finance
#' commitments broken down by flow class (e.g., ODA-like, OOF-like, etc.) from
#' the Global Chinese Development Finance Dataset 3.0. Values are in constant
#' 2021 USD.
#'
#' @format A data frame with approximately 100 rows and 3 variables:
#' \describe{
#'   \item{commitment_year}{Integer. Year of commitment}
#'   \item{flow_class}{Character. Classification of financial flow (ODA-like,
#'         OOF-like, or Vague Official Finance)}
#'   \item{commitments_bn}{Numeric. Total commitments in billions of constant
#'         2021 USD}
#' }
#'
#' @source
#' AidData's Global Chinese Development Finance Dataset, Version 3.0
#'
#' @examples
#' # Get total commitments by flow class
#' gcdf_yearly_flows |>
#'   dplyr::group_by(flow_class) |>
#'   dplyr::summarize(
#'     total_commitments_bn = sum(commitments_bn, na.rm = TRUE)
#'   )
#'
#' # Plot time series of commitments by flow class
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(
#'     gcdf_yearly_flows,
#'     ggplot2::aes(x = commitment_year, y = commitments_bn, color = flow_class)
#'   ) +
#'     ggplot2::geom_line()
#' }
"gcdf_yearly_flows"
