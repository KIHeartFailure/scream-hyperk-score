# default is to use tidyverse functions
select <- dplyr::select
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete
fixed <- stringr::fixed

# used for calculation of ci
global_z05 <- qnorm(1 - 0.025)

global_cols <- RColorBrewer::brewer.pal(7, "Dark2")

global_start <- ymd("2009-01-01")
global_end <- ymd("2025-12-31")

global_fu <- 365
