# Project specific packages, functions and settings -----------------------

source(here::here("setup/setup.R"))

# Read raw data and save as fst files -------------------------------------

# source(here("munge/01-read-data-to-fst.R")) # Run on MEB VDI ONLY!!!!!

# Munge data --------------------------------------------------------------

source(here("munge/02-fix-potas.R"))

source(here::here("setup/setup.R"))
source(here("munge/03-selection.R"))

source(here::here("setup/setup.R"))
source(here("munge/04-out-hyperk.R"))

source(here::here("setup/setup.R"))
source(here("munge/05-comorbs.R"))

source(here::here("setup/setup.R"))
source(here("munge/06-meds.R"))

source(here::here("setup/setup.R"))
source(here("munge/07-socioec.R"))

source(here::here("setup/setup.R"))
source(here("munge/08-labs.R"))

# Cache/save data ---------------------------------------------------------

flow <- read.fst("data/fst-data/flow.fst")
metaout <- read.fst("data/fst-data/metaout.fst")
metalm <- read.fst("data/fst-data/metalm.fst")

save(
  file = here("data/clean-data/hkdata.RData"),
  list = c(
    "flow",
    "metalm",
    "metaout",
    "pop"
  )
)

write_dta(pop,
  path = paste0("./data/clean-data/hkdata_", Sys.Date(), ".dta"),
  version = 14
)
