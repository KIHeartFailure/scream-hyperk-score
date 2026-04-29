potasuse <- read.fst("data/fst-data/potasuse.fst")
pop <- read.fst("data/fst-data/pop.fst")

pop <- pop %>%
  mutate(censdtm = pmin(censdtm, indexdtm + 365))

# outcome
outcome <- left_join(pop,
  potasuse %>% select(lopnr, labdtm),
  by = "lopnr"
) %>%
  filter(labdtm > indexdtm + 7 & labdtm <= censdtm) %>%
  group_by(lopnr) %>%
  arrange(labdtm) %>%
  slice(1) %>%
  ungroup() %>%
  select(lopnr, labdtm)

outcome120 <- left_join(pop,
  potasuse %>% select(lopnr, labdtm),
  by = "lopnr"
) %>%
  filter(labdtm > indexdtm + 120 & labdtm <= censdtm) %>%
  group_by(lopnr) %>%
  arrange(labdtm) %>%
  slice(1) %>%
  ungroup() %>%
  select(lopnr, labdtm) %>%
  rename(labdtm120 = labdtm)

pop <- left_join(pop, outcome, by = "lopnr")
pop <- left_join(pop, outcome120, by = "lopnr")

pop <- pop %>%
  mutate(
    outtime_hyperk7 = coalesce(as.numeric(labdtm - indexdtm), as.numeric(censdtm - indexdtm)),
    outtime_hyperk120 = coalesce(as.numeric(labdtm120 - indexdtm), as.numeric(censdtm - indexdtm)),
    out_hyperk7 = if_else(is.na(labdtm), 0, 1),
    out_hyperk120 = if_else(is.na(labdtm120), 0, 1),
  ) %>%
  select(-labdtm, -labdtm120)

write.fst(pop, "data/fst-data/pop.fst")
