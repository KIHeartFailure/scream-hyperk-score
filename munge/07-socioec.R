pop <- read.fst("data/fst-data/pop.fst")
socioec <- read.fst("data/kosmos-data/socioec.fst")

socioec <- inner_join(socioec, pop %>% select(lopnr), by = "lopnr")

pop <- left_join(
  pop %>%
    mutate(
      scbyear = year(indexdtm) - 1,
      scbyear = if_else(scbyear > 2018, 2018, scbyear)
    ), # fix since no socioec for 2018-2021 (and then -1)
  socioec,
  by = c("lopnr", "scbyear" = "year")
) %>%
  select(-scbyear)

write.fst(pop, "data/fst-data/pop.fst")
