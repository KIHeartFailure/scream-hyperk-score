pop <- read.fst("data/fst-data/pop.fst")

hb <- read.fst("data/kosmos-data/hb.fst")
hb <- inner_join(hb, pop %>% select(lopnr, indexdtm), by = "lopnr") %>%
  filter(labdtm <= indexdtm & labdtm >= indexdtm - 365) %>%
  group_by(lopnr) %>%
  arrange(desc(labdtm)) %>%
  slice(1) %>%
  ungroup() %>%
  rename(lab_hb = result) %>%
  select(lopnr, lab_hb)
pop <- left_join(pop, hb, by = "lopnr")


sodium <- read.fst("data/kosmos-data/sodium.fst")
sodium <- inner_join(sodium, pop %>% select(lopnr, indexdtm), by = "lopnr") %>%
  filter(labdtm <= indexdtm & labdtm >= indexdtm - 365) %>%
  group_by(lopnr) %>%
  arrange(desc(labdtm)) %>%
  slice(1) %>%
  ungroup() %>%
  rename(lab_sodium = result) %>%
  select(lopnr, lab_sodium)
pop <- left_join(pop, sodium, by = "lopnr")

egfr <- read.fst("data/kosmos-data/egfr.fst")
egfr <- inner_join(egfr, pop %>% select(lopnr, indexdtm), by = "lopnr") %>%
  filter(labdtm <= indexdtm & labdtm >= indexdtm - 365 * 1.5) %>%
  group_by(lopnr) %>%
  arrange(desc(labdtm)) %>%
  slice(1) %>%
  ungroup() %>%
  rename(lab_egfr = gfr) %>%
  select(lopnr, lab_egfr)
pop <- left_join(pop, egfr, by = "lopnr")

snr <- read.fst("data/kosmos-data/snr.fst")
snrtmp <- snr %>%
  filter(event_type %in% c("PD", "HD")) # already first date selected, however, someone can be selected at transplant and then get dialys
pop <- left_join(pop,
  snrtmp %>% select(lopnr, ckddtm),
  by = "lopnr"
) %>%
  mutate(snr_dialysis = if_else(indexdtm < ckddtm | is.na(ckddtm), 0, 1)) %>%
  select(-ckddtm)

write.fst(pop, "data/fst-data/pop.fst")
