# fix only potas first day of hosp and outpatient

potas <- read.fst("data/kosmos-data/potas.fst")
sv <- read.fst("data/kosmos-data/sv.fst")

potas2 <- potas %>%
  filter(result > 5.5) %>%
  group_by(lopnr, labdtm) %>%
  arrange(desc(result)) %>%
  slice(1) %>%
  ungroup()

sv2 <- sv %>%
  group_by(lopnr, bdtm, utdtm) %>%
  slice(1) %>%
  ungroup()

# koll <- sv2 %>%
#  filter(utdtm < bdtm) 0 posts

potassv <- inner_join(
  potas2,
  sv2,
  by = "lopnr"
) %>%
  filter(labdtm > bdtm & labdtm <= utdtm) %>%
  group_by(lopnr, labdtm) %>%
  slice(1) %>%
  ungroup()

potasuse <- anti_join(potas2, potassv, by = c("lopnr", "labdtm"))

write.fst(potasuse, "data/fst-data/potasuse.fst")

rm(sv)
rm(sv2)
rm(potas)
rm(potas2)
