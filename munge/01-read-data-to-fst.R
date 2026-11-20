source(here::here("setup/setup.R"))

# Read data from KOSMOS and save as fst files -----------------------------

dbhandle <- odbcConnect("kosmos")

tabs <- sqlTables(dbhandle, catalog = "SCREAM2", schema = "SCREAM2_CLEAN")

demo <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.scream2_demo")
demo <- demo %>%
  mutate(dob = ymd(dob)) %>%
  select(lopnr, female, dob) %>%
  group_by(lopnr) %>%
  arrange(dob, female) %>%
  slice(1) %>%
  ungroup()
# anyDuplicated(demo$lopnr)
write.fst(demo, "data/kosmos-data/demo.fst")

flytt <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.scream2_flytt")

flytt <- flytt %>%
  mutate(hdtm = ymd(hdat)) %>%
  select(lopnr, hdtm, hkod)
write.fst(flytt, "data/kosmos-data/flytt.fst")

socioec <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.socioecon")
# koll <- socioec %>%
#  group_by(lopnr, year) %>%
#  slice(2) %>%
#  ungroup()
write.fst(socioec, "data/kosmos-data/socioec.fst")

pininfo <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.reused_cases_pnr_2025")
# anyDuplicated(pininfo$lopnr)
pininfo <- pininfo %>%
  group_by(lopnr) %>%
  arrange(desc(ReturnPNr), desc(latest_check_pnr)) %>%
  slice(1) %>%
  ungroup()
write.fst(pininfo, "data/kosmos-data/pininfo.fst")

death <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.scream2_death")
# anyDuplicated(death$lopnr)

# check <- left_join(death %>% filter(diag_no == "ULORSAK") %>% group_by(lopnr) %>% slice(2) %>% ungroup(),
#                   death %>% filter(diag_no == "ULORSAK"),
#                   by = "lopnr")

death <- death %>%
  filter(diag_no == "ULORSAK") %>%
  mutate(
    deathdtm = ymd(dodsdat),
    deathyear = year(deathdtm)
  ) %>%
  select(lopnr, deathdtm, diagnos, deathyear) %>%
  distinct(lopnr, .keep_all = TRUE)
write.fst(death, "data/kosmos-data/death.fst")

snr <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.snr_rrt")

snr <- snr %>%
  filter(rrt == 1 | is.na(rrt)) %>%
  mutate(ckddtm = ymd(rrt_date)) %>%
  group_by(lopnr) %>%
  arrange(ckddtm) %>%
  slice(1) %>%
  ungroup() %>%
  select(lopnr, ckddtm, event_type)
write.fst(snr, "data/kosmos-data/snr.fst")


## Diagnosis

sv <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.slv_diagnoses_long")
sv <- sv %>%
  mutate(
    bdtm = ymd(bdat),
    utdtm = ymd(utdat_c),
    byear = year(bdtm)
  ) %>%
  select(lopnr, bdtm, byear, utdtm, diag_no, diagnosis)
write.fst(sv, "data/kosmos-data/sv.fst")

ov <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.ovr_diagnoses_long")
ov <- ov %>%
  mutate(
    bdtm = ymd(bdat),
    byear = year(bdtm)
  ) %>%
  select(lopnr, bdtm, byear, diag_no, diagnosis)
write.fst(ov, "data/kosmos-data/ov.fst")

kon <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.kon_diagnoses_long")
kon <- kon %>%
  rename(lopnr = LOPNR) %>%
  mutate(
    bdtm = ymd(bdat),
    byear = year(bdtm)
  ) %>%
  select(lopnr, bdtm, byear, diag_no, diagnosis)
write.fst(kon, "data/kosmos-data/kon.fst")

# procedure

proc_sv <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.slv_procedurecodes where opk LIKE 'DR%'")
proc_sv <- proc_sv %>%
  mutate(opdtm = ymd(datum_c)) %>%
  select(-kombika, -datum_c)

write.fst(proc_sv, "data/kosmos-data/proc_sv_dr.fst")

proc_ov <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.ovr_procedurecodes where opk LIKE 'DR%'")
proc_ov <- proc_ov %>%
  mutate(opdtm = ymd(datum)) %>%
  select(-kombika, -datum_c, -datum)

write.fst(proc_ov, "data/kosmos-data/proc_ov_dr.fst")

# lm

lm_a1 <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.v_x_lmed_all
               where atc LIKE 'A1%'")

lm_b01a <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.v_x_lmed_all
               where atc LIKE 'B01A%'")

lm_b03a <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.v_x_lmed_all
               where atc LIKE 'B03A%'")

lm_b03xa <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.v_x_lmed_all
               where atc LIKE 'B03XA%'")

lm_c <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.v_x_lmed_all
               where atc LIKE 'C%'")

lm_R03A <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.v_x_lmed_all
               where atc LIKE 'R03A%'")

lm_M01A <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.v_x_lmed_all
               where atc LIKE 'M01A%'")

lm_H02AB <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.v_x_lmed_all
               where atc LIKE 'H02AB%'")

lm_L04AD <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.v_x_lmed_all
               where atc LIKE 'L04AD%'")

lm_V03AE <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.v_x_lmed_all
               where atc LIKE 'V03AE%'")

lm <- bind_rows(lm_a1, lm_b01a, lm_b03a, lm_b03xa, lm_c, lm_R03A, lm_M01A, lm_H02AB, lm_L04AD, lm_V03AE)
rm(list = ls()[str_detect(ls(), "^lm_")])
gc()
lm <- lm %>%
  mutate(edatum = ymd(EDATUM)) %>%
  rename(
    lopnr = LopNr,
    atc = ATC
  ) %>%
  select(lopnr, edatum, ar, atc)

write.fst(lm, "data/kosmos-data/lm.fst")

# laboratory

# potassium
potas <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.Potassium_2006_2025")

# units are in mmol/L
potas <- potas %>%
  mutate(
    labdtm = ymd(datum),
    labyear = year(labdtm)
  ) %>%
  filter(labyear >= 2006 & labyear <= 2025 & Analys %in% c("P-KALIUM", "S-KALIUM") & result <= 10) %>%
  select(lopnr, labdtm, Analys, IP, OP, result, labyear, enhet)
write.fst(potas, "data/kosmos-data/potas.fst")
rm(potas)

# eGFR
demo <- read.fst("data/kosmos-data/demo.fst")
crea <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.Creatinine_2006_2025")
crea <- crea %>%
  mutate(
    labdtm = ymd(datum),
    labyear = year(labdtm)
  ) %>%
  filter(labyear >= 2006 & labyear <= 2025 & result >= 25 & result <= 1500 & IP == 0 &
    !Analys %in% c("aB-Krea ABL/PNA", "aB-Krea iSTAT/PNA", "kB-Krea ABL/PNA", "vB-Krea ABL/PNA", "vB-Krea iSTAT/PNA")) %>%
  group_by(lopnr, labdtm, labyear) %>%
  summarize(result = mean(result)) %>%
  ungroup()

egfr <- left_join(
  crea,
  demo,
  by = "lopnr"
) %>%
  mutate(
    labyear = year(labdtm),
    sex = if_else(female == 1, 0, 1),
    race = 0,
    age = floor(as.numeric(labdtm - dob) / 365.25),
    # units are in µmol/L
    gfr = nephro::CKDEpi.creat(creatinine = result / 88.4, sex = sex, age = age, ethnicity = race)
  ) %>%
  select(lopnr, labdtm, labyear, gfr)
write.fst(egfr, "data/kosmos-data/egfr.fst")

# all units g/L
hb <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.Hemoglobin_2006_2025")
hb <- hb %>%
  mutate(
    labdtm = ymd(datum),
    labyear = year(labdtm)
  ) %>%
  filter(labyear >= 2006 & labyear <= 2025 & IP == 0) %>%
  group_by(lopnr, labdtm, labyear) %>%
  summarize(result = mean(result)) %>%
  ungroup() %>%
  mutate(labyear = year(labdtm))
write.fst(hb, "data/kosmos-data/hb.fst")

sodium <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.Sodium_2006_2025")
# all units mmol/L
sodium <- sodium %>%
  mutate(
    labdtm = ymd(datum),
    labyear = year(labdtm)
  ) %>%
  filter(labyear >= 2006 & labyear <= 2025 & IP == 0) %>%
  group_by(lopnr, labdtm, labyear) %>%
  summarize(result = mean(result)) %>%
  ungroup() %>%
  mutate(labyear = year(labdtm))
write.fst(sodium, "data/kosmos-data/sodium.fst")
