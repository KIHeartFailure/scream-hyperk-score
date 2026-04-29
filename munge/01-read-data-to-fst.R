# Read data from KOSMOS and save as fst files -----------------------------

dbhandle <- odbcConnect("kosmos")

demo <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.scream2_demo")
demo <- demo %>%
  mutate(dob = ymd(dob)) %>%
  select(lopnr, female, dob)
# anyDuplicated(demo$lopnr)
write.fst(demo, "data/kosmos-data/demo.fst")

flytt <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.scream2_flytt")

flytt <- flytt %>%
  mutate(hdtm = ymd(hdat)) %>%
  select(lopnr, hdtm, hkod)
write.fst(flytt, "data/kosmos-data/flytt.fst")

socioec <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.socioecon")
write.fst(socioec, "data/kosmos-data/socioec.fst")

pininfo <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.pininfo")
# anyDuplicated(pininfo$lopnr)
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
  filter(rrt == 1) %>%
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

proc_sv <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.slv_procedurecodes")

write.fst(proc_sv, "data/kosmos-data/proc_sv.fst")

proc_ov <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.ovr_procedurecodes")

write.fst(proc_ov, "data/kosmos-data/proc_ov.fst")

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

lm <- lm %>%
  mutate(edatum = ymd(edatum)) %>%
  select(lopnr, edatum, ar, atc)

write.fst(lm, "data/kosmos-data/lm.fst")

# laboratory

lab <- sqlQuery(dbhandle, "select * from SCREAM2.SCREAM2_CLEAN.lab_values
                where test in ('crea', 'dipstick-alb', 'potas', 'sodium',
                'uacr', 'ualb', 'regular Hb')")

lab <- lab %>%
  mutate(
    labdtm = ymd(datum),
    labyear = year(labdtm)
  ) %>%
  filter(labyear >= 2006 & labyear <= 2021) %>%
  select(lopnr, labdtm, test, Analys, vtype, result, labyear, enhet)

write.fst(lab, "data/kosmos-data/labraw.fst")

# potassium
# units are in mmol/L
potas <- lab %>%
  filter(test == "potas" & Analys %in% c("P-Kalium", "S-Kalium") & result <= 10) %>%
  select(-test)
write.fst(potas, "data/kosmos-data/potas.fst")
rm(potas)

# eGFR
demo <- read.fst("data/kosmos-data/demo.fst")

egfr <- lab %>%
  filter(test == "crea" & result >= 25 & result <= 1500 & vtype != "IP") %>%
  group_by(lopnr, labdtm, labyear) %>%
  summarize(result = mean(result)) %>%
  ungroup()

egfr <- left_join(
  egfr,
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
hb <- lab %>%
  filter(test == "regular Hb" & vtype != "IP") %>%
  group_by(lopnr, labdtm, labyear) %>%
  summarize(result = mean(result)) %>%
  ungroup() %>%
  mutate(labyear = year(labdtm))
write.fst(hb, "data/kosmos-data/hb.fst")

# all units mmol/L
sodium <- lab %>%
  filter(test == "sodium" & vtype != "IP") %>%
  group_by(lopnr, labdtm, labyear) %>%
  summarize(result = mean(result)) %>%
  ungroup() %>%
  mutate(labyear = year(labdtm))
write.fst(sodium, "data/kosmos-data/sodium.fst")
