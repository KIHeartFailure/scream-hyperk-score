pop <- read.fst("data/fst-data/pop.fst")
metaout <- read.fst("data/fst-data/metaout.fst")

ov <- read.fst("data/kosmos-data/ov.fst")
ov <- inner_join(ov, pop %>% select(lopnr), by = "lopnr")

sv <- read.fst("data/kosmos-data/sv.fst")
sv <- inner_join(sv, pop %>% select(lopnr), by = "lopnr")

kon <- read.fst("data/kosmos-data/kon.fst")
kon <- inner_join(kon, pop %>% select(lopnr), by = "lopnr")

patreg <- bind_rows(
  kon %>% mutate(datasource = "kon"),
  sv %>% mutate(datasource = "sv"),
  ov %>% mutate(datasource = "ov")
)
rm(ov)
rm(sv)
rm(kon)
gc()


pop <- create_sosvar(
  sosdata = patreg %>% filter(diag_no == "diag1"),
  cohortdata = pop,
  patid = lopnr,
  indexdate = indexdtm,
  add_unique = type,
  sosdate = bdtm,
  diavar = diagnosis,
  type = "com",
  name = "hf",
  diakod = "^(I110|I130|I132|I255|I420|I423|I42[5-9]|I43|I50|J81|K761|R570)",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = TRUE,
  meta_reg = "NPR (in-patient, spec out-patient, primary care)"
)
pop <- create_sosvar(
  sosdata = patreg %>% filter(diag_no == "diag1"),
  cohortdata = pop,
  patid = lopnr,
  indexdate = indexdtm,
  add_unique = type,
  sosdate = bdtm,
  diavar = diagnosis,
  type = "com",
  name = "hypertension",
  diakod = "^(I1[0-5])",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = TRUE,
  meta_reg = "NPR (in-patient, spec out-patient, primary care)"
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = lopnr,
  indexdate = indexdtm,
  add_unique = type,
  sosdate = bdtm,
  diavar = diagnosis,
  type = "com",
  name = "mi",
  diakod = "^(I21|I22|I252)",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = TRUE,
  meta_reg = "NPR (in-patient, spec out-patient, primary care)"
)

pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = lopnr,
  indexdate = indexdtm,
  add_unique = type,
  sosdate = bdtm,
  diavar = diagnosis,
  type = "com",
  name = "diabetes",
  diakod = "^(E1[0-4])",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = TRUE,
  meta_reg = "NPR (in-patient, spec out-patient, primary care)"
)

pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = lopnr,
  indexdate = indexdtm,
  add_unique = type,
  sosdate = bdtm,
  diavar = diagnosis,
  type = "com",
  name = "af",
  diakod = "^I48",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = TRUE,
  meta_reg = "NPR (in-patient, spec out-patient, primary care)"
)

pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = lopnr,
  indexdate = indexdtm,
  add_unique = type,
  sosdate = bdtm,
  diavar = diagnosis,
  type = "com",
  name = "pvd",
  diakod = "^(I7[0-3])",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = TRUE,
  meta_reg = "NPR (in-patient, spec out-patient, primary care)"
)

pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = lopnr,
  indexdate = indexdtm,
  add_unique = type,
  sosdate = bdtm,
  diavar = diagnosis,
  type = "com",
  name = "stroketia",
  diakod = "^(I6[0-4]|I69[0-4]|G45)",
  stoptime = -10 * 365.25,
  valsclass = "num",
  warnings = TRUE,
  meta_reg = "NPR (in-patient, spec out-patient, primary care)"
)

pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = lopnr,
  indexdate = indexdtm,
  add_unique = type,
  sosdate = bdtm,
  diavar = diagnosis,
  type = "com",
  name = "pulmonary",
  diakod = "^J",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = TRUE,
  meta_reg = "NPR (in-patient, spec out-patient, primary care)"
)

pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = lopnr,
  indexdate = indexdtm,
  add_unique = type,
  sosdate = bdtm,
  diavar = diagnosis,
  type = "com",
  name = "malignancy",
  diakod = "^C",
  stoptime = -3 * 365.25,
  valsclass = "num",
  warnings = TRUE,
  meta_reg = "NPR (in-patient, spec out-patient, primary care)"
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = lopnr,
  indexdate = indexdtm,
  add_unique = type,
  sosdate = bdtm,
  diavar = diagnosis,
  type = "com",
  name = "anemiadiag",
  diakod = "^(D5|D6[0-4])",
  stoptime = -365.25,
  valsclass = "num",
  warnings = TRUE,
  meta_reg = "NPR (in-patient, spec out-patient, primary care)"
)
pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = lopnr,
  indexdate = indexdtm,
  add_unique = type,
  sosdate = bdtm,
  diavar = diagnosis,
  type = "com",
  name = "musculoskeletal",
  diakod = "^M",
  stoptime = -3 * 365.25,
  valsclass = "num",
  warnings = TRUE,
  meta_reg = "NPR (in-patient, spec out-patient, primary care)"
)

pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = lopnr,
  indexdate = indexdtm,
  add_unique = type,
  sosdate = bdtm,
  diavar = diagnosis,
  type = "com",
  name = "liver",
  diakod = "^(B18|I85|I864|I982|K70|K710|K711|K71[3-7]|K7[2-4]|K760|K76[2-9])",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = TRUE,
  meta_reg = "NPR (in-patient, spec out-patient, primary care)"
)

pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = lopnr,
  indexdate = indexdtm,
  add_unique = type,
  sosdate = bdtm,
  diavar = diagnosis,
  type = "com",
  name = "dementia",
  diakod = "^(F0[0-3]|F051|G30|G311|G319)",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = TRUE,
  meta_reg = "NPR (in-patient, spec out-patient, primary care)"
)

pop <- create_sosvar(
  sosdata = patreg,
  cohortdata = pop,
  patid = lopnr,
  indexdate = indexdtm,
  add_unique = type,
  sosdate = bdtm,
  diavar = diagnosis,
  type = "com",
  name = "depression",
  diakod = "^(F3[2-4])",
  stoptime = -5 * 365.25,
  valsclass = "num",
  warnings = TRUE,
  meta_reg = "NPR (in-patient, spec out-patient, primary care)"
)

write.fst(pop, "data/fst-data/pop.fst")
write.fst(metaout, "data/fst-data/metaout.fst")
