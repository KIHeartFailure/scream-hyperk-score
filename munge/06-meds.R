pop <- read.fst("data/fst-data/pop.fst")
lm <- read.fst("data/kosmos-data/lm.fst")

lm <- inner_join(lm, pop %>% select(lopnr), by = "lopnr") %>%
  rename(ATC = atc)

lmprior <- inner_join(lm,
  pop %>% select(lopnr, indexdtm),
  by = "lopnr",
  relationship = "many-to-many"
) %>%
  filter(edatum <= indexdtm & edatum >= indexdtm - 120) %>%
  select(-indexdtm)

lmpost <- inner_join(lm,
  pop %>% select(lopnr, indexdtm),
  by = "lopnr",
  relationship = "many-to-many"
) %>%
  filter(edatum >= indexdtm & edatum <= indexdtm + 120) %>%
  select(-indexdtm)


medfunc <- function(lmdata, lmtext, lmname) {
  pop <<- create_medvar(
    atc = "^(B03XA02|B03XA10|B03A)",
    medname = paste0("anemia_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )

  pop <<- create_medvar(
    atc = "^(C09A|C09B)",
    medname = paste0("acei_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^(C09C|C09D)",
    medname = paste0("arb_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^C09DX04",
    medname = paste0("arni_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^C03DA",
    medname = paste0("mra_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^A10BK",
    medname = paste0("sglt2i_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^A10BJ",
    medname = paste0("glp1a_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^(C03A|C03B|C09BA|C09DA|C09DX01)",
    medname = paste0("thiazide_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^C03C",
    medname = paste0("loop_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^(C08CA|C07FB02|C09BB|C09DB|C09DX01)",
    medname = paste0("ccb_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^C07",
    medname = paste0("bbl_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^R03A",
    medname = paste0("betaagonist_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^M01A",
    medname = paste0("nsaid_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^H02AB",
    medname = paste0("glucocorticoid_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^L04AD",
    medname = paste0("cyclotacro_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^A10A",
    medname = paste0("insulin_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^V03AE01",
    medname = paste0("pb_resin_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^(V03AE09|V03AE10)",
    medname = paste0("pb_novel_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^A12BA",
    medname = paste0("ps_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^(C10AA|C10BA)",
    medname = paste0("statin_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )

  pop <<- create_medvar(
    atc = "^C01DA",
    medname = paste0("nitrate_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^(B01AA|B01AF|B01AE)",
    medname = paste0("anticoagulant_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^B01AC",
    medname = paste0("antiplatelet_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^(B01AB04|B01AB05|B01AB10)",
    medname = paste0("lmwh_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
  pop <<- create_medvar(
    atc = "^C01AA",
    medname = paste0("digoxin_", lmname),
    medvar = ATC,
    meddata = lmdata,
    cohortdata = pop,
    id = "lopnr",
    indexdate = indexdtm,
    valsclass = "num",
    metatime = lmtext
  )
}

medfunc(lmdata = lmprior, lmtext = "-120-0", lmname = "prior")
medfunc(lmdata = lmpost, lmtext = "0-120", lmname = "post")

write.fst(pop, "data/fst-data/pop.fst")
write.fst(metalm, "data/fst-data/metalm.fst")
