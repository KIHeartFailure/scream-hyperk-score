# Merged cohort 2009-2021 -----------------------------------------------------

demo <- read.fst("data/kosmos-data/demo.fst")
pininfo <- read.fst("data/kosmos-data/pininfo.fst")
death <- read.fst("data/kosmos-data/death.fst")
flytt <- read.fst("data/kosmos-data/flytt.fst")

pop <- left_join(demo, death, by = "lopnr") %>%
  rename(deathcause = diagnos)
flow <- tibble(
  Criteria = "Patients in file demo (population)",
  N = nrow(pop)
)
dim(pop)
# [1]  3321340       6


# ej återanvänt personnummer
pop <- left_join(pop, pininfo, by = "lopnr") %>%
  filter(ReturnPNr == 0 & latest_check_pnr == 1) %>%
  select(-latest_check_pnr, -ReturnPNr, -ErrorLatePNr, -SameOrderNr)
flow <- add_row(flow,
  Criteria = "Not re-used or changed PINs",
  N = nrow(pop)
)

dim(pop)

pop <- pop %>%
  filter(is.na(deathyear) | deathyear >= year(global_start)) %>%
  mutate(censdtm = pmin(global_end, deathdtm, na.rm = T))
flow <- add_row(flow,
  Criteria = paste0("Alive >= ", year(global_start)),
  N = nrow(pop)
)

dim(pop)
# [1]  3042342        6

# hyperK NOT unique individuals from here on
potasuse <- read.fst("data/fst-data/potasuse.fst")

pop <- inner_join(pop,
  potasuse %>%
    rename(
      lab_potas = result,
      indexdtm = labdtm
    ) %>%
    select(lopnr, indexdtm, lab_potas),
  by = "lopnr"
) %>%
  filter(indexdtm >= global_start & indexdtm <= global_end)

flow <- add_row(flow,
  Criteria = ">= 1 potassium > 5.5",
  N = nrow(pop %>% distinct(lopnr))
)

# CV population -----------------------------------------------------------

sv <- read.fst("data/kosmos-data/sv.fst")
ov <- read.fst("data/kosmos-data/ov.fst")
kon <- read.fst("data/kosmos-data/kon.fst")

svcv <- sv %>%
  mutate(cvtype = case_when(
    str_detect(diagnosis, "^(I110|I130|I132|I255|I420|I423|I42[5-9]|I43|I50|J81|K761|R570)") ~ "hf",
    str_detect(diagnosis, "^(I1[0-5])") ~ "hypertension",
    str_detect(diagnosis, "^(I21|I22|I252)") ~ "mi"
  )) %>%
  filter(cvtype %in% c("hf", "hypertension") & diag_no == "diag1" | cvtype == "mi")
ov <- ov %>%
  mutate(cvtype = case_when(
    str_detect(diagnosis, "^(I110|I130|I132|I255|I420|I423|I42[5-9]|I43|I50|J81|K761|R570)") ~ "hf",
    str_detect(diagnosis, "^(I1[0-5])") ~ "hypertension",
    str_detect(diagnosis, "^(I21|I22|I252)") ~ "mi"
  )) %>%
  filter(cvtype %in% c("hf", "hypertension") & diag_no == "diag1" | cvtype == "mi")
kon <- kon %>%
  mutate(cvtype = case_when(
    str_detect(diagnosis, "^(I110|I130|I132|I255|I420|I423|I42[5-9]|I43|I50|J81|K761|R570)") ~ "hf",
    str_detect(diagnosis, "^(I1[0-5])") ~ "hypertension",
    str_detect(diagnosis, "^(I21|I22|I252)") ~ "mi"
  )) %>%
  filter(cvtype %in% c("hf", "hypertension") & diag_no == "diag1" | cvtype == "mi")
cv <- bind_rows(svcv, ov, kon)

cv <- cv %>%
  filter(bdtm <= global_end &
    ((cvtype %in% c("mi") & bdtm >= (global_start - 365.25 * 10)) | (cvtype %in% c("hf", "hypertension") & (bdtm >= global_start - 365.25 * 5)))) %>%
  rename(type = cvtype) %>%
  select(lopnr, bdtm, type)

rm(ov)
rm(svcv)
rm(kon)

# CKD population ----------------------------------------------------------

snr <- read.fst("data/kosmos-data/snr.fst")
egfr <- read.fst("data/kosmos-data/egfr.fst")

snr <- snr %>%
  filter(ckddtm <= global_end)

# egfr 2006-2021 already selected
ckdlab <- egfr %>%
  filter(labdtm >= (global_start - 1.5 * 365.25) & labdtm <= global_end & gfr < 60)

# all pop
popdiag <- bind_rows(
  snr %>% rename(popdtm = ckddtm) %>% mutate(type = "snr"),
  ckdlab %>% rename(popdtm = labdtm) %>% mutate(type = "gfr"),
  cv %>% rename(popdtm = bdtm)
) %>%
  select(lopnr, popdtm, type)

popdiag2 <- inner_join(pop %>% select(lopnr, indexdtm),
  popdiag,
  by = "lopnr"
) %>%
  filter(popdtm <= indexdtm &
    ((type %in% c("mi") & popdtm >= indexdtm - 365.25 * 10) |
      (type %in% c("hf", "hypertension") & popdtm >= indexdtm - 365.25 * 5) |
      (type %in% c("gfr") & popdtm >= indexdtm - 365.25 * 1.5) |
      type %in% c("snr"))) %>%
  group_by(lopnr, indexdtm, type) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(lopnr, indexdtm) %>%
  summarise(diagtype = paste(type, collapse = ", ")) %>%
  ungroup()

pop <- inner_join(pop, popdiag2, by = c("lopnr", "indexdtm"))

flow <- add_row(flow,
  Criteria = "With HF, MI, Hypertension or CKD",
  N = nrow(pop %>% distinct(lopnr))
)

dim(pop)

# bor i stockholm någon gång under tidsperioden
flyttuse <- inner_join(pop, flytt, by = "lopnr")
flyttlast <- flyttuse %>%
  group_by(lopnr, indexdtm) %>%
  arrange(hdtm) %>%
  slice(n()) %>%
  ungroup() %>%
  filter(hkod == "U" & hdtm <= indexdtm) %>%
  select(lopnr, indexdtm)

# need to be living in stockholm sometime after inclusion
# should not have emigrated last time prior to inclusion
pop <- anti_join(pop,
  flyttlast,
  by = c("lopnr", "indexdtm")
)

flow <- add_row(flow,
  Criteria = "Last emigration from Stockholm not before hyperK",
  N = nrow(pop %>% distinct(lopnr))
)

# select only in/out moves relevant for the study period
flyttuse2 <- flyttuse %>%
  filter((hdtm >= indexdtm & hkod == "U" | hdtm >= (indexdtm - 365.25 * 5) & hkod == "I") &
    hdtm <= censdtm) %>%
  group_by(lopnr, indexdtm, hkod) %>%
  arrange(hdtm) %>%
  slice(1) %>%
  ungroup()

pop <- left_join(pop,
  flyttuse2 %>% filter(hkod == "I") %>% rename(flyttindtm = hdtm) %>% select(lopnr, indexdtm, flyttindtm),
  by = c("lopnr", "indexdtm")
)

pop <- left_join(pop,
  flyttuse2 %>% filter(hkod == "U") %>% rename(flyttutdtm = hdtm) %>% select(lopnr, indexdtm, flyttutdtm),
  by = c("lopnr", "indexdtm")
) %>%
  mutate(
    tmpflyttindtm = if_else(!is.na(flyttutdtm) & flyttutdtm <= flyttindtm, NA_Date_, flyttindtm),
    censdtm = pmin(flyttutdtm, censdtm, na.rm = T),
    # koll = if_else(indexdtm <= censdtm & (indexdtm >= tmpflyttindtm + 5 * 365.25 | is.na(tmpflyttindtm)), "Yes", "No")
  ) %>%
  filter(indexdtm <= censdtm & (indexdtm >= tmpflyttindtm + 5 * 365.25 | is.na(tmpflyttindtm)))
#
# koll <- pop %>%
#   group_by(lopnr, koll) %>%
#   slice(1) %>%
#   ungroup() %>%
#   group_by(lopnr) %>%
#   slice(2) %>%
#   ungroup()
#
# koll2 <- inner_join(koll %>% select(lopnr), popkoll, by = "lopnr") %>% arrange(lopnr, indexdtm)

flow <- add_row(flow,
  Criteria = "Living in Stockholm >= 5 years (for comorb accural) prior to hyperK",
  N = nrow(pop %>% distinct(lopnr))
)

# Exclusion criteria
sv <- inner_join(sv, pop %>% select(lopnr), by = "lopnr") %>%
  filter(diag_no == "diag1")

proc_sv_dr <- read.fst("data/kosmos-data/proc_sv_dr.fst")
proc_ov_dr <- read.fst("data/kosmos-data/proc_ov_dr.fst")
proc <- bind_rows(proc_sv_dr, proc_ov_dr)

exfunc <- function(var, name, icd) {
  pop <<- create_sosvar(
    sosdata = sv,
    cohortdata = pop,
    patid = lopnr,
    indexdate = indexdtm,
    sosdate = bdtm,
    diavar = diagnosis,
    type = "com",
    name = var,
    diakod = icd,
    stoptime = -90,
    valsclass = "fac",
    warnings = TRUE,
    meta_reg = "NPR (in-patient)",
  )

  pop <<- pop %>%
    filter(!!sym(paste0("sos_com_", var)) == "No")

  flow <<- add_row(flow,
    Criteria = name,
    N = nrow(pop %>% distinct(lopnr))
  )
}

exfunc(var = "ex_acutekidney", name = "No acute kidney injury", icd = "^N17")
exfunc(var = "ex_redfluid", name = "No reduced fluid volume", icd = "^E86")
exfunc(var = "ex_acidosis", name = "No acidosis", icd = "^E872")
exfunc(var = "ex_tumorlysis", name = "No tumor lysis syndrome", icd = "^E883")
exfunc(var = "ex_constipation", name = "No constipation", icd = "^K590")
exfunc(var = "ex_injuries", name = "No injuries involving multiple body regions", icd = "^(T0[0-7])")
exfunc(var = "ex_rhabdomyolysis", name = "No rhabdomyolysis", icd = "^T796")
pop <- create_sosvar(
  sosdata = proc,
  cohortdata = pop,
  patid = lopnr,
  indexdate = indexdtm,
  sosdate = opdtm,
  diavar = opk,
  type = "com",
  name = "ex_transfusion",
  diakod = "^(DR029|DR03|DR040)",
  stoptime = -90,
  valsclass = "fac",
  warnings = TRUE,
  meta_reg = "NPR (in-patient, spec out-patient)",
)

pop <- pop %>%
  filter(sos_com_ex_transfusion == "No")

flow <- add_row(flow,
  Criteria = "No transfusion",
  N = nrow(pop %>% distinct(lopnr))
)


# check dist of no of hyperk
plotcheck <- pop %>%
  group_by(lopnr) %>%
  count() %>%
  ungroup() %>%
  count(n)

p1 <- ggplot(plotcheck, aes(x = n, y = nn)) +
  geom_bar(stat = "identity", col = "#8A9A5B", fill = "#8A9A5B") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20, face = "bold"),
    axis.text = element_text(color = "black"),
    plot.background = element_rect(fill = "#F0F0F0", colour = "#F0F0F0"),
    panel.background = element_rect(fill = "#F0F0F0", colour = "#F0F0F0")
  ) +
  labs(y = "N patients", x = "N hyperK")

p2 <- ggplot(plotcheck %>% filter(n <= 10), aes(x = n, y = nn)) +
  geom_bar(stat = "identity", col = "#8A9A5B", fill = "#8A9A5B") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20, face = "bold"),
    axis.text = element_text(color = "black"),
    plot.background = element_rect(fill = "#F0F0F0", colour = "#F0F0F0"),
    panel.background = element_rect(fill = "#F0F0F0", colour = "#F0F0F0")
  ) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  labs(y = "N patients", x = "N hyperK (truncated at 10)")

ggsave("./reports/figs/check_number_hyperk.pdf", p1)
ggsave("./reports/figs/check_number_hyperk2.pdf", p2)

plotcheck2 <- pop %>%
  group_by(lopnr) %>%
  arrange(indexdtm) %>%
  mutate(diff = as.numeric(indexdtm - lag(indexdtm))) %>%
  ungroup() %>%
  arrange(lopnr, indexdtm) %>%
  filter(diff < 365) %>%
  group_by(lopnr) %>%
  count() %>%
  ungroup() %>%
  count(n)

p3 <- ggplot(plotcheck2, aes(x = n, y = nn)) +
  geom_bar(stat = "identity", col = "#8A9A5B", fill = "#8A9A5B") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20, face = "bold"),
    axis.text = element_text(color = "black"),
    plot.background = element_rect(fill = "#F0F0F0", colour = "#F0F0F0"),
    panel.background = element_rect(fill = "#F0F0F0", colour = "#F0F0F0")
  ) +
  labs(y = "N patients", x = "N recurrent hyperK")

p4 <- ggplot(plotcheck2 %>% filter(n <= 10), aes(x = n, y = nn)) +
  geom_bar(stat = "identity", col = "#8A9A5B", fill = "#8A9A5B") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 20, face = "bold"),
    axis.text = element_text(color = "black"),
    plot.background = element_rect(fill = "#F0F0F0", colour = "#F0F0F0"),
    panel.background = element_rect(fill = "#F0F0F0", colour = "#F0F0F0")
  ) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  labs(y = "N patients", x = "N recurrent hyperK (truncated at 10)")

ggsave("./reports/figs/check_number_hyperk_rec.pdf", p3)
ggsave("./reports/figs/check_number_hyperk_rec2.pdf", p4)

pop1 <- pop %>%
  group_by(lopnr) %>%
  arrange(indexdtm) %>%
  slice(1) %>%
  ungroup()

set.seed(378425)
pop2 <- pop %>%
  group_by(lopnr) %>%
  arrange(indexdtm) %>%
  slice_sample(n = 1) %>%
  ungroup()


pop12 <- left_join(pop1 %>% select(lopnr, indexdtm),
  pop,
  by = "lopnr"
) %>%
  filter(indexdtm.y > indexdtm.x & indexdtm.y <= indexdtm.x + 365) %>%
  group_by(lopnr) %>%
  arrange(indexdtm.y) %>%
  slice(1) %>%
  ungroup() %>%
  count()


pop22 <- left_join(pop2 %>% select(lopnr, indexdtm),
  pop,
  by = "lopnr"
) %>%
  filter(indexdtm.y > indexdtm.x & indexdtm.y <= indexdtm.x + 365) %>%
  group_by(lopnr) %>%
  arrange(indexdtm.y) %>%
  slice(1) %>%
  ungroup() %>%
  count()

ncheck <- tibble(
  Selection = c("Selection process first", "Selection process random"),
  N = c(pop12$n, pop22$n)
)

# choose random selection

set.seed(378425)
pop <- pop %>%
  group_by(lopnr) %>%
  arrange(indexdtm) %>%
  slice_sample(n = 1) %>%
  ungroup()

pop <- pop %>%
  select(-tmpflyttindtm, -flyttindtm, -flyttutdtm, -starts_with("sos_com_ex_"))

write.fst(ncheck, "data/fst-data/ncheck.fst")
write.fst(pop, "data/fst-data/pop.fst")
write.fst(flow, "data/fst-data/flow.fst")
write.fst(metaout, "data/fst-data/metaout.fst")
