source("initiate.R")

f <- "input_data/hps_individual_week1_27.rds"
df.puf <- readRDS(f)

## select variables of interest

# ####################  2021-03-16  Mental health variables  ####################
#
# ## select variables of which the universe is all respondents
# vars.use <- c("anxious", "worry", "interest", "down",
#               "wrkloss", "expctloss", "curfoodsuf",
#               "tenure", "mortconf")
#
# df1 <- aggregate_state_count_nocrosstab(df.puf, vars.use)
#
# ## eye inspect the data before doing the following
# ## df1; summary(df1$data)
#
# df1.use <- df1$data
#
# ## compute the rate of which the universe is all respondents
# df1.total <- df1.use %>%
#     group_by(week, est_st, vars) %>%
#     summarise(total = sum(count)) %>%
#     ungroup()
#
# df1.use <- df1.use %>%
#     left_join(df1.total, by = c("week", "est_st", "vars")) %>%
#     mutate(rate = count / total)
#
# ## aggregate value
# agg.scheme <- bind_rows(
#     tibble(var = "anxious",    val = list(c(3, 4))), # newvar = "hps.anxious"),
#     tibble(var = "worry",      val = list(c(3, 4))), # newvar = "hps.worry"),
#     tibble(var = "interest",   val = list(c(3, 4))), # newvar = "hps.interest"),
#     tibble(var = "down",       val = list(c(3, 4))), # newvar = "hps.down"),
#     tibble(var = "wrkloss",    val = list(c(1))), # newvar = "hps.wrkloss"),
#     tibble(var = "expctloss",  val = list(c(1))), # newvar = "hps.expctloss"),
#     tibble(var = "curfoodsuf", val = list(c(3, 4))), # newvar = "hps.fdinsuf"),
#     tibble(var = "tenure",     val = list(c(2, 3))),
#     tibble(var = "mortconf",   val = list(c(1, 2)))
# )
#
# stopifnot(all(vars.use %in% agg.scheme$var))
#
# df1.use.agg <- df1.use %>%
#     group_by(week, est_st, vars) %>%
#     nest() %>%
#     left_join(agg.scheme, by = c("vars" = "var")) %>%
#     mutate(newdata = map2(
#                data, val,
#                function(data, val) {
#                    data %>%
#                        filter(value %in% val) %>%
#                        summarise(across(all_of(c("count", "rate")), ~ sum(.)))
#                })) %>%
#     select(-data, -val) %>%
#     mutate(vars = str_c(vars, ".agg")) %>%
#     unnest(newdata) %>%
#     ungroup()
#
#
# write_csv(df1.use, path = "output_data/20210318_mental_health_all.csv")
#
# write_csv(df1.use.agg, path = "output_data/20210318_mental_health_aggregate.csv")


################### Hoarding the following code for future revision #####################
flag.a <- TRUE
if (flag.a) {

    vars.meta <- c("scram", "week", "est_st", "pweight")
####################  1. Get some variable and compute the ratio ####################
    vars.use <- c(
        "prifoodsuf", "curfoodsuf", "hlthstatus", "anxious",
        "worry", "wrkloss", "expctloss", "freefood"
    )

    df.fi <- select_tidy_puf(df.puf, vars.use = vars.use)

    ## state aggregation
    df.fi.st <- compute_value_count(df.fi, grp.vars = c("week", "est_st", "vars", "value"))
    df.fi.us <- compute_value_count(df.fi, grp.vars = c("week", "vars", "value")) %>%
        mutate(est_st = "99")
    df.fi.st <- rbind(df.fi.st, df.fi.us)

    df.fi.st <- df.fi.st %>%
        group_by(week, est_st, vars) %>%
        mutate(ntot = sum(nres, na.rm = TRUE),
               rate = nres / ntot)

    items <- bind_rows(
        item1 = tibble(var = "curfoodsuf", val = list(c(3, 4)), newvar = "hps.fdinsuf"),
        item2 = tibble(var = "prifoodsuf", val = list(c(3, 4)), newvar = "hps.prefdinsuf"),
        item3 = tibble(var = "anxious",    val = list(c(3, 4)), newvar = "hps.anxious"),
        item4 = tibble(var = "hlthstatus", val = list(c(4, 5)), newvar = "hps.prehlth"),
        item5 = tibble(var = "worry",      val = list(c(3, 4)), newvar = "hps.worry"),
        item6 = tibble(var = "wrkloss",    val = list(c(1)),    newvar = "hps.wrkloss"),
        item7 = tibble(var = "expctloss",  val = list(c(1)),    newvar = "hps.expctloss"),
        item8 = tibble(var = "freefood",   val = list(c(1)),    newvar = "hps.freefood")
    )

    df.hps1 <- pmap(items,
                    function(var, val, newvar) {
                        compute_combined_rate(df.fi.st, var, val, newvar)
                    }) %>%
        reduce(left_join, by = c("week", "est_st")) %>%
        rename(state.fips = est_st)

    rm(df.fi, df.fi.us, df.fi.st)

################### 2. Get the answer for free food  #################################
    s <- "starts_with('wherefree')"
    df.fdsrc <- select_tidy_puf(df.puf, vars.selstr = s)

    df.fdsrc.st <- compute_value_count(df.fdsrc, grp.vars = c("week", "est_st", "vars", "value"))
    df.fdsrc.us <- compute_value_count(df.fdsrc, grp.vars = c("week", "vars", "value")) %>%
        mutate(est_st = "99")
    df.fdsrc.st <- rbind(df.fdsrc.st, df.fdsrc.us)

    ## compute the percentage of free food source relative to total number of people who got
    ## free food or total number of people who answered the question about free food.
    df.hps2 <- df.fdsrc.st %>%
        select(week, state.fips = est_st, vars, nres) %>%
        pivot_wider(names_from = vars, values_from = nres, names_prefix = "hps.") %>%
        left_join(
            select(df.hps1, all_of(c("week", "state.fips", "hps.freefood", "hps.freefood.rt"))),
            by = c("week", "state.fips")
        ) %>%
        mutate(across(contains("wherefree"), list(rt = ~(.x / hps.freefood)),
                      .names = "{col}.{fn}")) %>%
        mutate(across(matches("wherefree[0-9]\\.rt$"), list("2" = ~ (.x * hps.freefood.rt)),
                      .names = "{col}{fn}")) %>%
        select(-any_of(c("hps.freefood", "hps.freefood.rt")))

    rm(df.fdsrc, df.fdsrc.st, df.fdsrc.us)

####################  3. Get the FI status by income  ####################

##########  3.1 Current FI by income  ###########
    vars.use <- c("curfoodsuf", "income")
    df.fincls <- df.puf %>%
        select(all_of(c(vars.meta, vars.use))) %>%
        filter(!curfoodsuf %in% c(-88, -99),
               !income %in% c(-88, -99)) %>%
        ## combine food insufficient
        mutate(foodinsuf = ifelse(curfoodsuf %in% c(1, 2), 0, 1),
               hps.fdinsuf_income = str_c("FI_", foodinsuf, "_", "Income_", income),
               value = 1)

    df.fincls.st <- df.fincls %>%
        group_by(est_st, week, hps.fdinsuf_income) %>%
        summarise(nres = sum(pweight, na.rm = TRUE)) %>%
        ungroup()

    df.fincls.us <- df.fincls %>%
        group_by(week, hps.fdinsuf_income) %>%
        summarise(nres = sum(pweight, na.rm = TRUE)) %>%
        mutate(est_st = "99") %>%
        ungroup()

    df.fincls.st <- rbind(df.fincls.st, df.fincls.us) %>%
      group_by(est_st, week) %>%
      mutate(
        ntot = sum(nres, na.rm = TRUE),
        hps.fdinsuf_income_rate = nres / ntot
      ) %>%
      ungroup() %>%
      select(est_st, week, hps.fdinsuf_income, hps.fdinsuf_income_rate) %>%
      pivot_wider(
        id_cols = c(est_st, week),
        names_from = hps.fdinsuf_income,
        values_from = hps.fdinsuf_income_rate,
        values_fill = 0
      )

##########  3.2 Previous FI by income  ###########
    vars.use <- c("prifoodsuf", "income")
    df.prifincls <- df.puf %>%
        select(all_of(c(vars.meta, vars.use))) %>%
        filter(!prifoodsuf %in% c(-88, -99),
               !income %in% c(-88, -99)) %>%
        ## combine food insufficient
        mutate(prefoodinsuf = ifelse(prifoodsuf %in% c(1, 2), 0, 1),
               hps.prifdinsuf_income = str_c("PRFI_", prefoodinsuf, "_", "Income_", income),
               value = 1)

    df.prifincls.st <- df.prifincls %>%
        group_by(est_st, week, hps.prifdinsuf_income) %>%
        summarise(nres = sum(pweight, na.rm = TRUE)) %>%
        ungroup()

    df.prifincls.us <- df.prifincls %>%
        group_by(week, hps.prifdinsuf_income) %>%
        summarise(nres = sum(pweight, na.rm = TRUE)) %>%
        mutate(est_st = "99") %>%
        ungroup()

    df.prifincls.st <- rbind(df.prifincls.st, df.prifincls.us) %>%
        group_by(est_st, week) %>%
        mutate(ntot = sum(nres, na.rm = TRUE),
               hps.prifdinsuf_income_rate = nres / ntot) %>%
        ungroup() %>%
        select(est_st, week, hps.prifdinsuf_income, hps.prifdinsuf_income_rate) %>%
        pivot_wider(id_cols = c(est_st, week),
                    names_from = hps.prifdinsuf_income,
                    values_from = hps.prifdinsuf_income_rate,
                    values_fill = 0)


##########  3.3 Change in FI by income class ##########
    vars.use <- c("curfoodsuf", "income", "prifoodsuf")
    df.fichg <- df.puf %>%
        select(all_of(c(vars.meta, vars.use)))

    for (i in seq_along(vars.use)) {
        var <- vars.use[i]
        df.fichg <- df.fichg %>% remove_missing(var)
    }

    df.fichg <- df.fichg %>%
        mutate(fdstatchg = case_when(
                   curfoodsuf %in% c(1, 2) & prifoodsuf %in% c(1, 2) ~ "remain_good",
                   curfoodsuf %in% c(1, 2) & prifoodsuf %in% c(3, 4) ~ "get_better",
                   curfoodsuf %in% c(3, 4) & prifoodsuf %in% c(3, 4) ~ "remain_bad",
                   curfoodsuf %in% c(3, 4) & prifoodsuf %in% c(1, 2) ~ "get_worse",
                   TRUE ~ "others"),
               hps.fdstatchg_income = str_c("DFI_", fdstatchg, "_", "Income_", income))

    ## change in FI by income class
    df.fichg.st <- df.fichg %>%
        group_by(est_st, week, hps.fdstatchg_income) %>%
        summarise(nres = sum(pweight, na.rm = TRUE))

    df.fichg.us <- df.fichg %>%
        group_by(week, hps.fdstatchg_income) %>%
        summarise(nres = sum(pweight, na.rm = TRUE)) %>%
        mutate(est_st = "99")

    df.fichg.st <- rbind(df.fichg.st, df.fichg.us) %>%
        group_by(est_st, week) %>%
        mutate(ntot = sum(nres, na.rm = TRUE),
               hps.fdstatchg_income_rate = nres / ntot) %>%
        ungroup() %>%
        select(est_st, week, hps.fdstatchg_income, hps.fdstatchg_income_rate) %>%
        pivot_wider(
            id_cols = c(est_st, week),
            names_from = hps.fdstatchg_income,
            values_from = hps.fdstatchg_income_rate,
            values_fill = 0
        )

    ## change by FI without income class
    df.fichg.st2 <- df.fichg %>%
        group_by(est_st, week, fdstatchg) %>%
        summarise(nres = sum(pweight, na.rm = TRUE))

    df.fichg.us2 <- df.fichg %>%
        group_by(week, fdstatchg) %>%
        summarise(nres = sum(pweight, na.rm = TRUE)) %>%
        mutate(est_st = "99")

    df.fichg.st2 <- rbind(df.fichg.st2, df.fichg.us2) %>%
        group_by(est_st, week) %>%
        mutate(ntot = sum(nres, na.rm = TRUE),
               hps.fdstatchg_rate = nres / ntot) %>%
        ungroup() %>%
        select(est_st, week, fdstatchg, hps.fdstatchg_rate) %>%
        pivot_wider(
            id_cols = c(est_st, week),
            names_from = fdstatchg,
            values_from = hps.fdstatchg_rate,
            values_fill = 0,
            names_prefix = "DFI_"
        )

    df.hps3 <- df.fincls.st %>%
        left_join(df.prifincls.st, by = c("est_st", "week")) %>%
        left_join(df.fichg.st, by = c("est_st", "week")) %>%
        left_join(df.fichg.st2, by = c("est_st", "week")) %>%
        rename(state.fips = est_st)

##########  4. Children and school enrollment ##########

####### 4.1  children enrolled in schools  ###########
    s <- "matches('^enroll[1-3]')"
    df.enrl <- select_tidy_puf(df.puf, vars.selstr = s)
    df.enrl.st <- compute_value_count(df.enrl, grp.vars = c("week", "est_st", "vars", "value"))
    df.enrl.us <- compute_value_count(df.enrl, grp.vars = c("week", "vars", "value")) %>%
        mutate(est_st = "99")

    df.enrl.st <- rbind(df.enrl.st, df.enrl.us) %>%
        pivot_wider(names_from = vars, values_from = nres) %>%
        rowwise() %>%
        mutate(ntot = sum(c_across(starts_with("enroll")))) %>%
        ungroup() %>%
        mutate(across(starts_with("enroll"), list(rt = ~ (.x / ntot)), .names = "{col}.{fn}"))

    df.enrl.sch <- df.enrl.st %>%
        select(state.fips = est_st, week, hps.enrl = enroll1, hps.enrl.rt = enroll1.rt)

######## 4.2 HH with children under 18  ##########
    s <- "contains('thhld_numkid')"
    df.chln <- select_tidy_puf(df.puf, vars.selstr = s)
    df.chln.st <- compute_value_count(df.chln, grp.vars = c("week", "est_st", "vars", "value"))
    df.chln.us <- compute_value_count(df.chln, grp.vars = c("week", "vars", "value")) %>%
        mutate(est_st = 99)
    df.chln.st <- rbind(df.chln.st, df.chln.us)

    df.chln.tot <- df.chln.st %>%
        group_by(week, est_st, vars) %>%
        summarise(ntot = sum(nres))

    df.chln.18 <- df.chln.st %>%
        filter(value != 0) %>%
        group_by(week, est_st, vars) %>%
        summarise(chln18 = sum(nres)) %>%
        left_join(df.chln.tot, by = c("week", "est_st", "vars")) %>%
        mutate(hps.chln18.rt = chln18 / ntot) %>%
        select(state.fips = est_st, week, hps.chln18 = chln18, hps.chln18.rt)

    df.hps4 <- full_join(df.enrl.sch, df.chln.18, by = c("state.fips", "week")) %>%
        ## The rate of enrollment in too high. So adjust it to total respondents to the
        ## question of the number of children.
        mutate(hps.enrl.rt2 = hps.enrl / (hps.chln18 / hps.chln18.rt))

###########  Wrap up all  ##################

    df.hps.all <- list(
        df.hps1, df.hps2, df.hps3, df.hps4
    ) %>%
        reduce(full_join, by = c("state.fips", "week"))

    saveRDS(df.hps.all, file = "output_data/hps_fdinsuf_state.rds")

}

#########################  Trash  #################################

# ## compute the current food insufficiency rate by income class
# vars.use <- c("curfoodsuf", "income")
# df.use <- df.puf %>%
#   select(all_of(c(vars.meta, vars.use)))
#
# df.use <- df.use %>%
#   filter(!curfoodsuf %in% c(-88, -99),
#          !income %in% c(-88, -99)) %>%
#   ## combine food insufficient
#   mutate(foodinsuf = ifelse(curfoodsuf %in% c(1, 2), 0, 1),
#          hps.fdinsuf_income = str_c("FI_", foodinsuf, "_", "Income_", income))
#
# df.use.st <- df.use %>%
#   group_by(est_st, week, hps.fdinsuf_income) %>%
#   summarise(nres = sum(pweight, na.rm = TRUE)) %>%
#   mutate(ntot = sum(nres, na.rm = TRUE),
#          hps.fdinsuf_income_rate = nres / ntot) %>%
#   ungroup() %>%
#   select(est_st, week, hps.fdinsuf_income, hps.fdinsuf_income_rate) %>%
#   pivot_wider(id_cols = c(est_st, week),
#               names_from = hps.fdinsuf_income,
#               values_from = hps.fdinsuf_income_rate,
#               values_fill = 0)
#
# df.use.us <- df.use %>%
#   group_by(week, hps.fdinsuf_income) %>%
#   summarise(nres = sum(pweight, na.rm = TRUE)) %>%
#   mutate(ntot = sum(nres, na.rm = TRUE),
#          hps.fdinsuf_income_rate = nres / ntot) %>%
#   ungroup() %>%
#   select(week, hps.fdinsuf_income, hps.fdinsuf_income_rate) %>%
#   pivot_wider(id_cols = c(week),
#               names_from = hps.fdinsuf_income,
#               values_from = hps.fdinsuf_income_rate,
#               values_fill = 0) %>%
#   mutate(est_st = "99")
#
# df.use.st <- bind_rows(df.use.st, df.use.us)
#
# ## compute the previous food insufficiency rate by income class
# vars.use <- c("prifoodsuf", "income")
# df.use <- df.puf %>%
#   select(all_of(c(vars.meta, vars.use)))
#
# df.use <- df.use %>%
#   filter(!prifoodsuf %in% c(-88, -99),
#          !income %in% c(-88, -99)) %>%
#   ## combine food insufficient
#   mutate(prifoodinsuf = ifelse(prifoodsuf %in% c(1, 2), 0, 1),
#          hps.prifdinsuf_income = str_c("PRFI_", prifoodinsuf, "_", "Income_", income))
#
#
# df.use.st2 <- df.use %>%
#   group_by(est_st, week, hps.prifdinsuf_income) %>%
#   summarise(nres = sum(pweight, na.rm = TRUE)) %>%
#   mutate(ntot = sum(nres, na.rm = TRUE),
#          hps.prifdinsuf_income_rate = nres / ntot) %>%
#   ungroup() %>%
#   select(est_st, week, hps.prifdinsuf_income, hps.prifdinsuf_income_rate) %>%
#   pivot_wider(id_cols = c(est_st, week),
#               names_from = hps.prifdinsuf_income,
#               values_from = hps.prifdinsuf_income_rate,
#               values_fill = 0)
#
# df.use.us2 <- df.use %>%
#   group_by(week, hps.prifdinsuf_income) %>%
#   summarise(nres = sum(pweight, na.rm = TRUE)) %>%
#   mutate(ntot = sum(nres, na.rm = TRUE),
#          hps.prifdinsuf_income_rate = nres / ntot) %>%
#   ungroup() %>%
#   select(week, hps.prifdinsuf_income, hps.prifdinsuf_income_rate) %>%
#   pivot_wider(id_cols = c(week),
#               names_from = hps.prifdinsuf_income,
#               values_from = hps.prifdinsuf_income_rate,
#               values_fill = 0) %>%
#   mutate(est_st = "99")
#
# df.use.st2 <- bind_rows(df.use.st2, df.use.us2)
#
# ## get the change in FI by income class
# vars.use <- c("curfoodsuf", "income", "prifoodsuf")
# df.use <- df.puf %>%
#   select(all_of(c(vars.meta, vars.use)))
#
# for (i in seq_along(vars.use)) {
#   var <- vars.use[i]
#   df.use <- df.use %>% remove_missing(var)
# }
#
# df.use <- df.use %>%
#   mutate(fdstatchg = case_when(
#            curfoodsuf %in% c(1, 2) & prifoodsuf %in% c(1, 2) ~ "remain_good",
#            curfoodsuf %in% c(1, 2) & prifoodsuf %in% c(3, 4) ~ "get_better",
#            curfoodsuf %in% c(3, 4) & prifoodsuf %in% c(3, 4) ~ "remain_bad",
#            curfoodsuf %in% c(3, 4) & prifoodsuf %in% c(1, 2) ~ "get_worse",
#            TRUE ~ "others"),
#          hps.fdstatchg_income = str_c("DFI_", fdstatchg, "_", "Income_", income))
#
# ## FI status change by income class
# df.use.st3 <- df.use %>%
#   group_by(est_st, week, hps.fdstatchg_income) %>%
#   summarise(nres = sum(pweight, na.rm = TRUE)) %>%
#   mutate(ntot = sum(nres, na.rm = TRUE),
#          hps.fdstatchg_income_rate = nres / ntot) %>%
#   ungroup() %>%
#   select(est_st, week, hps.fdstatchg_income, hps.fdstatchg_income_rate) %>%
#   pivot_wider(id_cols = c(est_st, week),
#               names_from = hps.fdstatchg_income,
#               values_from = hps.fdstatchg_income_rate,
#               values_fill = 0)
#
# df.use.us3 <- df.use %>%
#   group_by(week, hps.fdstatchg_income) %>%
#   summarise(nres = sum(pweight, na.rm = TRUE)) %>%
#   mutate(ntot = sum(nres, na.rm = TRUE),
#          hps.fdstatchg_income_rate = nres / ntot) %>%
#   ungroup() %>%
#   select(week, hps.fdstatchg_income, hps.fdstatchg_income_rate) %>%
#   pivot_wider(id_cols = c(week),
#               names_from = hps.fdstatchg_income,
#               values_from = hps.fdstatchg_income_rate,
#               values_fill = 0) %>%
#   mutate(est_st = "99")
#
# df.use.st3 <- bind_rows(df.use.st3, df.use.us3)
#
# ## FI status change with all income class
# df.use.st3a <- df.use %>%
#   group_by(est_st, week, fdstatchg) %>%
#   summarise(nres = sum(pweight, na.rm = TRUE)) %>%
#   mutate(ntot = sum(nres, na.rm = TRUE),
#          hps.fdstatchg_rate = nres / ntot) %>%
#   ungroup() %>%
#   select(est_st, week, fdstatchg, hps.fdstatchg_rate) %>%
#   pivot_wider(id_cols = c(est_st, week),
#               names_from = fdstatchg,
#               values_from = hps.fdstatchg_rate,
#               values_fill = 0,
#               names_prefix = "DFI_")
#
# df.use.us3a <- df.use %>%
#   group_by(week, fdstatchg) %>%
#   summarise(nres = sum(pweight, na.rm = TRUE)) %>%
#   mutate(ntot = sum(nres, na.rm = TRUE),
#          hps.fdstatchg_rate = nres / ntot) %>%
#   ungroup() %>%
#   select(week, fdstatchg, hps.fdstatchg_rate) %>%
#   pivot_wider(id_cols = c(week),
#               names_from = fdstatchg,
#               values_from = hps.fdstatchg_rate,
#               values_fill = 0,
#               names_prefix = "DFI_") %>%
#   mutate(est_st = "99")
#
# df.use.st3a <- bind_rows(df.use.st3a, df.use.us3a)
#
# df.hps3 <- df.use.st %>%
#   left_join(df.use.st2, by = c("est_st", "week")) %>%
#   left_join(df.use.st3, by = c("est_st", "week")) %>%
#   left_join(df.use.st3a, by = c("est_st", "week")) %>%
#   rename(state.fips = est_st)
#
# df.hps3 <- full_join(df.hps3, df.hps2, by = c("state.fips", "week"))
#
# saveRDS(df.hps3, file = "output_data/20210126_hps_fdinsuf_state.rds")
