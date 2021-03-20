source("cmd_initiate.R")

####################  Prepare data ####################

f <- "input_data/20210318_mental_health_aggregate.csv"
df.mental <- read_csv(f)

f <- "input_data/hps_phases.csv"
df.wks <- read_csv(f)

df.stfips <- fips_codes %>%
    select(state.fips = state_code, state.abb = state, state = state_name) %>%
    distinct()

df.stfips <- rbind(df.stfips,
                   tibble(state.fips = "99",
                          state.abb = "US",
                          state = "United States"))

sp.state <- states(cb = TRUE, year = 2010)
## sf.state <- st_as_sf(sp.state) %>%
##     rename(state.fips = STATE, state = NAME)

vars.meta <- c("state.fips", "week")

df.use <- df.mental %>%
    left_join(df.wks, by = "week") %>%
    rename(state.fips = est_st) %>%
    left_join(df.stfips, by = "state.fips")
