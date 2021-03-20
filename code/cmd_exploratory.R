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


####################  Plotting ####################
df.plt <- df.use %>%
    filter(vars == "anxious.agg",
           state.fips %in% c("42", "99")) %>%
    select(week, end, rate, state)

ypos <- max(df.plt$rate, na.rm = TRUE)
ll <- ggplot(df.plt,
             aes(x = end, y = rate, color = state)) +
    ## line for phase I
    geom_line(data     = df.plt %>% filter(end <= ymd("2020-07-21")),
              size     = 1,
              linetype = "solid") +
    ## line for phase II
    geom_line(data     = df.plt %>% filter(end >= ymd("2020-08-31")),
              size     = 1,
              linetype = "solid") +
    ## add a dashed line
    geom_line(data     = df.plt,
              size     = 1,
              linetype = "dashed",
              alpha    = 0.5) +
    geom_text(aes(label = scales::percent(rate, .1, suffix = "")),
              size      = 5,
              nudge_y   = 0.005,
              show.legend = FALSE) +
    ## add some reference lines
    geom_vline(xintercept = ymd("2020-07-21"),
               linetype   = "dotted",
               alpha      = 0.4,
               size       = 1) +
    geom_vline(xintercept = ymd("2020-08-31"),
               linetype   = "dotted",
               alpha      = 0.4,
               size       = 1) +
    geom_vline(xintercept = ymd("2020-10-28"),
               linetype   = "dotted",
               alpha      = 0.4,
               size       = 1) +
    ## add text annotations
    geom_text(x     = ymd("2020-06-12"),
              y     = ypos*1.1,
              color = "black",
              label = "Phase I", size = 6) +
    geom_text(x     = ymd("2020-09-30"),
              y     = ypos*1.1,
              color = "black",
              label = "Phase II",
              size  = 6) +
    geom_text(x     = ymd("2020-11-25"),
              y     = ypos*1.1,
              color = "black",
              label = "Phase III",
              size  = 6) +
    scale_x_date(name        = "HPS survey weeks",
                 date_breaks = "2 weeks",
                 date_labels = "%m/%d/%y",
                 expand      = c(0.02, 0.03)) +
    scale_y_continuous(name    = "Percentage",
                       expand  = c(0, 0.05),
                       labels  = scales::percent_format(.1, suffix = "%")) +
    scale_color_brewer(palette = "Set1") + # manual(values = c("PA" = "black")) +
    ggtitle("The percentage of people who often felt anxious in the last 7 days") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 13, angle = 45, hjust = 0.9),
          text        = element_text(size = 14),
          legend.position = c(0.9, 0.15),
          panel.grid.minor = element_blank())

ggsave(plot = ll, filename = "output_img/20210221_anxious_lines_PA_US.png",
       width = 15, height = 6)
