source("cmd_initiate.R")

f <- "input_data/20210318_mental_health_aggregate.csv"
df.mental <- read_csv(f)

f <- "input_data/hps_phases.csv"
df.wks <- read_csv(f)
