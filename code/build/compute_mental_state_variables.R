## Generate mental health variables
source("initiate.R")

## read the input data from the hps PUF
f <- "/Users/ztian/OneDrive - The Pennsylvania State University/research/database/pulse_household/output_data/hps_individual_week1_27.rds"
df.puf <- readRDS(f)

## A function to compete the whole process for one HPS variable
##'
##' @param params a list of parameters to be fed into the function, including
##'     (1) var.use: a string for the variable name.
##'     (2) value.agg: a vector of integer that are aggregated to one category.
##'     (3) newvar: a string for the new variable name
##' @return a list of data frame objects, including
##'     (1) noagg: a data frame with the original category, including the count, rate, and the corresponding universe total count.
##'     (2) agg: a data frame with the aggregated category and new variables for count and rate.
##' @author Zheng Tian
compute_aggregated_variables <- function(params) {

    var.use   <- params$var.use
    value.agg <- params$value.agg
    newvar    <- params$newvar

    vars.grp = c("week", "est_st")

    ## Compute the count weighted count for each original category of the
    ## variable of interest.
    df <- compute_group_count(
        df.puf     = df.puf,
        vars.count = var.use,
        vars.group = vars.grp,
        var.wt     = "pweight")

    ## use the cleaned data for the steps following
    df.clean <- df$clean
    df.miss <- df$missing

    ## Compute the total count of the universe corresponding to the var.use
    df.total <- df.clean %>%
        ## add across the grouping variables
        count(across({{ vars.grp }}), wt = count, name = "total")

    ## join with the count and compute the ratio
    df.rate <- left_join(df.clean, df.total, by = vars.grp) %>%
        mutate(rate = count / total)

    ## aggregate some values (count and rate) of the variable and create new
    ## variables
    df.agg <- df.rate %>%
        ## use the unquo operation here
        filter(!! sym(var.use) %in% value.agg) %>%
        group_by(across({{ vars.grp }})) %>%
        summarise(across(all_of(c("count", "rate")),
                         ~ sum(., na.rm = TRUE),
                         .names = "{newvar}.{.col}")) %>%
        ungroup()

    ## reformat the non-aggregated data frame so that it will be easily
    ## row-binded with other data frames for other variables
    df.rate <- df.rate %>%
        mutate(var = var.use) %>%
        rename(value = !! sym(var.use)) %>%
        select(all_of(c(vars.grp, "var", "value", "count", "rate", "total")))

    list(
        noagg = df.rate,
        agg   = df.agg,
        missing = df.miss
    )
}


params.list <- list(
    list(var.use = "anxious",    value.agg = c(3, 4), newvar = "beanxious"),
    list(var.use = "worry",      value.agg = c(3, 4), newvar = "beworry"),
    list(var.use = "interest",   value.agg = c(3, 4), newvar = "nointerest"),
    list(var.use = "down",       value.agg = c(3, 4), newvar = "feeldown"),
    list(var.use = "expctloss",  value.agg = c(1),    newvar = "expctloss"),
    list(var.use = "curfoodsuf", value.agg = c(3, 4), newvar = "fdinsuf"),
    list(var.use = "tenure",     value.agg = c(2, 3), newvar = "payrentmort"),
    list(var.use = "mortconf",   value.agg = c(1, 2), newvar = "nomortconf")
)

df.ls <- map(params.list, ~ compute_aggregated_variables(.))

df.mental.noagg <- map_dfr(df.ls, "noagg")

df.mental.agg <- map(df.ls, "agg") %>%
    reduce(left_join, by = c("week", "est_st"))

## Since the universe of people who have no confidence in paying mortgage and
## rents are people who answered 2 and 3 in the question about tneure. To make
## the universe of people who have no confidence to all the population, I need
## to calculate another kind of rate.
df.mental.agg <- df.mental.agg %>%
    mutate(nomortconf.rate.allpop = nomortconf.rate * payrentmort.rate)


write_csv(df.mental.noagg, "output_data/mental_state_origcateg.csv")
write_csv(df.mental.agg, "output_data/mental_state_aggcateg.csv")
