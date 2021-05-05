# select_tidy_puf <- function(df.puf,
#                             vars.use = NULL,
#                             vars.selstr = NULL,
#                             vars.meta = c("scram", "week", "est_st", "pweight"),
#                             exclude.18y = TRUE,
#                             long.format = TRUE) {
#
#     ## exclude people whose age < 18
#     if (exclude.18y) {
#         df.puf <- df.puf %>%
#             filter((week <= 21 & tbirth_year <= 2002) | (week > 21 & tbirth_year <= 2003))
#     }
#
#     if (!is.null(vars.use)) {
#         df <- df.puf %>%
#             select(all_of(c(vars.meta, vars.use)))
#     }
#
#     if (!is.null(vars.selstr)) {
#         sel.expr <- rlang::parse_expr(vars.selstr)
#         df <- df.puf %>%
#             select(all_of(vars.meta), {{ sel.expr }})
#     }
#
#     if (long.format) {
#         ## convert the long format for easy aggregation
#         df <- df %>%
#             pivot_longer(cols = -any_of(vars.meta),
#                          names_to = "vars",
#                          values_to = "value")
#     }
#
#     return(df)
# }


#' A helper function to add US count when state counts are computed.
#'
#' @param df a data frame containing the count by state
#'
#' @return a new data frame with the US total appended
add_us_count <- function(df) {
  ## This is a handy function to add the count of the US based on the outcome
  ## from compute_group_count by state
  us <- count(df, across(-any_of(c("est_st", "count"))),
              wt = count,
              name = "count") %>%
    mutate(est_st = "99")
  return(rbind(df, us))
}


##' This function computes the count or weighted count of respondents for one or
##' more questions, grouped by grouping variables, such as week and state.
##'
##' @title Compute
##' @param df.puf A data frame that contains the original PUF variables.
##' @param vars.count A vector of strings for variable names to be counted.
##' @param vars.group A vector of strings for variable names that are used as
##'     the grouping variable, such as week and est_st. It can also include the
##'     variable(s) for cross-tabulation.
##' @param var.wt a single string for the variable name of weight.
##' @return A list of data frame. The item of clean is the data frame with
##'     completed answers, and the item of missing is the data frame with the
##'     missing answers. Each data frame contains a new column "count" for each
##'     cell of the combination of vars.count and vars.group.
##' @author Zheng Tian
compute_group_count <- function(df.puf, vars.count, vars.group, var.wt = NULL) {

    ## check variables are in the data frame
    vars.use <- c(vars.group, vars.count)
    stopifnot(all(c(vars.use, var.wt) %in% names(df.puf)))

    ## allow the raw count if var.wt = NULL; otherwise,
    ## add by the weighting variable
    if (is.null(var.wt)) {
        df <- df.puf %>%
            ## Here take advantage of the tidy-select with across
            count(across({{ vars.use }}), name = "count")
    } else {
        df <- df.puf %>%
            ## use the pronoun .data here for the weighting variable
            count(across({{ vars.use }}), wt = .data[[var.wt]], name = "count")
    }

    ## Set the conditions for missing values. Either NA or take the values of -88 or -99.
    ## The number of conditions depends on the number of variables
    cond <- map(vars.use,
                function(var) sprintf("(%s %%in%% c(-88, -99) | is.na(%s))", var, var))
    ## combine all conditions
    cond <- str_c(cond, collapse = " | ")
    ## parse the string to an expression
    cond <- rlang::parse_expr(cond)

    ## Separate missing data and complete ones
    df.miss  <- df %>% filter(eval(cond))
    df.clean <- df %>% filter(!eval(cond))

    return(list(clean = df.clean,
                missing = df.miss))
}


#' Remove non-response items.
#'
#' @param df the PUF data frame
#' @param vars.use a vector of strings for variable names
#' @return
remove_missing <- function(df, vars.use) {
  cond <- map(vars.use,
              function(var)
                sprintf("(%s %%in%% c(-88, -99) | is.na(%s))", var, var))
  ## combine all conditions
  cond <- str_c(cond, collapse = " | ")
  ## parse the string to an expression
  cond <- rlang::parse_expr(cond)

  df.miss  <- df %>% filter(eval(cond))
  df.clean <- df %>% filter(!eval(cond))

  return(list(clean = df.clean,
              missing = df.miss))
}


##' This function computes the aggregated count of respondants based on several values.
##'
##' @param df a data frame contains the variable to be aggregated.
##' @param var a string for the name of the variable to be aggregated.
##' @param val.agg a vector of numbers that are consider as one group.
##' @param newvar a string for the new variable name.
##' @param vars.group a vector of strings for the variable names of grouping.
##' @return A data frame with the new aggregated variable.
##' @author Zheng Tian
compute_agg_group_count <- function(df, var, val.agg, var.sum, newvar,
                                    vars.group = c("week", "est_st")) {

  ## check if the variables are all in the data frame
  stopifnot(var %in% names(df))
  stopifnot(var.sum %in% names(df))

  ## parse the string to an expression
  var.expr <- rlang::parse_expr(var)
  df.agg <- df %>%
    filter({{ var.expr }} %in% val.agg) %>%
    ## take advantag of tidy-select of across
    group_by(across({{ vars.group }})) %>%
    ## rename the sum to the new variable, using the pronoun .data
    summarise(!! sym(newvar) := sum(.data[[var.sum]], na.rm = TRUE)) %>%
    ungroup()
}


# compute_agg_wrapup <- function(df.agg) {
#
#   ## aggregate the count
#   df.agg.count <- pmap(df.agg,
#                        function(df, var, val.agg, newvar, vars.group) {
#                          compute_agg_group_count(
#                            df = df,
#                            var = var,
#                            val.agg = val.agg,
#                            var.sum = "count",
#                            newvar = newvar,
#                            vars.group = vars.group
#                          )
#                        })
#
#   ## aggregate the rate
#   df.agg.rate <- pmap(df.agg,
#                       function(df, var, val.agg, newvar, vars.group) {
#                         compute_agg_group_count(
#                           df = df,
#                           var = var,
#                           val.agg = val.agg,
#                           var.sum = "rate",
#                           newvar = newvar,
#                           vars.group = vars.group
#                         )
#                       })
#
#   ## get the joining variables
#   vars.join <- df.agg$vars.group[[1]]
#   map2(df.agg.count,
#        df.agg.rate,
#        function(df1, df2) {
#          left_join(df1, df2, by = vars.join, suffix = c(".count", ".rate"))
#        }
#   )
#
# }
#

## compute_value_count <- function(df.puf.lg, grp.vars = c("week", "est_st", "vars", "value")) {

##   ## compute count
##   df.agg <- df.puf.lg %>%
##     group_by(across({{ grp.vars }})) %>%
##     ## pweight is the weight to assign to each respondent;
##     ## the sum of pweight is an estimate of total count in population.
##     summarise(count = sum(pweight, na.rm = TRUE)) %>%
##     ungroup()

##   return(df.agg)
## }



#' Check the missing values in the PUF data
#'
#' @param df.puf a data frame that contains the original PUF data from the HPS
#' @param vars.use a vector of variable names to be used
#'
#' @return df.perc a data frame that contains the number of missing values (with suffix _miss),
#'         the number of observations (with suffix _nobs), and the percentage of missing values
#'         (with suffix _perc)
check_missing_values <- function(df.puf, vars.use) {
  ## convert the data into the long format
  df.puf.lg <- select_tidy_puf(df.puf, vars.use)

  ## missing answers
  df.miss <- df.puf.lg %>%
    group_by(week, est_st, vars) %>%
    summarise(miss = sum(is.na(value)))

  df.nobs <- df.puf.lg %>%
    group_by(week, est_st, vars) %>%
    summarise(nobs = n())

  df.perc <- df.miss %>%
    left_join(df.nobs, by = c("week", "est_st", "vars")) %>%
    mutate(perc = miss / nobs)

  df.perc <- df.perc %>%
    pivot_wider(names_from = vars,
                values_from = c(miss, nobs, perc)) %>%
    ungroup()
}


## aggregate_state_count_nocrosstab <- function(df.puf, vars.use) {

##     df.use <- select_tidy_puf(df.puf, vars.use, long.format = TRUE)

##     df.st <- compute_value_count(df.use,
##                                  grp.vars = c("week", "est_st", "vars", "value"))

##     df.us <- compute_value_count(df.use,
##                                  grp.vars = c("week", "vars", "value")) %>%
##         mutate(est_st = "99")

##     df.st <- rbind(df.st, df.us)

##     return(list(data = df.st %>% filter(!(value %in% c(-99, -88) | is.na(value))),
##                 noresp = df.st %>% filter(value %in% c(-99, -88) | is.na(value))))

## }


## aggregate_state_count_crosstab <- function(df.puf, vars.use) {

##     df.use <- select_tidy_puf(df.puf, vars.use = vars.use,
##                               long.format = FALSE)

##     s <- str_c(vars.use, " %in% c(-99, -88)")
##     s <- str_c(s, collapse = " | ")
##     ee <- rlang::parse_expr(s)

##     df.nonresp <- df.use %>%
##         filter(!! ee)

##     df.use <- df.use %>%
##         filter(!(!! ee))

##     df.st <- df.use %>%
##         compute_value_count(grp.vars = c("week", "est_st", vars.use))

##     df.us <- df.use %>%
##         compute_value_count(grp.vars = c("week", vars.use)) %>%
##         mutate(est_st = "99")

##     df.st <- rbind(df.st, df.us)

##     return(list(data = df.st,
##                 noresp = df.nonresp))
## }


# gen_crosstab_vars <- function(df, vars.cross) {
#     s <- map(vars.cross, function(x) str_c(x, df[[x]], sep = "_"))
#     s <- pmap_chr(s, function(...) str_c(..., sep = "_"))
#     df$vars <- s
#     return(df)
# }
#
#
# ##' This function is to compute the ratio variables, for example, the percentage
# ##' of respondents who answered (3) and (4) for the question of during the last
# ##' 7 days, did you feel anxious.
# ##'
# ##' @title Compute combined rate
# ##' @param df.st data frame. State level data of count of each survey item.
# ##' @param var string. The variable name of interest.
# ##' @param val vector of integre. The integer for answer items
# ##' @param newvar string. The new variable name.
# ##' @return a new data frame with both count and ratio variables.
# ##' @author Zheng Tian
# compute_combined_rate <- function(df.st, var, val, newvar) {
#   df.st %>%
#     filter(vars == var, value %in% val) %>%
#     group_by(week, est_st) %>%
#     summarise(count = sum(count, na.rm = TRUE),
#               rate = sum(rate, na.rm = TRUE)) %>%
#     rename(!!sym(str_c(newvar, ".rate")) := rate,
#            !!sym(newvar) := count) %>%
#     ungroup()
# }
