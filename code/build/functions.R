
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


## ##' This function computes the aggregated count of respondants based on several values.
## ##'
## ##' @param df a data frame contains the variable to be aggregated.
## ##' @param var a string for the name of the variable of which the values are to be aggregated.
## ##' @param val.agg a vector of numbers that are consider as one group.
## ##' @param newvar a string for the new variable name.
## ##' @param vars.group a vector of strings for the variable names of grouping.
## ##' @return A data frame with the new aggregated variable.
## ##' @author Zheng Tian
## compute_agg_group_count <- function(df, var, val.agg, var.sum, newvar,
##                                     vars.group = c("week", "est_st")) {

##   ## check if the variables are all in the data frame
##   stopifnot(var %in% names(df))
##   stopifnot(var.sum %in% names(df))

##   ## parse the string to an expression
##   var.expr <- rlang::parse_expr(var)
##   df.agg <- df %>%
##     filter({{ var.expr }} %in% val.agg) %>%
##     ## take advantag of tidy-select of across
##     group_by(across({{ vars.group }})) %>%
##     ## rename the sum to the new variable, using the pronoun .data
##     summarise(!! sym(newvar) := sum(.data[[var.sum]], na.rm = TRUE)) %>%
##     ungroup()
## }


## select_tidy_puf <- function(df.puf,
##                             vars.use = NULL,
##                             vars.selstr = NULL,
##                             vars.meta = c("scram", "week", "est_st", "pweight"),
##                             exclude.18y = TRUE,
##                             long.format = TRUE) {

##     ## exclude people whose age < 18
##     if (exclude.18y) {
##         df.puf <- df.puf %>%
##             filter((week <= 21 & tbirth_year <= 2002) | (week > 21 & tbirth_year <= 2003))
##     }

##     if (!is.null(vars.use)) {
##         df <- df.puf %>%
##             select(all_of(c(vars.meta, vars.use)))
##     }

##     if (!is.null(vars.selstr)) {
##         sel.expr <- rlang::parse_expr(vars.selstr)
##         df <- df.puf %>%
##             select(all_of(vars.meta), {{ sel.expr }})
##     }

##     if (long.format) {
##         ## convert the long format for easy aggregation
##         df <- df %>%
##             pivot_longer(cols = -any_of(vars.meta),
##                          names_to = "vars",
##                          values_to = "value")
##     }

##     return(df)
## }
