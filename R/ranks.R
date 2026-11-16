#' Wrapper function for rank-based statistical methods
#' 
#' @description
#' A wrapper function for any rank-based statistical
#' method, allowing it to be used with winnify(). 
#' 
#' @param formula An object of class formula describing the analyses to run. Format for model specification are given in Details
#' @param data A data.frame to analyse
#' @param ranks_statistic A function to run. See Details.
#' @param exponentiate A logical indicating if the results of \code{ranks_statistic} should be exponentiated
#' @param outcome_as_factor A logical indicating if the outcome to be fed to \code{ranks_statistic} should be cast to an ordered factor. If \code{FALSE}, a numeric value is passed. 
#' @param ... Any arguments to be passed to \code{ranks_statistic()} 
#' 
#' @details
#' This wrapper combines items on the left hand side of \code{formula} into a single
#' set of ranks and then runs \code{ranks_statistic}. When combined with \code{\link{winnify}()}
#' it allows arbitrary analysis functions to be used for Generalised Pairwise Comparisons
#' with Prioritised Outcomes.
#' 
#' The \code{ranks_statistic} function must satisfy the following criteria:
#' \itemize{
#' \item The first argument of the function must be a formula
#' \item There must be a named argument called \code{data} for providing the data
#' \item It must have \code{coef()} and \code{confint()} methods available for its output, which return the effect size and confidence intervals respectively
#' }
#' 
#' 
#' The left hand side of \code{formula} should list outcomes in order
#' of importance, with the most important outcome coming first.
#' Note that these are ranked from smallest value to highest value,
#' and a higher value is assumed to be preferred.
#' 
#' The right hand side of \code{formula} provides the grouping variable, marked with the function \code{w_arm(x)}.
#' It is expected that this variable is a factor containing exactly two values. If this term is not a factor,
#' it will be coerced into one before \code{ranks_statistic} is run.
#' 
#' All terms of \code{formula} are fed into \code{ranks_statistic}. Note that the estimates for wins, losses and ties
#' are unadjusted and ignore all terms other than the one marked as the grouping variable.
#' 
#' Note that if \code{ranks_statistic} requires profiling to be redone to estimate
#' confidence intervals (via e.g. a \code{vcov} call), this function will crash.
#' A notable example of this is \link[MASS]{polr}. Any optional arguments
#' to prevent re-profiling should be passed to \code{ranks_statistic} using \code{...}
#' 
#' @returns A one-row data frame containing the following:
#' \describe{
#' \item{effect_size}{The name of the \code{ranks_statistic} function}
#' \item{estimate}{The estimate from \code{ranks_statistic} function}
#' \item{lower}{The lower bound of the condifence interval estimate from \code{ranks_statistic} function}
#' \item{upper}{The upper bound of the confidence interval estimate from \code{ranks_statistic} function}
#' \item{pval}{\code{NA}}
#' \item{win}{The unadjusted number of wins}
#' \item{loss}{The unadjusted number of losses}
#' \item{tie}{The unadjusted number of ties}
#' } 
#' 
#' @examples
#' 
#' data <- neuro_improvement
#' formula <- mRS+early_neuro_improvement~w_arm(treat)
#' 
#' ## Use of ranks() by itself
#' 
#' # pim estimates the Win Odds for ordinal data
#' # and is structured to not require additional
#' # work to use it with ranks().
#' 
#' \donttest{
#' 
#' library(pim)
#' ranks(formula,data,ranks_statistic = pim)
#' 
#' ## Use of ranks() in a winnify() call
#' 
#' # estim=estimator.glm is used here because it's fast
#' x <- winnify(formula, ranks, data, ranks_statistic = pim, exponentiate=TRUE)
#' plot(x,effect_neutral_point = 1,pairs_tie = "split")
#' }
#' 
#' # Running other analysis methods, e.g.
#' # Proportional odds model
#' 
#' this_data <- winnify::remapcap
#' this_data$days_alive <- this_data$deathTime
#' this_data$days_out_of_icu <- 21-this_data$dischargeTime
#' 
#' # We only want two groups - organise factor levels and drop other.
#' this_data$group <- factor(this_data$group,levels = c("uc","shock"))
#' this_data <- this_data[which(!is.na(this_data$group)),]
#' 
#' library(MASS) 
#' ranks(days_alive + days_out_of_icu ~ w_arm(group),
#'       data = this_data,
#'       ranks_statistic = polr, Hess=TRUE,
#'       exponentiate = TRUE)
#' ## Example of use of a function that needs a wrapper
#' 
#' # genodds estimates the Win Odds or Win Ratio for ordinal data
#' # but doesn't accept a formula input. This can be fixed by creating
#' # a wrapper function for it
#' 
#' library(genodds)
#'  
#' my_statistic <- function(formula,data,...){
#' 
#' 
#'         lhs <- stats::model.frame(formula[1:2],data=data)
#'         rhs <- stats::model.frame(formula[c(1,3)],data=data)
#'   
#'         if(ncol(lhs)!=1) stop("Left hand side of formula should have one term")
#'         if(ncol(rhs)!=1) stop("Left hand side of formula should have one term")
#'   
#'         fit <- genodds(response = lhs[,1],
#'                        group = rhs[,1],
#'                         ...)
#'   
#'         # genodds returns an object of class Genodds.
#'         # if it didn't, we would need to give it a class
#'         # that we can write write S3 methods for.         
#'         # class(fit) <- c("MyRankStat",class(fit))   
#'         fit
#' }
#' 
#' # There's no coef() or confint() functions for
#' # class Genodds. We need to create some S3 methods to
#' # extract this info.
#' coef.Genodds <- function(x){x$pooled_lnodds}
#' confint.Genodds <- function(x){matrix(x$pooled_lnconf.int,nrow=1)}
#' 
#' wo_result <- winnify(formula, ranks, data, ranks_statistic = my_statistic,
#'                      exponentiate=TRUE, ties="split")
#' plot(wo_result,effect_neutral_point = 1,pairs_tie = "split")
#' 
#' wr_result <- winnify(formula, ranks, data, ranks_statistic = my_statistic,
#'                      exponentiate=TRUE, ties="drop")
#' plot(wr_result,effect_neutral_point = 1,pairs_tie = "drop")
#' 
#'
#' @export ranks 
ranks <- function(formula,data,ranks_statistic,exponentiate=FALSE,outcome_as_factor=TRUE,...){
   
    # Silence note about global variable bindings
    stat_name <- deparse(substitute(ranks_statistic))

    # Formula Parsing --------------------------------
    specials <- c("w_id","w_arm","w_stratum","w_time")
    accepted_specials <- c("w_arm")

    lhs <- stats::terms.formula(formula[c(1,2)],specials = specials)
    for(i in specials){
            if(length(attr(lhs,"specials")[[i]])>0) stop(sprintf("%s() should not be used on left hand side of formula",i))
    }

    rhs <- stats::terms.formula(formula[c(1,3)],specials = specials)
    for(i in accepted_specials){
            if(length(attr(rhs,"specials")[[i]])>1) stop(sprintf("%s() should only be used once in the right hand side of formula",i))
    }

    for(i in setdiff(specials,accepted_specials)){
            if(length(attr(rhs,"specials")[[i]])>0) stop(sprintf("%s() is not allowed in ranks() function",i))
    }

    # Need to get combined ranks

    groupVar <- attr(rhs,"specials")$w_arm
    if(is.null(groupVar)){
            stop("w_arm() is required")
    } else {
            groupVar <- attr(rhs,"term.labels")[groupVar]
    }
 
    otherVars <- attr(rhs,"term.labels")[setdiff(1:length(attr(rhs,"term.labels")), unlist(attr(rhs,"specials")))]


   
    # Main Data Setup --------------------------------

    main_data <- stats::model.frame(rhs,data)

    treatVar <- which(groupVar == colnames(main_data))
    if(!("factor" %in% class(main_data[,treatVar]))) main_data[,treatVar] <- as.factor(main_data[,treatVar])

    # Outcome ranks need constructed
    outcome_data <- data.frame(
            Y1 = eval(parse(text=attr(lhs,"term.labels")[1]),envir = data)
    )
    if(length(attr(lhs,"term.labels"))>1){
        for(i in 2:length(attr(lhs,"term.labels")))
        {
            outcome_data <- cbind(outcome_data,
                eval(parse(text=attr(lhs,"term.labels")[i]),envir = data)
            )
            colnames(outcome_data)[i] <- paste0("Y",i)
        }
    }
    for(i in 1:ncol(outcome_data)) if(!("factor" %in% outcome_data[,i])) outcome_data[,i] <- as.factor(outcome_data[,i])

    # Get a list of all possible combinations of each outcome. 
    # Note that we need to reverse the list of levels,
    # because expand.grid varies the first factor
    # fastest, not slowest.
    door_list <- do.call(
        "expand.grid",
        rev(lapply(as.list(outcome_data),levels)),
    )

    # Get a numeric score corresponding to
    # preference on this combination of outcomes.
    # We assume higher scores are better
    # (i.e. best ranked has highest score)

    outcome_var <- rep(NA,nrow(main_data))
    for(i in 1:nrow(door_list)){

        this_match <- rep(TRUE,nrow(main_data))

        for(j in colnames(door_list)){
            this_match <- this_match & (as.character(outcome_data[,j]) == as.character(door_list[i,j]) )            
        }
        
        outcome_var[this_match] <- i
    }

    # There could be something funny in the column names for main_data, so we'll pick a variable name that isn't taken
    if("outcome" %in% colnames(main_data)){
        outcome_name <- setdiff(paste0("outcome",1:ncol(main_data)),colnames(main_data))[1]
    } else {
        outcome_name <- "outcome"
    }
    main_data <- cbind(main_data, outcome_var)
    colnames(main_data)[ncol(main_data)] <- outcome_name

    if(outcome_as_factor) main_data[,outcome_name] <- factor(main_data[,outcome_name], ordered=TRUE)

    # Get wins, losses and ties based on pairwise comparisons
    if(length(otherVars)>0){
        warning("win, loss and tie estimates ignore all formula terms beyond group membership")
    }
    pwc <- table(with(data,eval(parse(text=groupVar)),main_data),main_data[,outcome_name])    
    pwc <- outer(
        pwc[1,],
        pwc[2,]
    )
    
    new_formula <- stats::as.formula(paste0(outcome_name,as.character(rhs)[1],paste0("`",as.character(rhs)[2],"`",collapse="")))

    analysis <- ranks_statistic(new_formula,data=main_data,...)

    analysis_exports <- .get_coef_confint(analysis)
    effectSize <- analysis_exports$coef
    confidenceInterval <- analysis_exports$confint
      
    if(length(effectSize)>1){
        effectSize <- effectSize[grepl(groupVar,names(effectSize),fixed = TRUE)]
    }
    
    if(is.matrix(confidenceInterval)){
        if(nrow(confidenceInterval)>1){
            confidenceInterval <- confidenceInterval[grepl(groupVar,rownames(confidenceInterval),fixed = T),,drop=FALSE]
        }
    } else if(is.vector(confidenceInterval)){
        confidenceInterval <- matrix(confidenceInterval,nrow=1)
    } else {
        stop("confint(ranks_statistic()) doesn't return a matrix or vector")
    }

    if(length(effectSize)>1) stop(sprintf("coef(%s(...)) retuned more than one value that matched %s", stat_name,groupVar))
    if(nrow(confidenceInterval)>1) stop(sprintf("confint(%s(...)) retuned more than one row that matched %s", stat_name,groupVar))

    out <- data.frame(
        effect_size = stat_name,
        estimate = effectSize,
        lower = confidenceInterval[1,1],
        upper = confidenceInterval[1,2],
        pval = NA,
        win = sum(pwc[upper.tri(pwc)]),   
        loss = sum(pwc[lower.tri(pwc)]),
        tie = sum(diag(pwc))
    )
    if(exponentiate){
     out$estimate <- exp(out$estimate)   
     out$lower <- exp(out$lower)   
     out$upper <- exp(out$upper)   
    }

    rownames(out) <- NULL
    return(out)

}