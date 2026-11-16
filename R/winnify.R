#' Better Prioritised Outcomes Analyses 
#'
#' @description 
#' winnify is a wrapper function that facilitates improved reporting of 
#' Generalised Pairwise Comparisons with Prioritised Outcomes e.g.
#' Win Ratio (Pocock et al. 2012), Win Odds (Dong et al. 2020) and
#' Net Benefit (Buyse et al. 2010).
#' 
#' It takes a formula that specifies one or more outcome variables
#' and then iteratively executes an analysis function to estimate
#' a statistic based on the first outcome, then the first and second outcome,
#' and so on. It also estimates the statistic on each outcome independently
#' of all other outcomes to provide additional context.
#' 
#' In doing so, it provides convenient estimates of the statistic
#' as successively less important outcomes are added to the analysis.
#' This enables improved visualisation of results across outcomes and
#' more transparent reporting (Johns et al. 2026). 
#' 
#' It also facilitates the use of rank-based statistical functions
#' for Generalised Pairwise Comparisons with Prioritised Outcomes
#' when preferences among pairwise comparisons are fully transitive.  
#'
#' @param formula An object of class formula describing the analyses to run. Details of model specification are given in Details
#' @param data A data.frame to analyse
#' @param statistic A function for performing analysis based on a formula and data. Details are given in Details.
#' @param ... Other arguments passed onto \code{statistic}
#'
#' @details
#' The left hand side of the formula should list all outcomes to consider, in order of decreasing importance from the left.
#' The right hand side of the formula lists all independent variables. The following special functions are used to indicate
#' fields of interest:
#' 
#' \itemize{
#' \item \code{w_arm(x)} - marks \code{x} as being the grouping variable. Is mandatory in the formula.
#' \item \code{w_id(x)} - marks \code{x} as being the ID variable
#' \item \code{w_stratum(x)} - marks \code{x} as being the stratum variable
#' \item \code{w_time(x)} - marks \code{x} as being the time variable for time-varying covariates.
#' \item \code{w_iptw(x)} - marks \code{x} as being a weight variable for use with iptw weighting
#' }
#' 
#' \code{w_arm(x)} is needed for both the supplied \link{wins} and \link{ranks} functions,
#' while \code{w_id(x)}, \code{w_stratum(x)} and \code{w_time(x)} are only accepted by \link{wins}. 
#' 
#' @returns An object of class Winnify
#' 
#' @examples
#' 
#' library(genodds)
#' 
#' my_statistic <- function(formula,data,...){
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
#'         # we can write write S3 methods for.         
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
#' result <- winnify(mRS+early_neuro_improvement~w_arm(treat),
#'                   statistic = ranks,
#'                   ranks_statistic = my_statistic,
#'                   data = neuro_improvement)
#' 
#' plot(result,effect_neutral_point = 1,effect_scale_log = FALSE)
#'
#' @references Johns, Hannah, et al. "Practical guidance for Win Statistics and Tournament Methods for multifaceted outcomes in stroke research: Review and recommendations." International Journal of Stroke (2026) DOI: https://doi.org/10.1177/17474930261475853
#' 
#' @rdname winnify
#' @export winnify
winnify <- function(formula, statistic=winnify::wins, data, ...){

        # Input sanitation
        data <- as.data.frame(data)
        if(!is.function(statistic)) stop("statistic should be a function with formula and data arguments")
        if(!rlang::is_formula(formula)) stop("formula should be a formula")

        if(!("formula" %in% methods::formalArgs(statistic))) stop("statistic function needs a formula argument")
        if(!("data" %in% methods::formalArgs(statistic))) stop("statistic function needs a data argument")

        lhs <- formula[-3]
        rhs <- formula[-2]

        lhs <- attr(stats::terms(lhs),"term.labels")

        out <- lapply(1:length(lhs), function(i){
                tmp_data <- data

                tmp_formula <- stats::as.formula(paste(c(lhs[i],as.character(rhs)),collapse = ""))
                individual_result <- statistic(formula=tmp_formula,data = tmp_data,...)

                tmp_formula <- stats::as.formula(paste(c(paste(lhs[1:i],collapse="+"),as.character(rhs)),collapse = ""))
                cumulative_result <- statistic(tmp_formula,tmp_data,...)


                if(any(c("data.frame","matrix") %in% class(individual_result))){
                        individual_result <- as.data.frame(individual_result)
                        cumulative_result <- as.data.frame(cumulative_result)
                } else {
                        individual_result <- as.data.frame(matrix(individual_result,nrow = 1,dimnames = list(NULL,names(individual_result))))
                        cumulative_result <- as.data.frame(matrix(cumulative_result,nrow = 1,dimnames = list(NULL,names(cumulative_result))))
                }

                # Construct rows based on these results
                if("effect_size" %in% colnames(individual_result)){
                        effectSize_name <- individual_result[,"effect_size"]
                } else {
                        effectSize_name <- "Effect Size"
                }


                result <- data.frame(effect_size = effectSize_name,
                                     outcome_names = i
                )

                if("neutral_point" %in% colnames(individual_result)){
                        result <- cbind(result,neutral_point = individual_result[,"neutral_point"])
                }

                if("tie_handling" %in% colnames(individual_result)){
                        result <- cbind(result,tie_handling = individual_result[,"tie_handling"])
                }

                colnames(individual_result) <- paste(colnames(individual_result),"individual",sep="_")
                colnames(cumulative_result) <- paste(colnames(cumulative_result),"cumulative",sep="_")

                result <- cbind(result,
                                individual_result[,which(!grepl("^(effect_size|win|loss|tie|neutral_point|tie_handling)",colnames(individual_result)))],
                                cumulative_result[,which(!grepl("^(effect_size|neutral_point|tie_handling)",colnames(cumulative_result)))]
                )

                attr(result,"groups") <- attr(individual_result,"groups")

                result

        })

        groups <- lapply(out,function(x){attr(x,"groups")})
        groups <- unique(groups)
        if(length(groups) != 1){
                groups <- c("Control", "Intervention")
        } else {
                groups <- unlist(groups)
        }
        out <- as.data.frame(do.call("rbind",out))
        out$outcome_names <- factor(out$outcome_names, levels = 1:length(lhs), labels = lhs)

        out <- as_winnify.data.frame(out)

        attr(out,"effect.sizes") <-  unique(out$outcome)
        attr(out,"groups") <- groups

        out

}

#' 
#' @param x a \code{Winnify} object returned by \code{\link{winnify}()} or \code{\link{as_winnify}()}
#' @param effect_size a character string or integer specifying which effect size to plot. Only relevant when \code{x} reports multiple effect sizes.
#' @param label_effect a character string for labelling the effect size. Inferred from \code{x} and \code{effect_size} if possible.
#' @param label_control a character string indicating the name of the control group
#' @param label_intervention a character string indicating the name of the intervention/comparator group
#' @param pairs_tie a character string indicating where the line splitting ties in the Overall row of the pairs plot should be drawn. If \code{NA}, no line is drawn. See Details. Inferred from \code{x} if possible.
#' @param pairs_tie.color a character string indicating the color of the line splitting ties
#' @param pairs_tie.linetype a character string indicating the linetype of the line splitting ties
#' @param pairs.fill a character string or character vector indicating the color scheme to use for the pairs plot. See Details.
#' @param pairs.fill.reverse a logical indicating if the color scheme provided in \code{pairs.fill} should be reversed. See Details.
#' @param pairs_component.width a number indicating the width of the component bars in the pairs plot.
#' @param pairs_overall.width a number indicating the width of the overall bar in the pairs plot.
#' @param effect_scale_log a logical indicating if the effect size should be shown on a log scale
#' @param effect_neutral_point  a numeric value indicating the neutral point for the effect plot. Inferred from \code{x} if possible.
#' @param effect_neutral_line.color a character string indicating the color of the neutral line in the effect plot.
#' @param effect_background_loss.fill a character string indicating the color of the background corresponding to favoring control in the effect plot. By default, inferred from \code{pairs.fill}
#' @param effect_background_win.fill a character string indicating the color of the background corresponding to favoring intervention in the effect plot. By default, inferred from \code{pairs.fill}
#' @param effect_background.alpha a numeric value indicating the opacity (alpha) of the background in the effect plot
#' @param effect_individual_color a character string indicating the color of the individual treatment effects in the effect plot
#' @param effect_cumulative.color a character string indicating the color of the cumulative treatment effects in the effect plot
#' @param effect_cumulative.dodge_width a number indicating the dodging width for the cumulative effect in the effect plot
#' @param effect_cumulative_polygon.height a number indicating the height of the final diamond showing the overall effect
#' @param effect_cumulative.linetype = a character string indicating the linetype for the line connecting cumulative effects. \code{NA} removes this line.
#' @param ... additional arguments
#'
#' @details
#'
#' \code{pairs_tie} controls how splitting of ties in the final overall value are visualised. There are three options for this:
#' \itemize{
#' \item \code{"drop"} - ties are split to respect the final ratio of wins to losses. This is consistent with the Win Ratio
#' \item \code{"split"} - ties are split evenly between wins and losses. This is consistent with the Win Odds
#' \item \code{NA} - no line is drawn
#' }
#'
#' \code{pairs.fill} can either be a character vector of length 5 or the name of a ColorBrewer palette (see <https://colorbrewer2.org>).
#'
#'  If a character vector is supplied, it should contain the following named values:
#'  \itemize{
#'  \item \code{"loss_inherited"} - the fill color for losses inherited from the previous level
#'  \item \code{"loss"}  - the fill color for losses new to the current level
#'  \item \code{"neutral"}  - the fill color for neutral/tied pairs
#'  \item \code{"win"}  - the fill color for wins new to the current level
#'  \item \code{"win_inherited"}  - the fill color for wins inherited from the previous level
#' }
#' If the vector is unnamed, names will be assumed based on these positions.
#'
#' If a ColorBrewer pallette is provided, a palette of length 5 is used. This pallette is reordered such that the sequence of colors is 2,1,3,5,4.
#' This allocates the extremes of the colour pallette to new wins and losses.
#' \code{pairs.fill.reverse} can be used to reverse the overall color sequence for a ColorBrewer pallette and is otherwise ignored.
#'
#'
#'
#'
#' 
#' @importFrom base plot
#' @rdname winnify
#' @method plot Winnify
#' @export plot Winnify
plot.Winnify <- function(x,
                         effect_size = NULL,

                         label_effect=NULL,
                         label_control = NULL,
                         label_intervention = NULL,

                         pairs_tie=NULL,
                         pairs_tie.color="#222222",
                         pairs_tie.linetype="dashed",

                         pairs.fill = NULL,
                         pairs.fill.reverse = NULL,
                         pairs_component.width  = 1,
                         pairs_overall.width  = 0.6,

                         effect_scale_log=FALSE,
                         effect_neutral_point=NULL,
                         effect_neutral_line.color = "#222222",

                         effect_background_loss.fill = NULL,
                         effect_background_win.fill = NULL,
                         effect_background.alpha=0.15,

                         effect_individual_color = "grey40",

                         effect_cumulative.color = "black",
                         effect_cumulative.dodge_width = 0.1,
                         effect_cumulative_polygon.height = 0.36,
                         effect_cumulative.linetype = "dashed",

                         ...
                    ){

        
        
        # Silence note about gloval variable bindings
        level <- width <- p_prev <- p <- pairType <- xmin <- xmax <- ymin <-
        ymax <- favours_int <- level_offset <- estimat <- ci_lower <- 
        ci_upper <- outcome_type <- x_coord <- y_coord <- estimate <-  NULL


        if(is.null(label_control)){
                label_control <- attr(x, "groups")[1]
        }

        if(is.null(label_intervention)){
                label_intervention <- attr(x, "groups")[2]
        }

        if(is.null(label_control)){
                label_control <- "Control"
        }
        if(is.null(label_intervention)){
                label_intervention <- "Intervention"
        }

        pairs.fill.reverse_redundant <- !is.null(pairs.fill.reverse)
        if(is.null(pairs.fill)){
                pairs_loss_inherited.fill="tan1"
                pairs_loss.fill="red2"
                pairs_neutral.fill="khaki1"
                pairs_win.fill = "forestgreen"
                pairs_win_inherited.fill = "#97d42c"
        } else if(is.character(pairs.fill) & length(pairs.fill)==1 & all(pairs.fill %in% rownames(RColorBrewer::brewer.pal.info))){

                pairs.fill.reverse_redundant <- FALSE
                brewer_pallette <- RColorBrewer::brewer.pal(5,pairs.fill)
                if(pairs.fill.reverse) brewer_pallette <- rev(brewer_pallette)

                pairs_loss_inherited.fill <- brewer_pallette[2]
                pairs_loss.fill <- brewer_pallette[1]
                pairs_neutral.fill <- brewer_pallette[3]
                pairs_win.fill = brewer_pallette[5]
                pairs_win_inherited.fill <- brewer_pallette[4]

        } else if(is.character(pairs.fill) & length(pairs.fill) == 5){
                if(!is.null(names(pairs.fill))){

                        if(length(setdiff(
                                c("loss_inherited","loss","neutral","win","win_inherited"),
                                names(pairs.fill)
                        ))){
                                stop(sprintf(
                                        "Missing the following named elements %s",
                                        paste0(setdiff(
                                                c("loss_inherited","loss","neutral","win","win_inherited"),
                                                names(pairs.fill)
                                        ),collapse = ", ")
                                ))
                        }

                        pairs_loss_inherited.fill <- pairs.fill["loss_inherited"]
                        pairs_loss.fill <- pairs.fill["loss"]
                        pairs_neutral.fill <- pairs.fill["neutral"]
                        pairs_win.fill = pairs.fill["win"]
                        pairs_win_inherited.fill <- pairs.fill["win_inherited"]

                } else {
                        pairs_loss_inherited.fill <- pairs.fill[1]
                        pairs_loss.fill <- pairs.fill[2]
                        pairs_neutral.fill <- pairs.fill[3]
                        pairs_win.fill = pairs.fill[4]
                        pairs_win_inherited.fill <- pairs.fill[5]
                }
        } else {
                stop("unrecognised option for pairs.fill")
        }

        if(pairs.fill.reverse_redundant) warning("pairs.fill.reverse argument is only used when pairs.fill is a ColorBrewer pallette")

        if(is.null(effect_background_loss.fill)) effect_background_loss.fill <-  pairs_loss.fill
        if(is.null(effect_background_win.fill)) effect_background_win.fill <-  pairs_win.fill


        # Get which effect size to plot
        if(is.null(effect_size)) effect_size <- 1L
        if(length(effect_size) != 1) stop("effect_size should be of length 1")
        if(is.numeric(effect_size)){
                effect_size <- unique(x$effect_size)[as.integer(effect_size)]
        } else if(is.character(effect_size)){

                editDistances <- utils::adist(tolower(effect_size),tolower(unique(x$effect_size)),costs = c(insertions = 1, deletions = Inf, substitutions = Inf))
                editDistances <- c(editDistances)
                names(editDistances) <- unique(x$effect_size)
                editDistances <- editDistances[is.finite(editDistances)]

                if(length(editDistances)>1){

                        if(sum(editDistances==min(editDistances)) > 1){
                                stop(sprintf("'%s' is ambiguous and matches multiple of %s",effect_size,paste(unique(x$effect_size), collapse = ", ")))
                        } else {
                                chosen <- names(editDistances)[editDistances==min(editDistances)]
                                message(sprintf("'%s' matches multiple of %s. %s assumed.",effect_size,paste(unique(x$effect_size), collapse = ", "),chosen))
                                effect_size <- chosen
                        }

                } else if(length(editDistances)==0){
                        stop(sprintf("%s does not match any of %s",effect_size,paste(unique(x$effect_size), collapse = ", ")))
                } else {
                        effect_size <- names(editDistances)
                }
        }

        x <- x[which(x$effect_size==effect_size),]

        # Load default arguments from x where possible


        if(is.null(label_effect)) {
                label_effect <- effect_size
        }
        if(is.null(label_effect)) {
                label_effect <-  "Effect Size"
        }

        if(is.null(effect_neutral_point)) {
                effect_neutral_point <- unique(x$neutral_point)
        }


        if(is.null(pairs_tie)) {
                pairs_tie <- unique(x$tie_handling)
        }
        if(!is.null(pairs_tie)) if(is.na(pairs_tie)) pairs_tie <- NULL

        # Create level ranking order variable
        # This is used to help rearrange things later on
        x <- cbind(level = 1:nrow(x), x)

        ### Variables for plots -----------------------
        # Set parameters for plot

        outcome_label <- sprintf("Outcome %s", tolower(label_effect))
        cumulative_label <- sprintf("Cumulative %s", tolower(label_effect))

        # Create a list of outcome names to use later in plot (reverse order given the plot axis is reversed)
        level_label_names_list <- rev(as.list(x$outcome_names))

        # Put dataframe in long form to generate proportions of wins/ties/losses etc.
        df_long <- do.call("rbind",lapply(c("loss_inherit","loss","tie_cumulative","win","win_inherit"), function(i){
                out <-  x[,c("level","outcome_names",i)]
                colnames(out)[3] <- "n"
                out$pairType <- i
                out
        }))
        df_long <- df_long[which(!is.na(df_long$n)),]

        # Reorder long form dataframe
        df_long <- df_long[order(df_long$level),]

        # Make pairtype a factor
        df_long$pairType <- factor(df_long$pairType,
                                   levels = c("loss_inherit", "loss", "tie_cumulative", "win", "win_inherit"),
                                   labels = c("Inherited losses", "Losses", "Ties", "Wins", "Inherited wins"))
        df_long$pairType <- droplevels(df_long$pairType)

        wlt_pallette <- c(
                "Inherited losses" = pairs_loss_inherited.fill,
                "Losses" = pairs_loss.fill,
                "Ties" = pairs_neutral.fill,
                "Wins" = pairs_win.fill,
                "Inherited wins" = pairs_win_inherited.fill
        )[levels(df_long$pairType)]

        # Create a proportion variable for each level
        df_long$p <- df_long$n
        for(i in unique(df_long$level)){
                df_long[df_long$level == i,"p"] <- df_long[df_long$level == i,"p"]/sum(df_long[df_long$level == i,"p"])
        }

        df_long <- do.call("rbind",by(df_long,df_long$level,function(df_rb){
                df_rb$p_prev <- ((cumsum(df_rb$p)-df_rb$p))
                df_rb
        }))


        # Set box width variable
        df_long$width <- ifelse(df_long$level==max(df_long$level), pairs_overall.width, pairs_component.width)

        ### Proportional stacked bar graph -----------------------
        # Win ratio plot with separate overall line
        wr_box_overall2 <- ggplot2::ggplot(df_long) +
                ggplot2::geom_rect(
                        ggplot2::aes(
                                xmin=level-width/2,
                                xmax=level+width/2,
                                ymin=p_prev,
                                ymax=p_prev+p,
                                fill=pairType
                        ),
                        color="black",
                ) +
                ggplot2::scale_x_reverse(
                        breaks=c(max(df_long$level):1),
                        labels=level_label_names_list,
                        minor_break=NULL,
                        limits=c(
                                max(df_long$level)+pairs_overall.width/2,
                                pairs_component.width/2
                        )
                ) +
                ggplot2::scale_fill_manual(
                        values = wlt_pallette
                ) +
                ggplot2::guides(
                        fill = ggplot2::guide_legend(
                                title = "Treatment comparison",
                                title.theme = ggplot2::element_text(face = "bold")
                        )
                ) +
                ggplot2::coord_flip() +
                ggplot2::ylab(sprintf("Proportion (%s vs %s)", label_intervention, label_control)) +
                ggplot2::theme_bw() +
                ggplot2::theme(
                        panel.grid.major.y = ggplot2::element_blank(),
                        panel.grid.minor.y = ggplot2::element_blank()
                ) +
                ggplot2::theme(axis.title.x = ggplot2::element_text(face="bold"))

        ### Tie handling -----------------------
        if(!is.null(pairs_tie)){

                #### Drop ties -----------------------
                if(pairs_tie=="drop"){
                        # drop: ties_start + losses/(wins+losses)*ties
                        dash_ties_start <- unname(unlist(
                                df_long[
                                        df_long$level==max(df_long$level) & df_long$pairType=="Ties",
                                        "p_prev"
                                ]
                        ))

                        dash_wins <- unname(unlist(
                                df_long[
                                        df_long$level==max(df_long$level) & df_long$pairType=="Wins",
                                        "p"
                                ]
                        ))

                        dash_losses <- unname(unlist(
                                df_long[
                                        df_long$level==max(df_long$level) & df_long$pairType=="Losses",
                                        "p"
                                ]
                        ))

                        dash_ties <- unname(unlist(
                                df_long[
                                        df_long$level==max(df_long$level) & df_long$pairType=="Ties",
                                        "p"
                                ]
                        ))

                        dash_x <- max(df_long$level)-pairs_overall.width/2
                        dash_xend <- max(df_long$level)+pairs_overall.width/2
                        y_dash <- dash_ties_start + dash_losses/(dash_losses+dash_wins)*dash_ties

                        wr_box_overall2 <- wr_box_overall2 +
                                ggplot2::annotate(
                                        "segment",
                                        y=y_dash,
                                        yend=y_dash,
                                        x=dash_x,
                                        xend=dash_xend,
                                        linetype=pairs_tie.linetype,
                                        color = pairs_tie.color
                                ) +
                                ggplot2::xlab(NULL)
                }

                ### Split ties -----------------------
                if(pairs_tie=="split"){

                        # split: ties_start + 0.5*ties
                        dash_ties_start <- unname(unlist(
                                df_long[
                                        df_long$level==max(df_long$level) & df_long$pairType=="Ties",
                                        "p_prev"
                                ]
                        ))

                        dash_ties <- unname(unlist(
                                df_long[
                                        df_long$level==max(df_long$level) & df_long$pairType=="Ties",
                                        "p"
                                ]
                        ))

                        dash_x <- max(df_long$level)-pairs_overall.width/2
                        dash_xend <- max(df_long$level)+pairs_overall.width/2
                        y_dash <- dash_ties_start + 0.5*dash_ties

                        wr_box_overall2 <- wr_box_overall2 +
                                ggplot2::annotate(
                                        "segment",
                                        y=y_dash,
                                        yend=y_dash,
                                        x=dash_x,
                                        xend=dash_xend,
                                        linetype=pairs_tie.linetype,
                                        color = pairs_tie.color
                                ) +
                                ggplot2::xlab(NULL)
                }

        } # End if !is.na(pairs_tie)

        # Create a long format of the dataframe with just the data for the forest plot
        drop <- c("win_inherit", "win", "tie", "loss", "loss_inherit")
        df_forest_wide <- x[ , !(names(x) %in% drop)]


        df_forest <- do.call("rbind",lapply(c("_individual","_cumulative"),function(i){

                if(i=="_cumulative"){
                        varNames <- c("estimate_cumulative", "lower_cumulative", "upper_cumulative")
                } else {
                        varNames <- c("estimate_individual", "lower_individual", "upper_individual")
                }

                out <- df_forest_wide[,c("level","outcome_names",varNames)]
                colnames(out)[3:5] <- c("estimate","ci_lower","ci_upper")

                out$outcome_type <- i
                out
        }))

        # Reorder long form dataframe
        df_forest <- df_forest[order(df_forest$level),]

        df_forest$level_offset <- ifelse(df_forest$outcome_type == "_individual",
                                         df_forest$level-effect_cumulative.dodge_width,
                                         df_forest$level+effect_cumulative.dodge_width)
        df_forest$outcome_type <- factor(df_forest$outcome_type,
                                         levels = c("_individual", "_cumulative"),
                                         labels = c(outcome_label, cumulative_label))


        # We need the final outcome to draw the final overall estimate as a diamond later on.
        # Extract this information here
        diamond_df = data.frame(
                x_coord=c(max(df_forest$level)-effect_cumulative_polygon.height/2, max(df_forest$level),
                          max(df_forest$level)+effect_cumulative_polygon.height/2, max(df_forest$level)
                ),
                y_coord=c(unname(unlist(df_forest[df_forest$level==max(df_forest$level) &df_forest$outcome_type==outcome_label, "estimate"])),
                          unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==outcome_label, "ci_upper"])),
                          unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==outcome_label, "estimate"])),
                          unname(unlist(df_forest[df_forest$level==max(df_forest$level) & df_forest$outcome_type==outcome_label, "ci_lower"]))
                )
        )



        ### Forest plot -----------------------
        # Win ratio forest plot with overall level as separate diamond and cumulative win ratio

        wr_forest_overall_diamond_cum2 <- ggplot2::ggplot(df_forest)

        if(!is.null(effect_neutral_point)){
                # Create dataframe with info to make the green and red background panels for forest plot
                background_colourpanel <- data.frame(favours_int=c(0,1),
                                                     xmin=c(Inf,Inf),
                                                     xmax=c(-Inf,-Inf),
                                                     ymin=c( ifelse(effect_scale_log,0,-Inf), effect_neutral_point),
                                                     ymax=c(effect_neutral_point,Inf))

                background_colourpanel$favours_int <- factor(background_colourpanel$favours_int,
                                                             levels = c(0,1),
                                                             labels = c(sprintf("Favours %s", label_control),
                                                                        sprintf("Favours %s", label_intervention)
                                                             )
                )

                wr_forest_overall_diamond_cum2 <- wr_forest_overall_diamond_cum2 +
                        ggplot2::geom_rect(data=background_colourpanel,
                        ggplot2::aes(
                              xmin=xmin,
                              xmax=xmax,
                              ymin=ymin,
                              ymax=ymax,
                              fill=favours_int
                      ),
                      alpha=effect_background.alpha) +
                        ggplot2::scale_fill_manual(
                                values=c(effect_background_loss.fill,
                                         effect_background_win.fill)
                        )+
                        ggplot2::geom_hline(yintercept=effect_neutral_point,color=effect_neutral_line.color)
        }



        # Only show path for the cumulative effect, this is the thing we care about
        if(!is.na(effect_cumulative.linetype)){
                wr_forest_overall_diamond_cum2 <- wr_forest_overall_diamond_cum2+
                        ggplot2::geom_line(data=df_forest[
                                df_forest$level<max(x$level) &
                                        df_forest$outcome_type == cumulative_label,
                        ],
                        ggplot2::aes(
                                x=level_offset,
                                y=estimate
                        ),
                        color=effect_cumulative.color,
                        linetype=effect_cumulative.linetype
                        )
        }


        # Show error bars and points for both cumulative and individual estimates
        wr_forest_overall_diamond_cum2 <- wr_forest_overall_diamond_cum2+
        ggplot2::geom_errorbar(data = df_forest[
                                        df_forest$level<max(df_forest$level),
                                      ],
                                  ggplot2::aes(
                                               ymin=ci_lower,
                                               ymax=ci_upper,
                                               x=level_offset,
                                               color=outcome_type
                                               ),
                               width=0
        ) +
        ggplot2::geom_point(data = df_forest[
                                        df_forest$level<max(df_forest$level),
                                   ],
                            ggplot2::aes(
                                    y=estimate,
                                    shape=outcome_type,
                                    x=level_offset,
                                    color=outcome_type),
                            size=3
        )

        wr_forest_overall_diamond_cum2 <- wr_forest_overall_diamond_cum2+
        ggplot2::geom_polygon(
                data = diamond_df,
                ggplot2::aes(x=x_coord, y=y_coord),group=1,
                fill = effect_cumulative.color
        )


        wr_forest_overall_diamond_cum2 <- wr_forest_overall_diamond_cum2+
        ggplot2::scale_x_reverse(
                breaks=NULL,
                limits=c(
                        max(df_forest$level)+pairs_overall.width/2,
                        pairs_component.width/2
                )
        ) +
                ggplot2::xlab(NULL) +
                ggplot2::ylab(label_effect) +
                ggplot2::guides(
                        color = ggplot2::guide_legend(reverse = FALSE, title = NULL),
                        shape = ggplot2::guide_legend(title = NULL),
                        fill = ggplot2::guide_legend(title = NULL)
                ) +
                ggplot2::scale_color_manual(values=c(effect_individual_color, effect_cumulative.color)) +
                ggplot2::scale_shape_manual(values=c(4,16)) +
                ggplot2::coord_flip() +
                ggplot2::theme_bw() +
                ggplot2::theme(axis.title.x = ggplot2::element_text(face="bold"))


        if(effect_scale_log){
                wr_forest_overall_diamond_cum2 <- wr_forest_overall_diamond_cum2 + ggplot2::scale_y_log10()
        } else {
                wr_forest_overall_diamond_cum2 <- wr_forest_overall_diamond_cum2 + ggplot2::scale_y_continuous()
        }

        ### Patching plots together -----------------------
        patchwork::wrap_plots(wr_box_overall2, wr_forest_overall_diamond_cum2) + patchwork::plot_layout(guides = 'collect')

}
