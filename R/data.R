#' A reconstructed dataset from the REMAP-CAP clinical trial by Angus et al. (2020)
#'
#' @format A data frame with 379 rows and 6 variables:
#' \describe{
#'   \item{group}{Type of treatment received}
#'   \item{deathTime}{Time to death in days}
#'   \item{dischargeTime}{Time to discharge in days}
#'   \item{deathStatus}{If patient died within 21 days of follow-up (1=Yes; 0=No)}
#'   \item{dischargeStatus}{If patient was discharged within 21 days of follow-up (1=Yes; 0=No)}
#'   \item{score}{Number of organ support-free days (if patient died, assigned value of -1)}
#' }
#'
#' @references
#' Angus, Derek C., et al. "Effect of hydrocortisone on mortality and organ support in patients with severe COVID-19: the REMAP-CAP COVID-19 corticosteroid domain randomized clinical trial." Jama 324.13 (2020): 1317-1329.
#'
"remapcap"

#' Synthetic data showing modified Rankin Scale and neurological improvement
#'
#' @format A data frame with 2000 rows and 3 variables:
#' \describe{
#'   \item{treat}{Type of treatment received}
#'   \item{mRS}{modified Rankin Score at 90 days (0= no disability, 6 = death)}
#'   \item{early_neuro_improvement}{Change in neurological function at 24 hours}
#' }
#'
"neuro_improvement"

#' Simulated time to event data in long form
#' 
#' @description This is a reformatted version of the simulated dataset provided by
#' \link[WINS]{data_tte}, \link[WINS]{Z_t_con} and \link[WINS]{Z_t_trt}
#' in the WINS package, with a stratification variable added.
#' It provides an example of data in time-series format.
#' The ID variable has been reworked so that each ID corresponds to a different
#' patient (i.e. there are not two patients with an ID of "1")
#' 
#' 
#' @format A data frame with 2420 obeservations on the following 9 variables:
#' \describe{
#' \item{id}{The patient ID}
#' \item{arm}{The treatment groups}
#' \item{time}{The time of observation. Used for both time-varying covariates and time to event outcomes}
#' \item{Z1}{The first covariate}
#' \item{Z2}{The second covariate}
#' \item{event_1}{Event status for the first outcome. 0=censored at this time, 1=event at this time, NA = neither}
#' \item{event_2}{Event status for the second outcome. 0=censored at this time, 1=event at this time, NA = neither}
#' \item{event_3}{Event status for the third outcome. 0=censored at this time, 1=event at this time, NA = neither}
#' \item{strata}{The stratum for each patient}
#' }
"tte_longform"


#' Simulated time to event data
#' 
#' @description This is a simulated dataset provided by
#' \link[WINS]{data_tte} in the WINS package, with a
#' stratification variable added. It provides an
#' example of outcome data in time-series format.
#' 
#' The ID variable has been reworked so that each ID corresponds to a different
#' patient (i.e. there are not two patients with an ID of "1")
#' 
#' @format A data frame with 400 obeservations on the following 9 variables:
#' \describe{
#' \item{id}{The patient ID}
#' \item{arm}{The treatment groups}
#' \item{Delta_1}{The time event or censoring for the first outcome.}
#' \item{Delta_2}{The time event or censoring for the second outcome.}
#' \item{Delta_3}{The time event or censoring for the third outcome.}
#' \item{Y_1}{Event status for the first outcome. 0=censored at this time, 1=event at this time, NA = neither}
#' \item{Y_2}{Event status for the second outcome. 0=censored at this time, 1=event at this time, NA = neither}
#' \item{Y_3}{Event status for the third outcome. 0=censored at this time, 1=event at this time, NA = neither}
#' \item{strata}{The stratum for each patient}
#' }
"tte_outcomes"


#' Simulated time-varying covariates
#' 
#' @description This is a reformatted version of the simulated datasets
#' provided by \link[WINS]{Z_t_trt} and \link[WINS]{Z_t_con} in the
#' WINS package. It provides an example of data in time-varying covariates
#' in a separate dataset to outcome variables, mimicking a common data structure
#' in clinical trials where outcomes and data collected at a single time point are
#' found in one dataset and repeating measures are found in a second dataset.
#' It is intended to be used in combination with \link{tte_outcomes}
#' 
#' The ID variable has been reworked so that each ID corresponds to a different
#' patient (i.e. there are not two patients with an ID of "1")
#'  
#' @format A data frame with 400 obeservations on the following 9 variables:
#' \describe{
#' \item{id}{The patient ID}
#' \item{time}{The time of observation}
#' \item{Z1}{The first covariate}
#' \item{Z2}{The second covariate}
#' }
"tte_covariates"