#' Identify patient ID variable
#'  
#' @description This is a special function used by winnify and supplied wrapper functions.
#' It identifies which variable corresponds to the patient ID 
#'
#' @usage w_id(x)
#' @param x A term in a formula
#' 
#' @details
#' When used outside of a formula, this is an identity function.
#' 
#' @returns
#' Whatever was supplied in \code{x}
w_id <- function(x){x}

#' Identify group variable
#'  
#' @description This is a special function used by winnify and supplied wrapper functions.
#' It identifies which variable corresponds to the group
#'
#' @usage w_arm(x)
#' @param x A term in a formula
#' 
#' @details
#' When used outside of a formula, this is an identity function.
#' 
#' @returns
#' Whatever was supplied in \code{x}
w_arm <- function(x){x}

#' Identify stratum variable
#'  
#' @description This is a special function used by winnify and supplied wrapper functions.
#' It identifies which variable corresponds to the strata
#'
#' @usage w_stratum(x)
#' @param x A term in a formula
#' 
#' @details
#' When used outside of a formula, this is a identity function.
#' 
#' @returns
#' Whatever was supplied in \code{x}
w_stratum <- function(x){x}

#' Identify time variable
#'  
#' @description This is a special function used by winnify and supplied wrapper functions.
#' It identifies which variable corresponds to time
#'
#' @usage w_time(x)
#' @param x A term in a formula
#' 
#' @details
#' When used outside of a formula, this is a identity function.
#' 
#' @returns
#' Whatever was supplied in \code{x}
w_time <- function(x){x}



#' Identify IPTW variable
#'  
#' @description This is a special function used by winnify and supplied wrapper functions.
#' It identifies which variable corresponds to inverse probability of treatment weight
#'
#' @usage w_iptw(x)
#' @param x A term in a formula
#' 
#' @details
#' When used outside of a formula, this is a identity function.
#' 
#' @returns
#' Whatever was supplied in \code{x}
w_iptw <- function(x){x}