.get_coef_confint <- function(x){
 
    # NOTE: if calculating confidence intervals requires
    # profiling to occur, this will break. See the following:
    # https://stackoverflow.com/questions/39151409/potential-bug-in-rs-polr-function-when-run-from-a-function-environment
  

    # We can pass this off to broom, but I'm pretty sure
    # that it'll just be adding a dependency for no good reason
    # suppressMessages({
    #     has_tidy_method <- any(paste0("tidy.", class(x)) %in% methods(broom::tidy))
    # })
    # if(has_tidy_method){    
    #    result <- tryCatch({
    #      this_tidy <- broom::tidy(x,conf.int=TRUE)      
    #      coef <- this_tidy$estimate
    #      names(coef) <- this_tidy$term
    #      confint <- result[,c("conf.low","conf.high")]
    #      rownames(confint) <- this_tidy$term
    #      list(
    #         coef=confint,
    #         confint=confint,
    #      )         
    #    }, error=function(e){e})
    #    if(!("error" %in% class(result))) return(result)
    # }
    
  
    # We want to evaluate coef(x) and confint(x) 
    # reliably, even if the namespace that created
    # x isn't loaded.

    # If x is an S4 object this is easy

    # If matching S4 methods were found, get the result 
    # Otherwise, attempt to fall back to S3 methods

    if(isS4(x)){
       def <- sloop::s4_methods_class(class(x))               

       # If we fail to find coef or confint, just mark it as NULL
       # and attempt an S3 method call.

       function_coef <- tryCatch(getExportedValue(def$source, "coef"), error=function(e){NULL})
       function_confint <- tryCatch(getExportedValue(def$source, "confint"), error=function(e){NULL})
    } else {
        # Check if there's a registered S3 generic
        # function_coef <- tryCatch({getS3method("coef",class(x))},error=function(e){NULL}) 
        # function_confint <- tryCatch({getS3method("confint",class(x))},error=function(e){NULL})
        function_coef <- function_confint <- NULL
    }

    # As a fallback, check if stats::coef() can do it.

    # The default message for a failure state here is going to be
    # awful to try and understand, so add some additional context
    # about what went wrong

    if(!is.null(function_coef)){
        result_coef <- function_coef(x)
    } else {
        result_coef <- tryCatch(stats::coef(x),error=function(e){e})
        if("error" %in% class(result_coef)){
            stop(sprintf("Error when attempting to evaluate coef.%s(): %s",class(x),result_coef$message))
        }
    }
    
    if(!is.null(function_confint)){
        result_confint <- function_confint(x)
    } else {
        result_confint <- tryCatch(stats::confint(x),error=function(e){e})
        if("error" %in% class(result_confint)){          
            browser()
            stop(sprintf("Error when attempting to evaluate confint.%s(): %s",class(x),result_confint$message))
        }
    }

    return(
        list(
            coef = result_coef,
            confint = result_confint
        )
    )
}
