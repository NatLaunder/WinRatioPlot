test_that("wins gives the same results as WINS::win.stat", {

  this_data <- winnify::tte_longform
  this_data$arm <- factor(this_data$arm,levels = c("B","A"))
  wins_result <- wins(survival::Surv(time,event_1) +
                            survival::Surv(time,event_2) +
                            survival::Surv(time,event_3) ~
                            w_arm(arm) + w_time(time) + w_id(id) + Z1 + Z2,
                          data=this_data
  )

  res_tte_ipcw <- WINS::win.stat(data = WINS::data_tte,
    ep_type = "tte", arm.name = c("A","B"),
    Z_t_trt = WINS::Z_t_trt,
    Z_t_con = WINS::Z_t_con,
    priority = c(1:3), alpha = 0.05, digit = 3,
    method = "covipcw",
    stratum.weight = "unstratified",
    pvalue = "two-sided",
    summary.print = FALSE)

  testthat::expect_equal(
  wins_result$estimate[wins_result$effect_size == "Win Ratio"],
  unname(res_tte_ipcw$Win_statistic$Win_Ratio["WR"])
  )
  
})


test_that("wins gives the same results if input rows are reordered", {

  this_data <- winnify::tte_longform
  this_data$arm <- factor(this_data$arm,levels = c("B","A"))
  wins_result <- wins(survival::Surv(time,event_1) +
                            survival::Surv(time,event_2) +
                            survival::Surv(time,event_3) ~
                            w_arm(arm) + w_time(time) + w_id(id) + Z1 + Z2,
                          data=this_data
  )

  wins_result2 <- wins(survival::Surv(time,event_1) +
                            survival::Surv(time,event_2) +
                            survival::Surv(time,event_3) ~
                            w_arm(arm) + w_time(time) + w_id(id) + Z1 + Z2,
                          data=this_data[nrow(this_data):1,]
  )

  testthat::expect_equal(
  wins_result$estimate[wins_result$effect_size == "Win Ratio"],
  wins_result2$estimate[wins_result$effect_size == "Win Ratio"],
  )

})

test_that("longform data and two-dataset formats give same result on test data",{

  this_data <- winnify::tte_longform
  this_data$arm <- factor(this_data$arm,levels = c("B","A"))
  result_longform <- wins(survival::Surv(time,event_1) +
                            survival::Surv(time,event_2) +
                            survival::Surv(time,event_3) ~
                            w_arm(arm) + w_time(time) + w_id(id) + Z1 + Z2,
                          data=this_data
  )

  this_data_outcomes <- winnify::tte_outcomes
  this_data_outcomes$arm <- factor(this_data_outcomes$arm,levels = c("B","A"))
  this_data_covariates <- winnify::tte_covariates

  result_twoform <- wins(survival::Surv(Y_1,Delta_1) +
       survival::Surv(Y_2,Delta_2) +
       survival::Surv(Y_3,Delta_3) ~
       w_arm(arm) + w_time(time) + w_id(id) + Z1 + Z2,
      data=this_data_outcomes,data_covariates = this_data_covariates
    )

  expect_equal(
    result_longform$estimate[result_longform$effect_size=="Win Ratio"],
    result_twoform$estimate[result_twoform$effect_size=="Win Ratio"]
  )
  
})