test_that("ranks gives the same result as a direct input",{

  this_data <- winnify::remapcap

  this_data$days_alive <- this_data$deathTime
  this_data$days_out_of_icu <- 21-this_data$dischargeTime
  this_data$group <- factor(this_data$group,levels = c("uc","shock"))
  this_data <- this_data[which(!is.na(this_data$group)),]

  ranks_fit <- ranks(days_alive ~ w_arm(group),
        data = this_data,
        ranks_statistic = MASS::polr, Hess=TRUE)

  raw_fit <- MASS::polr(factor(days_alive) ~ w_arm(group),
        data = this_data, Hess=TRUE)

  expect_equal(
          ranks_fit$estimate,
          unname(coef(raw_fit))
  )
})


