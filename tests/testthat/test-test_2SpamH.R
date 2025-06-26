data("example_data")

passive_variable <- "step_count"
activity_level_vars <- "n_uploads"
phone_usage_vars <- c("screen_unlocks","display_events")
all_vars <- c(passive_variable, activity_level_vars, phone_usage_vars)

test_that("TwoSpamH works", {
  a <- TwoSpamH(
    data = example_data,
    passive_variable = passive_variable,
    phone_usage_vars = phone_usage_vars,
    activity_level_vars = activity_level_vars,
    thresholds = data.frame(
      lower_bound_phone_usage = 0.2,
      upper_bound_phone_usage = 0.8,
      lower_bound_activity_level = 0.3,
      upper_bound_activity_level = 0.7
    ),
    plot.data = FALSE
  )[86, ]
  
  expect_equal(a$step_count, 4664)
  expect_equal(a$label, "Non Missing")
})

test_that("Training works", {
  b <- TwoSpamH_train(
    data = example_data,
    passive_variable = passive_variable,
    phone_usage_vars = phone_usage_vars,
    activity_level_vars = activity_level_vars,
    num.neighbor = 5,
    plot.data = FALSE
  )[88, ]
  
  expect_equal(b$step_count, 2174)
  expect_equal(b$label, "Non Missing")
})

test_that("Online 2SpamH works", {
  train <- TwoSpamH_train(
    data = example_data,
    passive_variable = passive_variable,
    phone_usage_vars = phone_usage_vars,
    activity_level_vars = activity_level_vars,
    plot.data = FALSE
  )
  
  new_data <- data.frame(
    step_count = 3400,
    n_uploads = 90,
    screen_unlocks = 25,
    display_events = 130
  )
  
  c <- Online_TwoSpamH(
    new_data = new_data,
    training_data = train,
    passive_variable = passive_variable,
    phone_usage_vars = phone_usage_vars,
    activity_level_vars = activity_level_vars,
    plot.data = FALSE
  )
  
  expect_equal(c$label, "Non Missing")
})

