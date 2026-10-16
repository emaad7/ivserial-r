test_that("API and reproducibility", {
  set.seed(0)
  x <- rnorm(400)
  a <- serial_test(x, B = 499, seed = 3)
  b <- serial_test(x, B = 499, seed = 3)
  expect_identical(a$p_C, b$p_C)
  expect_true(a$p_C > 0 && a$p_C <= 1)
  expect_equal(c(m = a$m, d = a$d), choose_resolution(400))
  expect_s3_class(as.data.frame(a), "data.frame")
  expect_output(print(a), "combined test C")
})

test_that("the session's random numbers are not changed by seed", {
  set.seed(5); u1 <- runif(1)
  set.seed(5); invisible(serial_test(rnorm(50), B = 9, seed = 1)); u2 <- runif(1)
  set.seed(5); invisible(rnorm(50)); u3 <- runif(1)
  expect_identical(u2, u3)
})

test_that("size under independence", {
  set.seed(1)
  rej <- vapply(1:200, function(i) serial_test(rt(200, 3), B = 199, seed = i)$p_C <= 0.05, logical(1))
  expect_true(mean(rej) >= 0.01 && mean(rej) <= 0.10)
})

test_that("power against ARCH", {
  set.seed(2)
  n <- 1000; e <- rnorm(n + 200); z <- numeric(n + 200)
  for (t in 2:(n + 200)) z[t] <- sqrt(1 + 0.8 * z[t - 1]^2) * e[t]
  r <- serial_test(z[201:(n + 200)], B = 499, seed = 1)
  expect_true(r$reject)
  expect_gt(r$gamma_hat, 0.05)
})

test_that("statistics agree with the Python package", {
  fx <- read_series(system.file("extdata", "exchange_rates.csv", package = "ivserial"))
  expect_equal(dim(fx), c(940L, 20L))
  r <- serial_test(fx$China, transform = "logdiff", B = 99, seed = 1)
  expect_equal(r$Q, 380.3692636073, tolerance = 1e-9)
  expect_equal(r$P2, 170.3493320105, tolerance = 1e-9)
  expect_equal(r$gamma_hat, 0.2864133016, tolerance = 1e-8)
  expect_equal(r$tau_hat, 2.9425266878, tolerance = 1e-8)
  expect_equal(r$cramer_v, 0.1092423341, tolerance = 1e-8)
  expect_equal(r$cells$pattern[1], "3-3-3")
  expect_equal(r$cells$observed[1], 41)
})

test_that("null limit and planning", {
  expect_lt(abs(null_limit_sf(33.76, 3, 3) - 0.05), 0.002)
  expect_equal(null_limit_sf(qchisq(0.95, 4), 2, 3), 0.05, tolerance = 1e-8)
  expect_equal(null_limit_sf(null_limit_quantile(0.01, 3, 4), 3, 4), 0.01, tolerance = 1e-6)
  expect_gt(required_sample_size(0.05, m = 3, d = 4), required_sample_size(0.2, m = 3, d = 4))
  expect_equal(approximate_power(0.13, n = 350, m = 3, d = 4, tau = 1.13), 0.8386065959, tolerance = 1e-8)
  expect_equal(gamma_from_cramer_v(serial_cramer_v(0.13, 3, 4), 3, 4), 0.13)
  expect_output(print(null_limit_summary(3, 4)), "critical value")
})

test_that("screening and reading", {
  sc <- screen_series(exchange_rates[, 2:4], transform = "logdiff", B = 99, seed = 1)
  expect_equal(nrow(sc$table), 3)
  expect_true(all(sc$table$p_adj >= sc$table$p_C))
  f <- tempfile(fileext = ".csv")
  writeLines(c("a;b", "1,5;2", "2,5;7", "3,0;1"), f)
  d <- read_series(f)
  expect_equal(d$a, c(1.5, 2.5, 3))
  expect_error(serial_test(1:10), "at least 20")
})

test_that("the graphical interface can be built", {
  skip_if_not_installed("shiny")
  expect_s3_class(ivserial:::ivserial_app_object(), "shiny.appobj")
})
