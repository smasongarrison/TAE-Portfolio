make_condition_trials <- function(condition_label, n_trials, n_correct, rt_mean, rt_sd) {
  tibble(
    sing_prob_clean = condition_label,
    acc = sample(c(rep(1, n_correct), rep(0, n_trials - n_correct))),
    rt = rnorm(n_trials, mean = rt_mean, sd = rt_sd)
  )
}
sample_first_item <- function(sing_prob_clean) {
  if (is.na(sing_prob_clean) || sing_prob_clean == "Absent") {
    sample(
      c("Target", "Nonsingleton"),
      1,
      prob = c(.715, .285)
    )
  } else if (sing_prob_clean == "High") {
    sample(
      c("Target", "Singleton", "Nonsingleton"),
      1,
      prob = c(.486, .335, .180)
    )
  } else {
    sample(
      c("Target", "Singleton", "Nonsingleton"),
      1,
      prob = c(.407, .417, .176)
    )
  }
}

sample_later_item <- function(sing_prob_clean) {
  if (is.na(sing_prob_clean) || sing_prob_clean == "Absent") {
    sample(
      c("Target", "Nonsingleton", "Center/Other"),
      1,
      prob = c(.55, .35, .10)
    )
  } else {
    sample(
      c("Target", "Singleton", "Nonsingleton", "Center/Other"),
      1,
      prob = c(.45, .20, .25, .10)
    )
  }
}

choose_fake_loc <- function(curr_item, targ_loc, sing_loc) {
  if (curr_item == "Target") {
    targ_loc
  } else if (curr_item == "Singleton") {
    sing_loc
  } else if (curr_item == "Nonsingleton") {
    sample(setdiff(1:set_size, c(targ_loc, sing_loc)), 1)
  } else {
    NA_integer_
  }
}