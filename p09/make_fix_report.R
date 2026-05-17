item_locations <- tibble(
  item_loc = 1:set_size,
  angle = (item_loc - 1) * 2 * pi / set_size,
  item_x = center_x + eccentricity * sin(angle),
  item_y = center_y - eccentricity * cos(angle)
)

participants_fake <- tibble(
  subjnum = 1:n_participants,
  age = sample(18:25, n_participants, replace = TRUE),
  exp_vers = 3,
  gender = sample(c("F", "M"), n_participants, replace = TRUE),
  hand = sample(c("R", "L"), n_participants, replace = TRUE, prob = c(.85, .15)),
  recording_session_label = paste0("SS_", subjnum)
)

experiment_trials <- bind_rows(
  make_condition_trials(
    condition_label = "Absent",
    n_trials = 7131,
    n_correct = 6792,
    rt_mean = 995,
    rt_sd = 250
  ),
  make_condition_trials(
    condition_label = "High",
    n_trials = 5319,
    n_correct = 4976,
    rt_mean = 1120,
    rt_sd = 275
  ),
  make_condition_trials(
    condition_label = "Low",
    n_trials = 1766,
    n_correct = 1643,
    rt_mean = 1147,
    rt_sd = 290
  )
) %>%
  mutate(
    phase = "experiment",
    rt = pmin(pmax(rt, 220), 2400)
  )

practice_trials <- tibble(
  sing_prob_clean = NA_character_,
  acc = rbinom(n_trial_units - nrow(experiment_trials), 1, .95),
  rt = pmin(pmax(rnorm(n_trial_units - nrow(experiment_trials), 950, 250), 220), 2400),
  phase = "practice"
)

fake_trials <- bind_rows(practice_trials, experiment_trials) %>%
  mutate(
    trial_unit = row_number(),
    subjnum = rep(1:n_participants, length.out = n()),
    block = rep(1:n_blocks, length.out = n())
  ) %>%
  group_by(subjnum) %>%
  mutate(trial = row_number()) %>%
  ungroup() %>%
  left_join(participants_fake, by = "subjnum") %>%
  mutate(
    targ_loc = sample(1:set_size, n(), replace = TRUE),
    sing_loc = map_int(targ_loc, ~ sample(setdiff(1:set_size, .x), 1)),
    sing_pres = case_when(
      phase == "practice" ~ "A",
      sing_prob_clean == "Absent" ~ "A",
      sing_prob_clean %in% c("High", "Low") ~ "P"
    ),
    sing_prob = case_when(
      sing_prob_clean == "High" ~ "high",
      sing_prob_clean == "Low" ~ "low",
      TRUE ~ NA_character_
    ),
    condition = case_when(
      sing_pres == "A" ~ "A",
      sing_prob == "high" ~ "high",
      sing_prob == "low" ~ "low"
    ),
    targ_shape = sample(c("circle", "diamond"), n(), replace = TRUE),
    targ_id = sample(c("H", "V"), n(), replace = TRUE),
    resp = if_else(
      acc == 1,
      targ_id,
      if_else(targ_id == "H", "V", "H")
    ),
    sing_col = sample(c("green", "red", "blue", "yellow"), n(), replace = TRUE)
  )

# Create fixation counts so that the final fake fixation report has exactly
# 65,876 rows, matching the knitted p09 output.
n_fixations_per_trial <- pmax(1, rpois(n_trial_units, lambda = 3.35) + 1)

difference <- n_rows_target - sum(n_fixations_per_trial)

if (difference > 0) {
  add_to <- sample(seq_along(n_fixations_per_trial), difference, replace = TRUE)
  n_fixations_per_trial <- n_fixations_per_trial + tabulate(add_to, nbins = n_trial_units)
}

if (difference < 0) {
  for (i in seq_len(abs(difference))) {
    eligible_trials <- which(n_fixations_per_trial > 1)
    reduce_trial <- sample(eligible_trials, 1)
    n_fixations_per_trial[reduce_trial] <- n_fixations_per_trial[reduce_trial] - 1
  }
}

fake_trials <- fake_trials %>%
  mutate(n_fixations = n_fixations_per_trial)

fix_raw <- fake_trials %>%
  uncount(n_fixations, .id = "current_fix_index") %>%
  mutate(
    curr_item = map2_chr(
      sing_prob_clean,
      current_fix_index,
      ~ if (.y == 1) {
        sample_first_item(.x)
      } else {
        sample_later_item(.x)
      }
    ),
    curr_loc = pmap_int(
      list(curr_item, targ_loc, sing_loc),
      choose_fake_loc
    )
  ) %>%
  left_join(item_locations, by = c("curr_loc" = "item_loc")) %>%
  mutate(
    mean_x = if_else(is.na(curr_loc), center_x, item_x),
    mean_y = if_else(is.na(curr_loc), center_y, item_y),
    fixation_sd = case_when(
      curr_item == "Target" ~ 55,
      curr_item == "Singleton" ~ 55,
      curr_item == "Nonsingleton" ~ 60,
      curr_item == "Center/Other" ~ 140
    ),
    current_fix_x = rnorm(n(), mean = mean_x, sd = fixation_sd),
    current_fix_y = rnorm(n(), mean = mean_y, sd = fixation_sd),
    current_fix_x = pmin(pmax(current_fix_x, 0), screen_width),
    current_fix_y = pmin(pmax(current_fix_y, 0), screen_height),
    current_fix_duration = case_when(
      sing_prob_clean == "Absent" & curr_item == "Target" ~ rnorm(n(), 279, 55),
      sing_prob_clean == "High" & curr_item == "Target" ~ rnorm(n(), 287, 55),
      sing_prob_clean == "Low" & curr_item == "Target" ~ rnorm(n(), 296, 55),
      curr_item == "Singleton" ~ rnorm(n(), 158, 35),
      curr_item == "Nonsingleton" ~ rnorm(n(), 156, 35),
      TRUE ~ rnorm(n(), 130, 35)
    ),
    current_fix_duration = pmax(round(current_fix_duration), 20)
  ) %>%
  group_by(trial_unit) %>%
  arrange(current_fix_index, .by_group = TRUE) %>%
  mutate(
    fixation_gap = if_else(
      current_fix_index == 1,
      0L,
      sample(14:44, n(), replace = TRUE)
    ),
    current_fix_start = cumsum(lag(current_fix_duration + fixation_gap, default = 0)),
    current_fix_end = current_fix_start + current_fix_duration - 2,
    previous_fix_end = lag(current_fix_end),
    previous_sac_start_time = if_else(
      current_fix_index == 1,
      NA_real_,
      current_fix_start - sample(10:30, n(), replace = TRUE)
    ),
    previous_sac_end_time = if_else(
      current_fix_index == 1,
      NA_real_,
      current_fix_start - sample(1:9, n(), replace = TRUE)
    )
  ) %>%
  ungroup() %>%
  select(
    subjnum,
    block,
    trial,
    acc,
    rt,
    sing_pres,
    targ_loc,
    sing_loc,
    previous_sac_start_time,
    current_fix_x,
    current_fix_y,
    current_fix_index,
    current_fix_duration,
    current_fix_end,
    previous_sac_end_time,
    targ_shape,
    targ_id,
    sing_col,
    resp,
    age,
    exp_vers,
    gender,
    hand,
    recording_session_label,
    condition,
    phase,
    sing_prob,
    previous_fix_end,
    current_fix_start
  )

remove(fake_trials)
remove(practice_trials)
remove(item_locations)
remove(participants_fake)
