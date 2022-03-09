# TCORS Project 4 Enrollment - API Call Script
# Tony Barrows
# 2021-01-20


library(redcapAPI)
library(dplyr)


# common -------
sessions <- c()
for (i in 1:12) {
  tmp <- paste("week_", i, sep = "")
  sessions <- c(sessions, tmp)
}
sessions <- c(
  "prescreen", "screening", "baseline_1", 
  "baseline_2","abstinence", "gestational_week_2",
  "30_day_followup", "birth_outcomes"
  )

recruit_source_vars <- list(
  "recruit_1___0" = "TV",
  "recruit_1___1" = "Radio",
  "recruit_1___2" = "Metro Newspaper",
  "recruit_1___3" = "Community Newspaper",
  "recruit_1___4" = "Flyer",
  "recruit_1___5" = "Facebook",
  "recruit_1___6" = "Craigslist",
  "recruit_1___7" = "Bus Advertisement",
  "recruit_1___8" = "Participation in Other Studies",
  "recruit_1___9" = "Clinical Trials Websites",
  "recruit_1___10" = "Direct Mail",
  "recruit_1___11" = "Other Person",
  "recruit_1___12" = "Other",
  "recruit_1___13" = "Instagram",
  "recruit_1___14" = "Google",
  "recruit_1___15" = "YouTube",
  "recruit_1___16" = "Front Porch Forum",
  "recruit_1___17" = "Spotify",
  "recruit_1___18" = "TikTok",
  "recruit_1___19" = "Reddit",
  "recruit_1___20" = "BuildClinical"
)

exclusion_vars <- list(
  "sl_exclusion___1" = "did_not_consent",
  "sl_exclusion___2" = "illiterate",
  "sl_exclusion___3" = "not_pregnant",
  "sl_exclusion___4" = "morethan_25wks",
  "sl_exclusion___5" = "no_EGA_verification",
  "sl_exclusion___6" = "over_44",
  "sl_exclusion___7" = "under_21",
  "sl_exclusion___8" = "ed_too_high",
  "sl_exclusion___9" = "other_research",
  "sl_exclusion___10" = "high_co",
  "sl_exclusion___11" = "neg_cotinine",
  "sl_exclusion___12" = "cessation_aids",
  "sl_exclusion___13" = "cessation_txt",
  "sl_exclusion___14" = "quit_plans",
  "sl_exclusion___15" = "quit_past_month",
  "sl_exclusion___16" = "other_tob_use",
  "sl_exclusion___17" = "rolls_own",
  "sl_exclusion___18" = "suicide_ideation",
  "sl_exclusion___19" = "suicide_attempt",
  "sl_exclusion___20" = "psychosis",
  "sl_exclusion___21" = "nmania",
  "sl_exclusion___22" = "failed_tox_screen",
  "sl_exclusion___23" = "bp_out_of_range",
  "sl_exclusion___24" = "hr_out_of_range",
  "sl_exclusion___25" = "unstable_om_txt",
  "sl_exclusion___26" = "opted_out",
  "sl_exclusion___27" = "lmp_exclusion",
  "sl_exclusion___28" = "covid19"
)


# functions ------

# TODO IVR pull function (*.mdb)

build_rcon <- function(rc){
  # Wrapper around the redcapAPI::redcapConnection() using
  # a .csv token sheet in the working directory.
  url <- 'https://redcap.med.uvm.edu/api/'
  # import passwords document
  pw <- read.csv("./password.csv", stringsAsFactors = FALSE)
  token <- pw$password[pw$username == rc]
  redcapConnection(url = url, token = token)
}


pull_enrollment_info <- function(rcon) {
  # Pull REDCap project information to count enrolled participants
  
  enrl_fields <- c("screen_id", "sl_status", "sl_exclusion")
  
  session_dates <-c(
    "screen_date", "rescreen_date", "date_s1", "date_s2",
    "date_session_iph1", "date_session_iph2", "date_session_iph3"
  )
  
  fields <- c(enrl_fields, session_dates)
  
  exportRecords(
    rcon,
    fields = fields,
    labels = FALSE,
    survey = FALSE,
    factors = FALSE,
    dates = FALSE,
    form_complete_auto = FALSE
  ) %>%
    filter(!stringr::str_detect(screen_id, "-2"))
}

pull_prescreen_info <- function(rcon, sitename) {
  # Pull REDCap project information to count prescreens
  
  fields <- c(
    "redcap_id", "recruit_date", "recruit_1",
    "recruit_3", "recruit_3a", "recruit_4",
    "recruit_4a", "recruit_5", "recruit_6",
    "recruit_7", "recruit_7a", "recruit_8",
    "recruit_9", "recruit_10", "recruit_11",
    "recruit_12", "recruit_13", "recruit_14",
    "recruit_15", "recruit_15a", "recruit_int_summ",
    "screen_status", "screen_subjectid"
  )
  
  exportRecords(
    rcon,
    fields = fields,
    labels = FALSE,
    survey = FALSE,
    factors = FALSE,
    dates = FALSE,
    dag = FALSE,
    form_complete_auto = FALSE
  ) %>%
    mutate(site = sitename)
}


rename_matrix <- function(df) {
  # make values names of columns
  w1 <- which(df[,1:ncol(df)] == 1, arr.ind = TRUE)
  w0 <- which(df[,1:ncol(df)] == 0, arr.ind = TRUE)
  
  # avoid zero-length error
  if(length(w1 > 0)) {
    df[w1] <- names(df)[w1[,"col"]]
  }
  df[w0] <- NA
  
  df
}

reshape_recruitment_sources <- function(df, recruit_source_vars) {
  # create single column of vectors
  df_sub <- df %>% 
    plyr::rename(recruit_source_vars) %>%
    select(unlist(recruit_source_vars, use.names = FALSE)) %>%
    rename_matrix() %>%
    tidyr::unite(
      col = "recruitment_sources",
      sep = ",",
      na.rm = TRUE
    )
  df_sub[df_sub == ""] <- NA
  
  df %>%
    select(-starts_with("recruit_1")) %>%
    cbind(df_sub)
}

general_prescreen_cleaning <- function(df) {
  
  test_ids <- c("TEST")
  
  exclusion_vars <- list(
    "recruit_3" = "smoke_daily",
    "recruit_4a" = "ndays_other_tob_use",
    "recruit_5" = "nic_rep",
    "recruit_6" = "quit_med",
    "recruit_7" = "rolls_own",
    "recruit_7a" = "quit_plans",
    "recruit_8" = "used_research_cigs"
  )
  
  # recode for eligibility
  df_sub <- df %>%
    plyr::rename(exclusion_vars) %>%
    mutate(
      smoke_daily = ifelse(smoke_daily == 0, 1, 0),
      ndays_other_tob_use = ifelse(ndays_other_tob_use > 9, 1, 0),
      nic_rep = ifelse(nic_rep == 1, 1, 0),
      rolls_own = ifelse(rolls_own == 1, 1, 0),
      quit_plans = ifelse(quit_plans == 1, 1, 0),
      used_research_cigs = ifelse(used_research_cigs == 1, 1, 0)
    )
  
  # create single column of vectors
  df_sub <- df_sub %>% 
    select(unlist(exclusion_vars, use.names = FALSE)) %>%
    rename_matrix() %>%
    tidyr::unite(
      col = "ps_exclusion_reasons",
      sep = ", ",
      na.rm = TRUE
    )
  df_sub[df_sub == ""] <- NA
  
  df %>%
    rename(
      "ps_summary" = recruit_int_summ,
      "date" = recruit_date,
      "screen_id" = screen_subjectid
      ) %>%
    select(-starts_with("recruit_")) %>%
    select(-redcap_event_name) %>%
    cbind(df_sub) %>%
    mutate(
      ps_summary = redcapFactorFlip(ps_summary),
      screen_status = redcapFactorFlip(screen_status),
      ps_exclusion_reasons = ifelse(
        ps_exclusion_reasons == "", NA, ps_exclusion_reasons),
      date = as.Date(date)
      ) %>%
    filter(!stringr::str_detect(redcap_id, test_ids)) %>%
    mutate(session = "prescreen")

}

reshape_exclusion_reasons <- function(df, exclusion_vars) {
  # create single column of vectors
  df_sub <- df %>% 
    plyr::rename(exclusion_vars) %>%
    select(unlist(exclusion_vars, use.names = FALSE)) %>%
    rename_matrix() %>%
    tidyr::unite(
      col = "reasons_for_exclusion",
      sep = ",",
      na.rm = TRUE
    )
  df_sub[df_sub == ""] <- NA
  
  df %>%
    select(-starts_with("sl_exclusion")) %>%
    cbind(df_sub)
    
}

reshape_session_dates <- function(df) {
  
  # if rescreen date exists, use that
  df <- df %>% 
    mutate(
      screen_date = ifelse(
        !is.na(rescreen_date), 
        rescreen_date, 
        screen_date
        )
      ) %>%
    select(-rescreen_date)
  
  # make single column of dates
  df %>%
    tidyr::unite(
      contains("date"),
      col = "date",
      na.rm = TRUE
    ) %>%
    mutate(date = as.Date(date))

}

general_cleaning <- function(df, sessions) {
  df_sub <- df %>%
    select(
      screen_id, 
      "site" = redcap_data_access_group,
      "session" = redcap_event_name,
      everything()
      ) %>%
    select(-c(redcap_repeat_instance, redcap_repeat_instrument)) %>%
    mutate(
      sl_status = as.character(redcapFactorFlip(sl_status)),
      site = factor(site, levels = c("uvm", "uky")),
      session = stringr::str_replace_all(session, "_arm_1", ""),
      session = factor(session, levels = sessions)
      ) %>%
    filter(!is.na(site))
  
  # pt. is given product at BL2
  df_sub <- df_sub %>%
    mutate(
      randomized = ifelse(
        session == "baseline_2" & !is.na(date), "yes", NA
        )
      )
  
  # complete dataframe
  df_sub <- df_sub %>%
    group_by(screen_id) %>%
    tidyr::fill(
      sl_status, reasons_for_exclusion, randomized,
      .direction = "downup"
      ) %>%
    ungroup() %>%
    mutate(
      sl_status = ifelse(
        sl_status == "In Progress" & is.na(randomized),
        "Baseline", sl_status
      )) %>%
    mutate(sl_status = ifelse(
      sl_status == "In Progress" & randomized == "yes",
      "Experimental", sl_status)
      )
  
  # make factor again
  df_sub %>%
    mutate(sl_status = factor(sl_status, levels = c(
      "Screening Ineligible", "Screening 1 Complete",
      "Under LMP Review", "Baseline", "Experimental",
      "Withdrawn - Pre-Product", "Withdrawn - Post-Product",
      "Complete"
    )))
  
}

impose_site <- function(df) {
  # catchall to ensure all subject IDs have site indicators
  df$site <- NA
  df$site[substr(df$screen_id, 3, 3) == "A"] <- "uvm"
  df$site[substr(df$screen_id, 3, 3) == "K"] <- "uky"
  df
}

# main -------

# pull
rcon_proper <- build_rcon("rc_proper_p4")
rcon_pilot <- build_rcon("rc_pilot_p4")

rcon_ps_uvm <- build_rcon("rc_recruitment_uvm_p4")
rcon_s3_uvm <- build_rcon("rc_recruitment_uvm_s3")
rcon_ps_uky <- build_rcon("rc_recruitment_uky_p4")

# trial data
tryCatch(
  # attempt API call -- if empty, import blank data.
  expr = {
    # df_enrl_proper <- pull_enrollment_info(rcon_proper)
    df_enrl_pilot <- pull_enrollment_info(rcon_pilot)
    
    # df_enrl_proper$pi_prop <- "proper"
    df_enrl_pilot$pi_prop <- "pilot"
    
    # df_enrl <- rbind(df_enrl_proper, df_enrl_pilot)
    df_enrl <- df_enrl_pilot
    # clean
    df_enrl <- reshape_exclusion_reasons(df_enrl, exclusion_vars)
    df_enrl <- reshape_session_dates(df_enrl)
    df_enrl <- general_cleaning(df_enrl, sessions)
  },
  error = {
    load("./data/placeholder.RData")
    df_enrl <- df_placeholder
  },
  finally = {
    # load pre-remote trial data
    
    load("./data/ip.RData")
    df_enrl <- rbind(df_enrl, tmp)
  }
)

# prescreen data
tryCatch(
  expr = {
    df_ps_uvm <- pull_prescreen_info(rcon_ps_uvm, sitename = "uvm")
    df_ps_uky <- pull_prescreen_info(rcon_ps_uky, sitename = "uky")
    
    df_ps <- rbind(df_ps_uvm, df_ps_uky)
    df_ps <- reshape_recruitment_sources(df_ps, recruit_source_vars)
    df_ps <- general_prescreen_cleaning(df_ps)
  }
)

# join
df_enrl <- df_enrl %>% 
  select(-site) %>%
  full_join(df_ps, by = c("screen_id", "session", "date")) %>%
  group_by(screen_id) %>%
  tidyr::fill(
    sl_status, pi_prop, reasons_for_exclusion,
    randomized, redcap_id, ps_summary, screen_status,
    recruitment_sources, ps_exclusion_reasons, site,
    .direction = "updown"
  ) %>%
  ungroup() %>%
  arrange(screen_id, session)

# # write
save(df_enrl, file = "./data/enrollment.RData")


#TODO Incorporate S3 prescreen, filter by pregnant
#TODO 

