library(tidyverse)
library(googlesheets4)
gs4_deauth()

raw <- read_sheet("https://docs.google.com/spreadsheets/d/1kQ35JKpk3vNaM7ZfFK2DFb1B9kQIQ4IODr7lOM394nQ/edit#gid=769268401") %>%
  select(-c(3:5))

# google "r vector from row"
# https://stackoverflow.com/a/55262815/841405
  raw %>%
    slice(2) %>%
    unlist(use.names = FALSE)

  raw %>%
    slice(2) %>%
    unlist(use.names = FALSE) %>%
    as_tibble() %>%
    mutate(value = str_remove(value, "Positive COVID-19 Cases\n"))

  raw %>%
    slice(2) %>%
    unlist(use.names = FALSE) %>%
    as_tibble() %>%
    mutate(value = str_remove(value, "Positive COVID-19 Cases\n")) %>%
    mutate(value = str_trim(value, "both"))

  raw %>%
    slice(2) %>%
    unlist(use.names = FALSE) %>%
    as_tibble() %>%
    mutate(value = str_remove(value, "Positive COVID-19 Cases\n")) %>%
    mutate(value = str_trim(value, "both")) %>%
    slice(3:n())

  raw %>%
    slice(2) %>%
    unlist(use.names = FALSE) %>%
    as_tibble() %>%
    mutate(value = str_remove(value, "Positive COVID-19 Cases\n")) %>%
    mutate(value = str_trim(value, "both")) %>%
    slice(3:n()) %>%
    mutate(value = str_replace(value,
                               # pattern:
                               #  find first comma (\\,)
                               #  and any character after it (.)
                               #  all times (*)
                               "\\,.*",

                               # replace with:
                               #  nothing ("")
                               ""
                               )
           )

  raw %>%
    slice(2) %>%
    unlist(use.names = FALSE) %>%
    as_tibble() %>%
    mutate(value = str_remove(value, "Positive COVID-19 Cases\n")) %>%
    mutate(value = str_trim(value, "both")) %>%
    slice(3:n()) %>%
    mutate(value = str_replace(value,
                               # pattern:
                               #  find first comma (\\,)
                               #  and any character after it (.)
                               #  all times (*)
                               "\\,.*",

                               # replace with:
                               #  nothing ("")
                               ""
    )
    ) %>%
    mutate(value = str_replace(value,
                               "\\-.*",
                               ""))

  start_date <- raw %>%
    slice(2) %>%
    unlist(use.names = FALSE) %>%
    as_tibble() %>%
    mutate(value = str_remove(value, "Positive COVID-19 Cases\n")) %>%
    mutate(value = str_trim(value, "both")) %>%
    slice(3:n()) %>%
    mutate(value = str_replace(value, "\\,.*", "")) %>%
    mutate(value = str_replace(value, "\\-.*", ""))

# google: "r repeat vector in place"
# https://stackoverflow.com/a/15141795/841405
  dates <- rep(start_date$value, each = 3)

  labels <- rep(c("students", "staff", "clusters"), nrow(start_date))

  dates_labels <- paste(dates, labels, sep = "_")

  raw %>%
    setNames(c("code", "site_name", dates_labels))

  raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n())

  school_levels <- c("Central Services Total",
                     "Elementary Total",
                     "Middle Total",
                     "High/Secondary Total",
                     "GRAND TOTAL")
  raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% school_levels))

  elem <- c("320304", "320308", "320374", "320318",
            "320319", "320363", "320313", "320310",
            "320315", "320344", "320332", "320347",
            "320320", "320324", "320328", "320327",
            "320339", "320340", "320348", "320352",
            "320354", "320360", "320362", "320364",
            "320367", "320369", "320372", "320376",
            "320388", "320400", "320289")

  middle <- c("320306", "320316", "320338", "320342",
              "320346", "320343", "320355",  "320370",
              "320366")

  high <- c("320312", "320317", "320323", "320322",
            "320309", "320325", "320701", "320341",
            "320353", "320356", "320365", "320368",
            "320314", "320401")

  raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% school_levels)) %>%
    mutate(level = case_when(
      code %in% "320LEA" ~ "Central Services",
      code %in% elem ~ "Elementary",
      code %in% middle ~ "Middle",
      code %in% high ~ "High",
      TRUE ~ "Hmmm, what are you?"
    ))


  raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    mutate(across(everything(), as.character)) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% school_levels)) %>%
    mutate(level = case_when(
      code %in% "320LEA" ~ "Central Services",
      code %in% elem ~ "Elementary",
      code %in% middle ~ "Middle",
      code %in% high ~ "High",
      TRUE ~ "Hmmm, what are you?"
    )) %>%
    mutate(across(everything(),
                  ~ case_when(. == "NULL" ~ "0",
                              . == "N/A" ~ "0",
                              TRUE ~ .)))

  raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    mutate(across(everything(), as.character)) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% school_levels)) %>%
    mutate(level = case_when(
      code %in% "320LEA" ~ "Central Services",
      code %in% elem ~ "Elementary",
      code %in% middle ~ "Middle",
      code %in% high ~ "High",
      TRUE ~ "Hmmm, what are you?"
    )) %>%
    mutate(across(everything(),
                  ~ case_when(. == "NULL" ~ "0",
                              . == "N/A" ~ "0",
                              TRUE ~ .))) %>%
    mutate(across(-c(code, site_name, level),
                  ~ as.numeric(.)))

  raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    mutate(across(everything(), as.character)) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% school_levels)) %>%
    mutate(level = case_when(
      code %in% "320LEA" ~ "Central Services",
      code %in% elem ~ "Elementary",
      code %in% middle ~ "Middle",
      code %in% high ~ "High",
      TRUE ~ "Hmmm, what are you?"
    )) %>%
    mutate(across(everything(),
                  ~ case_when(. == "NULL" ~ "0",
                              . == "N/A" ~ "0",
                              TRUE ~ .))) %>%
    mutate(across(-c(code, site_name, level),
                  ~ as.numeric(.))) %>%
    pivot_longer(cols = -c(code, site_name, level),
                 names_to = c("start_date", "type"),
                 values_to = "value",
                 names_sep = "_")

  raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    mutate(across(everything(), as.character)) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% school_levels)) %>%
    mutate(level = case_when(
      code %in% "320LEA" ~ "Central Services",
      code %in% elem ~ "Elementary",
      code %in% middle ~ "Middle",
      code %in% high ~ "High",
      TRUE ~ "Hmmm, what are you?"
    )) %>%
    mutate(across(everything(),
                  ~ case_when(. == "NULL" ~ "0",
                              . == "N/A" ~ "0",
                              TRUE ~ .))) %>%
    mutate(across(-c(code, site_name, level),
                  ~ as.numeric(.))) %>%
    pivot_longer(cols = -c(code, site_name, level),
                 names_to = c("start_date", "type"),
                 values_to = "value",
                 names_sep = "_") %>%
    pivot_wider(id_cols = c(code, site_name, level, start_date),
                names_from = type,
                values_from = value)

  raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    mutate(across(everything(), as.character)) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% school_levels)) %>%
    mutate(level = case_when(
      code %in% "320LEA" ~ "Central Services",
      code %in% elem ~ "Elementary",
      code %in% middle ~ "Middle",
      code %in% high ~ "High",
      TRUE ~ "Hmmm, what are you?"
    )) %>%
    mutate(across(everything(),
                  ~ case_when(. == "NULL" ~ "0",
                              . == "N/A" ~ "0",
                              TRUE ~ .))) %>%
    mutate(across(-c(code, site_name, level),
                  ~ as.numeric(.))) %>%
    pivot_longer(cols = -c(code, site_name, level),
                 names_to = c("start_date", "type"),
                 values_to = "value",
                 names_sep = "_") %>%
    pivot_wider(id_cols = c(code, site_name, level, start_date),
                names_from = type,
                values_from = value) %>%
    mutate(year = case_when(
            grepl("August", start_date) ~ 2021,
            grepl("September", start_date) ~ 2021,
            grepl("October", start_date) ~ 2021,
            grepl("November", start_date) ~ 2021,
            grepl("December", start_date) ~ 2021,
            TRUE ~ 2022),
           start_date = paste(start_date, year),
           start_date = lubridate::parse_date_time(start_date,
                                                   "%b d y"))

  dps <- raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    mutate(across(everything(), as.character)) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% school_levels)) %>%
    mutate(level = case_when(
      code %in% "320LEA" ~ "Central Services",
      code %in% elem ~ "Elementary",
      code %in% middle ~ "Middle",
      code %in% high ~ "High",
      TRUE ~ "Hmmm, what are you?"
    )) %>%
    mutate(across(everything(),
                  ~ case_when(. == "NULL" ~ "0",
                              . == "N/A" ~ "0",
                              TRUE ~ .))) %>%
    mutate(across(-c(code, site_name, level),
                  ~ as.numeric(.))) %>%
    pivot_longer(cols = -c(code, site_name, level),
                 names_to = c("start_date", "type"),
                 values_to = "value",
                 names_sep = "_") %>%
    pivot_wider(id_cols = c(code, site_name, level, start_date),
                names_from = type,
                values_from = value) %>%
    mutate(year = case_when(
      grepl("August", start_date) ~ 2021,
      grepl("September", start_date) ~ 2021,
      grepl("October", start_date) ~ 2021,
      grepl("November", start_date) ~ 2021,
      grepl("December", start_date) ~ 2021,
      TRUE ~ 2022),
      start_date = paste(start_date, year),
      start_date = lubridate::parse_date_time(start_date,
                                              "%b d y")) %>%
    select(-year)

  dps %>%
    group_by(level, start_date) %>%
    summarize(total_students = sum(students),
              total_staff = sum(staff),
              total_clusters = sum(clusters))




# load
  library(tidyverse)
  library(googlesheets4)
  gs4_deauth()

  raw <- read_sheet("https://docs.google.com/spreadsheets/d/1kQ35JKpk3vNaM7ZfFK2DFb1B9kQIQ4IODr7lOM394nQ/edit#gid=769268401") %>%
    select(-c(3:5))

# create vector of column names
  start_date <- raw %>%
    slice(2) %>%
    unlist(use.names = FALSE) %>%
    as_tibble() %>%
    mutate(value = str_remove(value, "Positive COVID-19 Cases\n")) %>%
    mutate(value = str_trim(value, "both")) %>%
    slice(3:n()) %>%
    mutate(value = str_replace(value, "\\,.*", "")) %>%
    mutate(value = str_replace(value, "\\-.*", ""))

  dates <- rep(start_date$value, each = 3)

  labels <- rep(c("students", "staff", "clusters"), nrow(start_date))

  dates_labels <- paste(dates, labels, sep = "_")

# create helpers
  school_levels <- c("Central Services Total",
                     "Elementary Total",
                     "Middle Total",
                     "High/Secondary Total",
                     "GRAND TOTAL")

  elem <- c("320304", "320308", "320374", "320318",
            "320319", "320363", "320313", "320310",
            "320315", "320344", "320332", "320347",
            "320320", "320324", "320328", "320327",
            "320339", "320340", "320348", "320352",
            "320354", "320360", "320362", "320364",
            "320367", "320369", "320372", "320376",
            "320388", "320400", "320289")

  middle <- c("320306", "320316", "320338", "320342",
              "320346", "320343", "320355",  "320370",
              "320366")

  high <- c("320312", "320317", "320323", "320322",
            "320309", "320325", "320701", "320341",
            "320353", "320356", "320365", "320368",
            "320314", "320401")

# tidy(ish)
  dps <- raw %>%
    setNames(c("code", "site_name", dates_labels)) %>%
    slice(5:n()) %>%
    mutate(across(everything(), as.character)) %>%
    filter(!is.na(site_name)) %>%
    filter(!(site_name %in% school_levels)) %>%
    mutate(level = case_when(
      code %in% "320LEA" ~ "Central Services",
      code %in% elem ~ "Elementary",
      code %in% middle ~ "Middle",
      code %in% high ~ "High",
      TRUE ~ "Hmmm, what are you?"
    )) %>%
    mutate(across(everything(),
                  ~ case_when(. == "NULL" ~ "0",
                              . == "N/A" ~ NA_character_,
                              TRUE ~ .))) %>%
    mutate(across(-c(code, site_name, level),
                  ~ as.numeric(.))) %>%
    pivot_longer(cols = -c(code, site_name, level),
                 names_to = c("start_date", "type"),
                 values_to = "value",
                 names_sep = "_") %>%
    pivot_wider(id_cols = c(code, site_name, level, start_date),
                names_from = type,
                values_from = value) %>%
    mutate(year = case_when(
      grepl("August", start_date) ~ 2021,
      grepl("September", start_date) ~ 2021,
      grepl("October", start_date) ~ 2021,
      grepl("November", start_date) ~ 2021,
      grepl("December", start_date) ~ 2021,
      TRUE ~ 2022),
      start_date = paste(start_date, year),
      start_date = lubridate::parse_date_time(start_date,
                                              "%b d y"),
      start_date = lubridate::date(start_date)) %>%
    select(-year) %>%
    mutate(level = factor(level,
                          levels = c("Central Services",
                                     "Elementary",
                                     "Middle",
                                     "High")))

# plot

# median line
  dps_median <- dps %>%
    filter(level!="Central Services") %>%
    group_by(level, start_date) %>%
    summarize(students = median(students))

# annotate n
  dps_n <- dps %>%
    filter(level!="Central Services") %>%
    distinct(site_name, .keep_all = TRUE) %>%
    count(level) %>%
    mutate(date = lubridate::ymd("2021-08-01")) %>%
    mutate(label = paste0(n, " schools"))

# annotate cases
  dps_cases <- dps %>%
    filter(level!="Central Services") %>%
    group_by(level) %>%
    summarize(total_students = sum(students)) %>%
    mutate(date = lubridate::ymd("2021-08-01")) %>%
    mutate(label = paste0(total_students, " total cases"))

# subtitle date min
  date_min <- dps %>%
    summarize(min = min(start_date)) %>%
    mutate(month = lubridate::month(min, label = TRUE, abbr = FALSE),
           day = lubridate::day(min),
           year = lubridate::year(min),
           date = paste0(month, " ", day, ", ", year)) %>%
    pull(date)

# subtitle date max
  date_max <- dps %>%
    summarize(max = max(start_date)) %>%
    mutate(month = lubridate::month(max, label = TRUE, abbr = FALSE),
           day = lubridate::day(max),
           year = lubridate::year(max),
           date = paste0(month, " ", day, ", ", year)) %>%
    pull(date)


  dps %>%
    filter(level!="Central Services") %>%
    ggplot(aes(x=start_date, y=students)) +
      #geom_step(color="grey", aes(group=site_name)) +
      geom_smooth(color="grey", aes(group=site_name), se = FALSE,
                  alpha = 0.3, size = .4) +
      geom_smooth(data = dps_median, aes(x=start_date, y=students), se = FALSE,
                  alpha = 1, size = 1) +
      #geom_step(data = dps_median, aes(x=start_date, y=students)) +
      geom_jitter(data = dps %>% filter(students > 10),
                  alpha = 0.3, shape = 21) +
      geom_text(data = dps_n, aes(x=date, y = 50, label=label),
                hjust=0) +
      geom_text(data = dps_cases, aes(x=date, y = 43, label=label),
              hjust=0) +
      facet_wrap(~ level, ncol = 1) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            plot.caption = element_text(margin=margin(15,0,0,0)),
            plot.title = element_text(face="bold")) +
      labs(x=NULL, y=NULL,
           title = str_wrap("Durham Public Schools kept cases low throughout the Delta and Omicron waves", 60),
           subtitle = paste0("Weekly reported cases among students, ",
                             "(", date_min, "-", date_max, ")"),
           caption = "Data Source: Durham Public Schools, https://tinyurl.com/mr32wdzs")
a
