# This next line is run when the whole file is executed, but not when knitr calls individual chunks.
rm(list=ls(all=TRUE)) #Clear the memory for any variables set from any previous runs.


# ---- load-sources ------------------------------------------------------------


# ---- load-packages -----------------------------------------------------------
library(magrittr                , quietly=TRUE)
library(stringr                               )
library(wakefield                             )
library(plotly                                ) # devtools::install_github("ropensci/plotly")
requireNamespace("dplyr"                      )
requireNamespace("lubridate"                  )
requireNamespace("forcats"                    )


# ---- declare-globals ---------------------------------------------------------
# readr::spec_csv(path_in_links)
path_in_links    <- "./data-unshared/names-for-link-king/linking-output/final_link_master.csv"
path_in_matching <- "./data-unshared/names-for-link-king/ds_matching.csv"
path_in_sample   <- "./data-unshared/names-for-link-king/ds_sample.csv"

col_types_links = readr::cols_only(
  certainty                       = readr::col_character(),
  uniqueid                        = readr::col_integer(),
  client_identifier               = readr::col_character(),
  sample                          = readr::col_integer(),
  client_first_name               = readr::col_character(),
  client_last_name                = readr::col_character(),
  client_middle_name              = readr::col_character(),
  client_alternate_last_name      = readr::col_character(),
  client_race                     = readr::col_character(),
  client_social_security_number   = readr::col_character(),
  client_birthdate                = readr::col_character(),
  client_gender                   = readr::col_character(),
  flex                            = readr::col_character(),
  RECNUM                          = readr::col_integer(),
  fn_ln_pct                       = readr::col_double(),
  # TEMP_ID                         = readr::col_character(),
  # client_origin                   = readr::col_double(),
  # client_core                     = readr::col_character(),
  # summary_d                       = readr::col_character(),
  MATCHED                         = readr::col_character(),
  SCORE                           = readr::col_double(),
  # manually_verified               = readr::col_logical(),
  method                          = readr::col_character()#,
  # regroup                         = readr::col_logical(),
  # pending_edits                   = readr::col_double(),
  # decision                        = readr::col_logical(),
  # display_option                  = readr::col_double(),
  # client_grp_ok                   = readr::col_logical(),
  # quick_group2                    = readr::col_double(),
  # quick_group3                    = readr::col_double(),
  # quick_group4                    = readr::col_double(),
  # quick_group5                    = readr::col_double(),
  # quick_group6                    = readr::col_logical(),
  # found                           = readr::col_logical(),
  # alias_strength                  = readr::col_logical(),
  # collision_case                  = readr::col_logical(),
  # weak_alias_sort1                = readr::col_double(),
  # weak_alias_sort2                = readr::col_double(),
  # xlink_num                       = readr::col_logical(),
  # xlink_case                      = readr::col_logical(),
  # user_grouped                    = readr::col_logical(),
  # min_cert                        = readr::col_double(),
  # max_cert                        = readr::col_double(),
  # quick_sort                      = readr::col_double(),
  # first_unique                    = readr::col_double(),
  # first                           = readr::col_double(),
  # regroup_n                       = readr::col_double(),
  # initial_core                    = readr::col_double(),
  # sortindex1                      = readr::col_double(),
  # sortindex2                      = readr::col_double(),
  # sortindex3                      = readr::col_double(),
  # sortindex4                      = readr::col_double(),
  # sortindex5                      = readr::col_double(),
  # sortindex6                      = readr::col_double(),
  # sortindex7                      = readr::col_double(),
  # sortindex8                      = readr::col_double(),
  # sortindex9                      = readr::col_double(),
  # sortindex10                     = readr::col_double(),
  # non_core_n                      = readr::col_double(),
  # core_n                          = readr::col_double(),
  # mapped_recnum                   = readr::col_double(),
  # first_collision                 = readr::col_double()
)

# ---- load-data ---------------------------------------------------------------
ds_links_0    <- readr::read_csv(path_in_links    , col_types=col_types_links )
ds_matching_0 <- readr::read_csv(path_in_matching                             )
ds_sample_0   <- readr::read_csv(path_in_sample                               )


# ---- tweak-data ---------------------------------------------------------------
ds_links_0 <-
  ds_links_0 %>% 
  dplyr::group_by(uniqueid) %>% 
  dplyr::mutate(
    sample = dplyr::case_when(
      is.na(sample) ~ 0L,
      sample == 1L  ~ 1L,
      TRUE          ~ NA_integer_
    ),
    
    count_links    = dplyr::n(),
    count_sample   = sum(sample == 1L, na.rm = TRUE),
    count_matching = sum(sample == 0L, na.rm = TRUE),
  ) %>% 
  dplyr::ungroup()

table(ds_links_0$count_links   , useNA="ifany")
table(ds_links_0$count_sample  , useNA="ifany")
table(ds_links_0$count_matching, useNA="ifany")

if(length(unique(table(ds_links_0$count_links))) != 2){
  print("Some individual has more than two rows associated with them. This is definitely wrong")
  stop()
}

ds_links <-
  ds_links_0 %>% 
  dplyr::filter(count_links == 2) %>% 
  dplyr::group_by(uniqueid) %>% 
  dplyr::mutate(
    matched          = OuhscMunge::first_nonmissing(MATCHED  ),
    score            = OuhscMunge::first_nonmissing(SCORE    ),
    method           = OuhscMunge::first_nonmissing(method   ),
    
    rown_o_sample    = dplyr::if_else(sample == 1L, client_identifier, NA_character_),
    rown_o_matching  = dplyr::if_else(sample == 0L, client_identifier, NA_character_),
    
    # rown_o_sample    = OuhscMunge::first_nonmissing(rown_o_sample   ),
    # rown_o_matching  = OuhscMunge::first_nonmissing(rown_o_matching ),
    
    # link_correct1    = substr(rown_o_sample, start = 2, stop = 6),
    # link_correct2    = dplyr::lead(rown_o_matching, 1),
    
    link_correct     = substr(rown_o_sample, start = 2, stop = 6) == dplyr::lead(rown_o_matching, 1),
    
    uncertainty_int = certainty,
    uncertainty_int = dplyr::case_when(
      is.na(uncertainty_int)         ~ NA_integer_,
      uncertainty_int == "Reference" ~ NA_integer_,
      uncertainty_int == "Level 1"   ~ 1L,
      uncertainty_int == "Level 2"   ~ 2L,
      uncertainty_int == "Level 3"   ~ 3L,
      uncertainty_int == "Level 4"   ~ 4L,
      uncertainty_int == "Level 6"   ~ 6L,
      uncertainty_int == "Level 7"   ~ 7L
    ),
    uncertainty_int = OuhscMunge::first_nonmissing(uncertainty_int)
    
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(link_correct) %>% 
  dplyr::mutate(
    rown_o_matching = substr(rown_o_sample, start = 2, stop = 6),
  ) %>% 
  dplyr::select(
    client_identifier,
    rown_o_sample,
    rown_o_matching,
    link_correct,
    uncertainty_int
  )

ds_sample <-
  ds_sample_0 %>% 
  dplyr::mutate(
    wrinkle_count_corrected = name_middle_missing + name_maiden_missing + gender_swapped + gender_missing + ssn_typo + ssn_missing + ethnicity_swapped  + ethnicity_missing + dob_increment + dob_missing + zip_typo + zip_missing + nicknamed
  )
  
ds_matching <-
  ds_matching_0 %>% 
  dplyr::mutate(
    rown_o_w = paste0("w",rown_o)
  ) %>% 
  dplyr::select(
    rown_o,
    rown_o_w,
    
    name_first,
    name_last,
    name_middle,
    name_maiden, 
    gender,
    ethnicity,
    dob,
    zip_code,
    ssn_fake
  )

ds_results_sample_possible <-
  ds_sample %>% 
  dplyr::filter(grab_for_sample_possible) %>%  # nrow(ds_results_sample_possible) = 3000
  dplyr::left_join(
    ds_matching,
    by = c("rown_o_w" = "rown_o_w")
  ) %>% # nrow(ds_results_sample_possible) = 3000
  dplyr::left_join(
    ds_links,
    by = c("rown_o_w" = "rown_o_sample")
  ) %>%  # nrow(ds_results_sample_possible) = 3000
  dplyr::mutate(
    has_a_link   = dplyr::if_else(!is.na(link_correct), TRUE , FALSE),
    # link_correct = dplyr::if_else(is.na(link_correct) , FALSE, link_correct),
    
    uncertainty_plus_link = dplyr::if_else(!link_correct & is.na(uncertainty_int), 10L, uncertainty_int )
    
  ) %>% 
  dplyr::select(
    rown_o_w,
    rown_o,
    
    has_a_link,
    link_correct,
    uncertainty_int,
    wrinkle_count_corrected,
    
    uncertainty_plus_link,
    
    name_first, 
    name_first_w,
    name_last,
    name_last_w,
    name_middle,
    name_middle_w, 
    name_maiden,
    name_maiden_w,
    gender,
    gender_w,
    ethnicity, 
    ethnicity_w,
    dob,
    dob_w,
    zip_code,
    zip_code_w, 
    ssn_fake,
    ssn_fake_w,
    
    name_middle_missing,
    name_maiden_missing, 
    gender_swapped,
    gender_missing,
    ssn_typo,
    ssn_missing, 
    ethnicity_swapped,
    ethnicity_missing,
    dob_increment,
    dob_missing, 
    zip_typo,
    zip_missing,
    nicknamed,
  )

ds_results_sample_impossible <-
  ds_sample %>% 
  dplyr::filter(grab_for_sample_impossible) %>% # nrow(ds_results_sample_impossible) = 1000
  dplyr::left_join(
    ds_links,
    by = c("rown_o_w" = "rown_o_sample")
  ) %>% 
  dplyr::mutate(
    failure_correct = dplyr::if_else(is.na(link_correct), TRUE, FALSE)
  )


# ---- table-results-simple ---------------------------------------------------------------
table_correct_links_possible <-
  ds_results_sample_possible %>% 
  dplyr::summarize(
    count            = dplyr::n(),
    
    count_correct       = sum(link_correct, na.rm = TRUE),
    prop_correct        = count_correct / count,
    
    count_failed        = sum(!has_a_link, na.rm = TRUE),
    prop_failed         = count_failed / count,
    
    count_misleading    = sum(has_a_link & !link_correct, na.rm = TRUE),
    prop_misleading     = count_misleading / count,
    
  ) %>% 
  dplyr::mutate(
    Count_of_Possible_Sample_Rows = count,
    
    Count_of_True_Positives       = paste0(round(count_correct    , 2), " ( ", 100L*prop_correct    , "% )"),
    Count_of_False_Negatives      = paste0(round(count_failed     , 2), " ( ", 100L*prop_failed     , "% )"),
    Count_of_Misleading_Positives = paste0(round(count_misleading , 2), " ( ", 100L*prop_misleading , "% )"),
    
  ) %>% 
  dplyr::select(
    Count_of_Possible_Sample_Rows,
    Count_of_True_Positives,
    Count_of_False_Negatives,
    Count_of_Misleading_Positives
  ) %>% 
  DT::datatable(
    colnames=gsub("_", " ", colnames(.)),
    options = list(
      pageLength = 25,
      columnDefs = list(list(className = 'dt-center', targets = 1:4))
    )
  )

table_correct_links_impossible <-
  ds_results_sample_impossible %>% 
  dplyr::summarize(
    count = dplyr::n(),
    
    count_unlinked_correct = sum(failure_correct),
    prop_unlinked_correct  = count_unlinked_correct / count,
    
    count_unlinked_incorrect = sum(!failure_correct),
    prop_unlinked_incorrect  = count_unlinked_incorrect / count
    
  ) %>% 
  dplyr::mutate(
    Count_of_Unmatchable_Sample_Rows = count,
    
    Count_of_True_Negative_Links  = paste0(count_unlinked_correct  , " ( ", 100L*prop_unlinked_correct  , "% )"),
    Count_of_False_Positive_Links = paste0(count_unlinked_incorrect, " ( ", 100L*prop_unlinked_incorrect, "% )"),
  ) %>% 
  dplyr::select(
    Count_of_Unmatchable_Sample_Rows,
    Count_of_True_Negative_Links,
    Count_of_False_Positive_Links,
  ) %>% 
  DT::datatable(
    colnames=gsub("_", " ", colnames(.)),
    options = list(
      pageLength = 25,
      columnDefs = list(list(className = 'dt-center', targets = 1:3))
    )
  )

table_correct_links_possible
table_correct_links_impossible


# ---- graph-wrinkle-links ---------------------------------------------------------------
table_wrinkle_true_positives <-
  ds_results_sample_possible %>% 
  dplyr::group_by(wrinkle_count_corrected) %>% 
  dplyr::summarize(
    count                = dplyr::n(),
    count_true_positives = sum(has_a_link, na.rm = TRUE),
    prop_true_positives  = count_true_positives / count
  ) %>% 
  dplyr::ungroup() 

graph_wrinkle_true_positives <-
  table_wrinkle_true_positives %>% 
  ggplot(
    aes(
      x = wrinkle_count_corrected,
      y = prop_true_positives
    )
  ) +
  geom_line() +
  geom_point(
    aes(size = count_true_positives),
    alpha    = 0.5
  ) +
  coord_cartesian(ylim = c(0,1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "blue", alpha = 0.5)+
  scale_y_continuous(labels = scales::percent) +
  # repo_theme(14) +
  theme(legend.position = "right") +
  labs(
    x        = "Wrinkle Count",
    y        = "Percentage: True Positive",
    title    = "Percentage of Accurately Linked Rows by Count of Wrinkles\nThe blue line represents a 50% of rows being correctly linked."
  )

graph_wrinkle_true_positives %>%  ggplotly(height = 500, width = 700)





