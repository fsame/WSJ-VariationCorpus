## ---------------------------
##
## Script name: processing the gpt results
##
## ---------------------------

#___________________________________
# Directory, library, functions ----
#___________________________________

#***********************************
# set directory
#***********************************

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#***********************************
# libraries
#***********************************
library(tidyverse)
library(strex)
library(readr)
library(readxl)

doc_info <- read_rds("doc_info.rds") %>% 
  mutate(doc_number = str_after_first(Document_Name, "_wsj") %>% str_extract(., "\\d+") ) %>% 
  mutate(doc_number = ifelse(str_detect(Document_Name, "other_12504_wsj2436"), "2436-1", 
                             ifelse(str_detect(Document_Name, "citycountry_1432_wsj0910") & Target == "The United States (it/its)", "0910-1", doc_number)))


#list1rep1 <- read_delim("exp_output/list1rep1.txt", col_names = FALSE)


# Read in all gpt experiment results --------------------------------------


# List all text files in the folder
txt_files <- list.files(path = "exp_output/", pattern = "*.txt", full.names = TRUE)

# Initialize an empty list to store individual data frames
list_of_dfs <- list()

# Loop through each file
for (i in seq_along(txt_files)) {
  # Read the file
  tmp_df <- read_delim(txt_files[i], delim = "\t", col_names = FALSE)
  
  # Add file name as a new column
  tmp_df$file_name <- basename(txt_files[i])
  
  # Append to the list
  list_of_dfs[[i]] <- tmp_df
}

# Combine all data frames into one
final_df <- bind_rows(list_of_dfs)


final_df2 <- final_df %>%
  mutate(ref_code = str_before_first(X1, ":") %>% str_trim(.)) %>% 
  mutate(gen_refex = str_after_first(X1, ":") %>%  str_trim(.)) %>% 
  mutate(doc_number = str_extract(ref_code, "\\d+(?=_)" )) %>% 
  mutate(doc_number = ifelse(doc_number == "0910" & str_detect(file_name, "list3"), "0910-1",
                             ifelse(doc_number == "2436" & str_detect(file_name, "list3"), "2436-1", doc_number))) %>% 
  left_join(., doc_info, by = "doc_number") %>% 
  mutate(ID = sprintf("%04d", row_number()))


final_df3 <- final_df2 %>% 
  select(ID, gen_refex, Target, Title) %>% 
  mutate(gen_target = str_c(gen_refex, "_", Target)) %>% 
  distinct(gen_target, .keep_all = TRUE ) %>% 
  select(-gen_target) %>% 
  mutate(Title = str_replace_all(Title, "\\s+", " "))

write_delim(final_df3, "unique_exp_referent.txt", delim = "\t")


# Bring in the gpt annotations --------------------------------------------

gpt_annotations <- read_delim("gpt_annotations.txt", delim = "|", col_names = FALSE) %>% 
  rename(ID = X1, form = X2, correct_ref = X3) %>% 
  left_join(., final_df3, by = "ID")


write_delim(gpt_annotations, "annotations_for_correction.txt", delim = "\t")



# Read in the excel file after correction ---------------------------------


corrected_gpt_anno <- read_excel("annotations_excel.xlsx", col_names = TRUE) %>% 
  select(ID, form, correct_ref, generated_refex = gen_refex)

final_df4 <- final_df2 %>% 
  left_join(., corrected_gpt_anno, by = "ID") %>% 
  mutate(form = ifelse(form == "proper", "prop", form)) %>% 
  mutate(very_unique_id = str_c(doc_number, "_", Target, "_", gen_refex) ) %>% 
  group_by(very_unique_id) %>%
  mutate(form = first(na.omit(form))) %>%
  mutate(correct_ref = first(na.omit(correct_ref))) %>%
  ungroup() %>% 
  relocate(form,correct_ref, .after = gen_refex)



write_rds(final_df4, "gpt_experiment_annotation/all_gpt_refexes.rds")


# Bring in the final gpt annotations --------------------------------------


wsj_all_features <- read_rds("D:/Sciebo_shared_with_me/Mark_Ellison/00-TMEFS-Vault/03-Documents/SIGDIAL-2023/data/dfs_cleaned/stimuli.rds") %>% 
  mutate(doc_number = str_after_first(unique_connecting_code, "_wsj") %>% str_before_first(., "_") ) %>% 
  mutate(doc_number = ifelse(doc_number == "0910" & str_detect(unique_connecting_code, "citycountry_1432"), "0910-1",
                             ifelse(doc_number == "2436" & str_detect(unique_connecting_code, "other_12504"), "2436-1", doc_number)))%>% 
  mutate(refex_number = str_after_first(unique_connecting_code, "_wsj") ) %>% 
  mutate(refex_number = ifelse(doc_number == "0910-1" & str_detect(unique_connecting_code, "citycountry_1432"), str_c(refex_number, "-1"),
                             ifelse(doc_number == "2436-1" & str_detect(unique_connecting_code, "other_12504"), str_c(refex_number, "-1"), refex_number)))

table(wsj_all_features$doc_number)

all_gpt_refexes <- read_rds("gpt_experiment_annotation/all_gpt_refexes.rds") %>% 
  mutate(refex_number = str_extract(ref_code, "\\d+_\\d+") ) %>% 
  mutate(refex_number = ifelse(doc_number == "0910-1" & str_detect(file_name, "list3"), str_c(refex_number, "-1"),
                               ifelse(doc_number == "2436-1" & str_detect(file_name, "list3"), str_c(refex_number, "-1"), refex_number))) %>% 
  left_join(., wsj_all_features, by = "refex_number") %>% 
  select(X1, file_name, ref_code, original_ref, everything(), -generated_refex) 


write_rds(all_gpt_refexes, "gpt_experiment_annotation/gpt_original_refexes.rds")




