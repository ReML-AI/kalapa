#=========================================
#  A Naive Approach for Data Processing
#=========================================

# Clear work space: 
rm(list = ls())

# Load data: 
suppressMessages(library(tidyverse))

args <- commandArgs(trailingOnly = TRUE)
# print(args)

INPUT_DIR <- args[1]
FIELD_NAME <- args[2]
BIN_NUM_LIMIT <- as.numeric(args[3])
STOP_LIMIT <- args[4]
if (STOP_LIMIT != "N") {
    STOP_LIMIT <- as.double(STOP_LIMIT)
}
COUNT_DISTR_LIMIT <- as.double(args[4])

train_file <- paste0(INPUT_DIR, "/", FIELD_NAME, "/", "train.csv")
# print(train_file)
df_train <- read_csv(train_file, col_type = cols())
df_train %>% mutate_if(is.character, function(x) {str_to_upper(x)}) -> df_train

test_file <- paste0(INPUT_DIR, "/", FIELD_NAME, "/", "test.csv")
# print(test_file)
df_test <- read_csv(test_file, col_type = cols())
df_test %>% mutate_if(is.character, function(x) {str_to_upper(x)}) -> df_test

if (FIELD_NAME == "FIELD_23") {
    df_train$FIELD_23[is.na(df_train$FIELD_23)] <- FALSE
    df_test$FIELD_23[is.na(df_test$FIELD_23)] <- FALSE
} else if (FIELD_NAME == "FIELD_36") {
    df_test %>% mutate(FIELD_36 = as.logical(FIELD_36)) -> df_test
}


# Conduct binning variables: 
suppressMessages(library(scorecard))

# Generates optimal binning for all variables/features: 
bins_var <- woebin(df_train %>% select(-id), y="label", no_cores=1, positive="label|1", check_cate_num=FALSE, bin_num_limit=BIN_NUM_LIMIT, stop_limit=STOP_LIMIT, count_distr_limit=COUNT_DISTR_LIMIT, init_count_distr=0.02)


# IV for variables/features: 

do.call("rbind", bins_var) %>% 
    as.data.frame() %>% 
    filter(!duplicated(variable)) %>% 
    rename(iv_var = total_iv) %>% 
    arrange(iv_var) %>% 
    mutate(variable = factor(variable, levels = variable)) -> iv_values


# Features have IV > 0: 

iv_values %>%
    filter(iv_var > 0) %>% 
    pull(variable) %>% 
    as.character() -> var_IV_10

# print(var_IV_10)

# Conduct data transformation based on IV/WoE and filter features with IV > 0: 

train_woe <- woebin_ply(df_train %>% select(-id), bins_var) %>% 
    as.data.frame() %>% 
    select(c("label", paste0(var_IV_10, "_", "woe")))


# Data transformation for actual test data: 

test_woe <- woebin_ply(df_test %>% select(-id), bins_var) %>% 
    as.data.frame() %>% 
    select(paste0(var_IV_10, "_", "woe"))

# A function imputes NA observations: 

replace_na_categorical <- function(x, y) {
    
    y %>% 
        table() %>% 
        as.data.frame() %>% 
        arrange(-Freq) -> my_df
    
    n_obs <- sum(my_df$Freq)
    
    pop <- my_df$. %>% as.character() %>% as.numeric()
    
    set.seed(29)
    
    x[is.na(x)] <- sample(pop, sum(is.na(x)), replace = TRUE, prob = my_df$Freq)
    
    return(x)
}

# Use the function: 

if (FIELD_NAME == "FIELD_13") {
    test_woe %>% mutate(FIELD_13_woe = replace_na_categorical(FIELD_13_woe, train_woe$FIELD_13_woe)) -> test_woe_imputed
} else if (FIELD_NAME == "FIELD_7") {
    test_woe %>% mutate(FIELD_7_woe = replace_na_categorical(FIELD_7_woe, train_woe$FIELD_7_woe)) -> test_woe_imputed
} else if (FIELD_NAME == "FIELD_41") {
    test_woe %>% mutate(FIELD_41_woe = replace_na_categorical(FIELD_41_woe, train_woe$FIELD_41_woe)) -> test_woe_imputed 
} else if (FIELD_NAME == "FIELD_10") {
    test_woe %>% mutate(FIELD_10_woe = replace_na_categorical(FIELD_10_woe, train_woe$FIELD_10_woe)) -> test_woe_imputed
} else if (FIELD_NAME == "FIELD_39") {
    test_woe %>% mutate(FIELD_39_woe = replace_na_categorical(FIELD_39_woe, train_woe$FIELD_39_woe)) -> test_woe_imputed
} else if (FIELD_NAME == "FIELD_11") {
    test_woe %>% mutate(FIELD_11_woe = replace_na_categorical(FIELD_11_woe, train_woe$FIELD_11_woe)) -> test_woe_imputed
} else if (FIELD_NAME == "FIELD_9") {
    test_woe %>% mutate(FIELD_9_woe = replace_na_categorical(FIELD_9_woe, train_woe$FIELD_9_woe)) -> test_woe_imputed
} else if (FIELD_NAME == "FIELD_12") {
    test_woe %>% mutate(FIELD_12_woe = replace_na_categorical(FIELD_12_woe, train_woe$FIELD_12_woe)) -> test_woe_imputed
} else {
    test_woe -> test_woe_imputed
}


#======================================================
# Attempt 4: Default Random Forest with Scaled Data
#======================================================

# For convinience, convert binary target variable to factor: 

train_woe %>% 
    mutate(label = case_when(label == 1 ~ "Bad", TRUE ~ "Good")) %>% 
    mutate(label = as.factor(label)) -> df_forGBM

# Scale our data: 

df_forGBM %>% 
    mutate_if(is.numeric, function(x) {(x - min(x)) / (max(x) - min(x))}) -> df_forGBM_Scaled

test_woe_imputed %>% 
    mutate_if(is.numeric, function(x) {(x - min(x)) / (max(x) - min(x))}) -> df_test_Scaled

write_csv(data.frame(id = 0:29999, df_forGBM_Scaled), paste0(INPUT_DIR, "/", FIELD_NAME, "/", "train_woe.csv"))
write_csv(data.frame(id = 30000:49999, df_test_Scaled), paste0(INPUT_DIR, "/", FIELD_NAME, "/", "test_woe.csv"))