# Trevor Jones
# Project Draft Code

# load in bibliotecas
library(nflreadr)
library(tidyverse)
library(dplyr)
library(modelsummary)
library(tidymodels)

# load in combine data
combine <- load_combine(
  seasons = TRUE,
  file_type = getOption("nflreadr.prefer", default = "rds")) %>% 
  filter(season >= 2002) %>% rename(cfb_player_id = cfb_id) %>% 
  filter(!is.na(draft_ovr))
head(combine) 

draft <- load_draft_picks(
  seasons = T, 
  file_type = getOption("nflreadr.prefer", default = "rds")) %>% 
  filter(season >= 2002) %>% filter(!is.na(pick)) %>% rename(player_name = pfr_player_name)
head(draft)
# note, data is limited because it only shows players that are drafted

# merge the two datasets
df <- left_join(draft, combine, by = c("player_name"))
df <- df %>% rename(year = season.x)
head(df)

df <- df %>% select(year, round, pick, age, ht, allpro, seasons_started, w_av, dr_av,
                    games, wt, forty, bench, vertical, broad_jump, cone, shuttle, 
                    position, player_name)


# now, want to create a factor variable for positions
unique(df$position)
df$pos <- factor(df$position, labels = c("QB", "DE", "T", "DB", "DT", "WR", "TE",
                                         "RB", "LB", "G", "C", "K", "FB", "P", 
                                         "NT", "OL", "DL", "OLB", "CB", "S", 
                                         "ILB", "LS", "SAF", "OT"))
datasummary_skim(df, histogram = F, output = "combinesummary.tex")


# data imputation
est1 <- lm(pick ~ age + pos + wt + ht + forty + bench + vertical + broad_jump + cone + shuttle,
          data = df)
est2 <- lm(pick ~ age + pos + wt + ht + forty + bench + vertical + broad_jump + cone + shuttle,
           data = df)
models <- list(est1, est2)
modelsummary(models, stars = T, output = "initialmodels.tex")


# listwise deletion for missing values
df.listwise <- filter(df, !is.na(wt) & !is.na(forty) & !is.na(bench) & 
                        !is.na(vertical) & !is.na(broad_jump) & !is.na(cone) & 
                        !is.na(shuttle))
est <- lm(pick ~ age + pos + wt + forty + bench + vertical + broad_jump + cone + shuttle,
          data = df.listwise)







# going to need to impute data for my model, for now lets do an MAR assumption
df.predicted <- df
df.predicted$wt[is.na(df.predicted$wt)] <- 
  predict(est, newdata = df.predicted$wt[is.na(df.predicted$wt),])






# lets create the model I'll use for the decision tree


# Now, lets make a decision tree
df_split <- initial_split(df, prop = 0.8)
df_train <- training(df_split)
df_test <- testing(df_split)


tune_tree_spec <- decision_tree(
  min_n = tune(),
  tree_depth = tune(),
  cost_complexity = tune(),
) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

# regularization parameter
tree_parm_df1 <- tibble(cost_complexity = seq(.001,.2,by=.05))
tree_parm_df2 <- tibble(min_n = seq(10,100,by=10))
tree_parm_df3 <- tibble(tree_depth = seq(5,20,by=5))
tree_parm_df  <- full_join(tree_parm_df1,tree_parm_df2,by=character()) %>% full_join(.,tree_parm_df3,by=character())

# 3-Fold CV
rec_folds_tree <- vfold_cv(df_train, v = 3)

# Workflow
rec_wf_tree <- workflow() %>%
  add_model(tune_tree_spec) %>%
  add_formula(pick ~ age + pos + wt + forty + bench + vertical + broad_jump + 
                cone + shuttle)

# Tuning Results
rec_res_tree <- rec_wf_tree %>%
  tune_grid(
    resamples = rec_folds_tree,
    grid = tree_parm_df
  )
  # issue with this is that there is not a lot of variation within some of the estimates, 
  # may lead to overfitting

# what is the best value of lambda?
top_acc_tree  <- show_best(rec_res_tree, metric = "accuracy")
best_acc_tree <- select_best(rec_res_tree, metric = "accuracy")
final_tree_lasso <- finalize_workflow(rec_wf_tree,
                                      best_acc_tree
)
print('*********** TREE MODEL **************')
tree_test <- last_fit(final_tree_lasso,income_split) %>%
  collect_metrics()

tree_test %>% print(n = 1)
top_acc_tree %>% print(n = 1)

# combine results into a nice tibble (for later use)
tree_ans <- top_acc_tree %>% slice(1)
tree_ans %<>% left_join(tree_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "tree") %>% select(-starts_with(".config"))









# Trevor Jones
# Project Draft Code

# load in bibliotecas
library(nflreadr)
library(tidyverse)
library(dplyr)
library(modelsummary)
library(tidymodels)

# load in combine data
combine <- load_combine(
  seasons = TRUE,
  file_type = getOption("nflreadr.prefer", default = "rds")) %>% 
  filter(season >= 2002) %>% rename(cfb_player_id = cfb_id) %>% 
  filter(!is.na(draft_ovr))
head(combine) 

draft <- load_draft_picks(
  seasons = T, 
  file_type = getOption("nflreadr.prefer", default = "rds")) %>% 
  filter(season >= 2002) %>% filter(!is.na(pick)) %>% rename(player_name = pfr_player_name)
head(draft)
# note, data is limited because it only shows players that are drafted

# merge the two datasets
df <- left_join(draft, combine, by = c("player_name"))
df <- df %>% rename(year = season.x)
head(df)

df <- df %>% select(year, round, pick, age, ht, allpro, seasons_started, w_av, dr_av,
                    games, wt, forty, bench, vertical, broad_jump, cone, shuttle, 
                    position, player_name)


# now, want to create a factor variable for positions
unique(df$position)
df$pos <- factor(df$position, labels = c("QB", "DE", "T", "DB", "DT", "WR", "TE",
                                         "RB", "LB", "G", "C", "K", "FB", "P", 
                                         "NT", "OL", "DL", "OLB", "CB", "S", 
                                         "ILB", "LS", "SAF", "OT"))
datasummary_skim(df, histogram = F, output = "combinesummary.tex")


# data imputation
est1 <- lm(pick ~ age + pos + wt + ht + forty + bench + vertical + broad_jump + cone + shuttle,
          data = df)
est2 <- lm(pick ~ age + pos + wt + ht + forty + bench + vertical + broad_jump + cone + shuttle,
           data = df)
models <- list(est1, est2)
modelsummary(models, stars = T, output = "initialmodels.tex")


# listwise deletion for missing values
df.listwise <- filter(df, !is.na(wt) & !is.na(forty) & !is.na(bench) & 
                        !is.na(vertical) & !is.na(broad_jump) & !is.na(cone) & 
                        !is.na(shuttle))
est <- lm(pick ~ age + pos + wt + forty + bench + vertical + broad_jump + cone + shuttle,
          data = df.listwise)







# going to need to impute data for my model, for now lets do an MAR assumption
df.predicted <- df
df.predicted$wt[is.na(df.predicted$wt)] <- 
  predict(est, newdata = df.predicted$wt[is.na(df.predicted$wt),])






# lets create the model I'll use for the decision tree


# Now, lets make a decision tree
df_split <- initial_split(df, prop = 0.8)
df_train <- training(df_split)
df_test <- testing(df_split)


tune_tree_spec <- decision_tree(
  min_n = tune(),
  tree_depth = tune(),
  cost_complexity = tune(),
) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

# regularization parameter
tree_parm_df1 <- tibble(cost_complexity = seq(.001,.2,by=.05))
tree_parm_df2 <- tibble(min_n = seq(10,100,by=10))
tree_parm_df3 <- tibble(tree_depth = seq(5,20,by=5))
tree_parm_df  <- full_join(tree_parm_df1,tree_parm_df2,by=character()) %>% full_join(.,tree_parm_df3,by=character())

# 3-Fold CV
rec_folds_tree <- vfold_cv(df_train, v = 3)

# Workflow
rec_wf_tree <- workflow() %>%
  add_model(tune_tree_spec) %>%
  add_formula(pick ~ age + pos + wt + forty + bench + vertical + broad_jump + 
                cone + shuttle)

# Tuning Results
rec_res_tree <- rec_wf_tree %>%
  tune_grid(
    resamples = rec_folds_tree,
    grid = tree_parm_df
  )
  # issue with this is that there is not a lot of variation within some of the estimates, 
  # may lead to overfitting

# what is the best value of lambda?
top_acc_tree  <- show_best(rec_res_tree, metric = "accuracy")
best_acc_tree <- select_best(rec_res_tree, metric = "accuracy")
final_tree_lasso <- finalize_workflow(rec_wf_tree,
                                      best_acc_tree
)
print('*********** TREE MODEL **************')
tree_test <- last_fit(final_tree_lasso,income_split) %>%
  collect_metrics()

tree_test %>% print(n = 1)
top_acc_tree %>% print(n = 1)

# combine results into a nice tibble (for later use)
tree_ans <- top_acc_tree %>% slice(1)
tree_ans %<>% left_join(tree_test %>% slice(1),by=c(".metric",".estimator")) %>%
  mutate(alg = "tree") %>% select(-starts_with(".config"))










