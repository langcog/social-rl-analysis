d_ranked <- d |> 
  filter(shot == TRUE) |>
  rowwise() |> 
  mutate(
    payoffs_sorted = list(sort(c(payoffs_0, payoffs_1, payoffs_2, payoffs_3, payoffs_4, payoffs_5, payoffs_6, payoffs_7, payoffs_8, payoffs_9), decreasing = TRUE)),
    payoffs_ranked_0 = payoffs_sorted[1],
    payoffs_ranked_1 = payoffs_sorted[2],
    payoffs_ranked_2 = payoffs_sorted[3],
    payoffs_ranked_3 = payoffs_sorted[4],
    payoffs_ranked_4 = payoffs_sorted[5],
    payoffs_ranked_5 = payoffs_sorted[6],
    payoffs_ranked_6 = payoffs_sorted[7],
    payoffs_ranked_7 = payoffs_sorted[8],
    payoffs_ranked_8 = payoffs_sorted[9],
    payoffs_ranked_9 = payoffs_sorted[10]
  ) |> 
  ungroup() |> 
  select(-payoffs_sorted, -total_reward, -lifetime_total_reward, -reward_generated, -shot, -timestamp, -prolific_id, -solver, -zone_index, -refresh_count, -policy_id)  # Remove the temporary list column
# View the result
d_ranked





d_ranked |> 
  group_by(uid, condition_index, teacher, block_index, game_type, condition_count_index) |>
  filter(step == max(step)) |>
  ungroup() |>
  arrange(uid, condition_index, block_index, condition_count_index, step) |>
  select(uid, step, condition_index, block_index, condition_count_index, game_type, teacher) 

unique(d$uid)



# Copy the dataframe
d_cum <- filter(d, shot==TRUE)

# Initialize num_shots columns
for(i in 0:9) {
  d_cum[[paste0("num_shots_", i)]] <- 0
}

# Split the dataframe by groups
grouped_dfs <- d_cum %>% 
  group_by(uid, condition_index, teacher, block_index, game_type, condition_count_index) %>% 
  group_split()

# Function to update counts within each group
update_group_counts <- function(df) {
  for (i in 1:nrow(df)) {
    if (i > 1) {
      for (j in 0:9) {
        df[i, paste0("num_shots_", j)] <- df[i - 1, paste0("num_shots_", j)]
      }
    }
    position <- df[i, "position"]
    df[i, paste0("num_shots_", position)] <- df[i, paste0("num_shots_", position)] + 1
  }
  return(df)
}

# Apply the function to each group and recombine
d_cum <- do.call(rbind, lapply(grouped_dfs, update_group_counts))

# View the result
#d_cum

# Assuming d_cum is your grouped and updated dataframe
d_max_step <- d_cum %>%
  group_by(uid, condition_index, teacher, block_index, game_type, condition_count_index) %>%
  filter(step == max(step)) %>%
  ungroup() %>%
  select(uid, condition_index, condition_count_index, teacher, block_index, game_type,
         payoffs_0, payoffs_1, payoffs_2, payoffs_3, payoffs_4, payoffs_5, payoffs_6, payoffs_7, payoffs_8, payoffs_9,
         num_shots_0, num_shots_1, num_shots_2, num_shots_3, num_shots_4, num_shots_5, num_shots_6, num_shots_7, num_shots_8, num_shots_9) %>%
  arrange(uid, condition_index, block_index, condition_count_index)  

# View the result
print(d_max_step)










d_ranked <- d_max_step |> 
  rowwise() |> 
  mutate(
    sorted_indices = list(order(c(payoffs_0, payoffs_1, payoffs_2, payoffs_3, payoffs_4, payoffs_5, payoffs_6, payoffs_7, payoffs_8, payoffs_9))),
    num_shots_list = list(c(num_shots_0, num_shots_1, num_shots_2, num_shots_3, num_shots_4, num_shots_5, num_shots_6, num_shots_7, num_shots_8, num_shots_9)),
    total_num_shots = sum(num_shots_list),
    num_shots_rank_0 = num_shots_list[sorted_indices[1]] / total_num_shots,
    num_shots_rank_1 = num_shots_list[sorted_indices[2]] / total_num_shots,
    num_shots_rank_2 = num_shots_list[sorted_indices[3]] / total_num_shots,
    num_shots_rank_3 = num_shots_list[sorted_indices[4]] / total_num_shots,
    num_shots_rank_4 = num_shots_list[sorted_indices[5]] / total_num_shots,
    num_shots_rank_5 = num_shots_list[sorted_indices[6]] / total_num_shots,
    num_shots_rank_6 = num_shots_list[sorted_indices[7]] / total_num_shots,
    num_shots_rank_7 = num_shots_list[sorted_indices[8]] / total_num_shots,
    num_shots_rank_8 = num_shots_list[sorted_indices[9]] / total_num_shots,
    num_shots_rank_9 = num_shots_list[sorted_indices[10]] / total_num_shots
  ) |> 
  ungroup() |> 
  select(-num_shots_list, -sorted_indices) |> 
  arrange(uid, condition_index, block_index, condition_count_index) |>
  select(uid, condition_index, condition_count_index, teacher, block_index, game_type, 
         num_shots_rank_0, num_shots_rank_1, num_shots_rank_2, num_shots_rank_3, num_shots_rank_4, 
         num_shots_rank_5, num_shots_rank_6, num_shots_rank_7, num_shots_rank_8, num_shots_rank_9)

# View the result
print(d_ranked)









block_rankings <- d_ranked |>
  group_by(uid, condition_index, condition_count_index) |>
  arrange(uid, condition_index, block_index, condition_count_index, game_type) |>
  mutate(teacher_condition = teacher[!is.na(teacher)][1], 
         block_condition = reduce(game_type, paste)) |>
  group_by(teacher_condition, block_condition, block_index, game_type) |>
  summarise(mean_num_shots_rank_0 = mean(num_shots_rank_0),
            mean_num_shots_rank_1 = mean(num_shots_rank_1),
            mean_num_shots_rank_2 = mean(num_shots_rank_2),
            mean_num_shots_rank_3 = mean(num_shots_rank_3),
            mean_num_shots_rank_4 = mean(num_shots_rank_4),
            mean_num_shots_rank_5 = mean(num_shots_rank_5),
            mean_num_shots_rank_6 = mean(num_shots_rank_6),
            mean_num_shots_rank_7 = mean(num_shots_rank_7),
            mean_num_shots_rank_8 = mean(num_shots_rank_8),
            mean_num_shots_rank_9 = mean(num_shots_rank_9),
            sd_num_shots_rank_0 = sd(num_shots_rank_0),
            sd_num_shots_rank_1 = sd(num_shots_rank_1),
            sd_num_shots_rank_2 = sd(num_shots_rank_2),
            sd_num_shots_rank_3 = sd(num_shots_rank_3),
            sd_num_shots_rank_4 = sd(num_shots_rank_4),
            sd_num_shots_rank_5 = sd(num_shots_rank_5),
            sd_num_shots_rank_6 = sd(num_shots_rank_6),
            sd_num_shots_rank_7 = sd(num_shots_rank_7),
            sd_num_shots_rank_8 = sd(num_shots_rank_8),
            sd_num_shots_rank_9 = sd(num_shots_rank_9),
            sem_num_shots_rank_0 = sd(num_shots_rank_0)/sqrt(num_shots_rank_0),
            sem_num_shots_rank_1 = sd(num_shots_rank_1)/sqrt(num_shots_rank_1),
            sem_num_shots_rank_2 = sd(num_shots_rank_2)/sqrt(num_shots_rank_2),
            sem_num_shots_rank_3 = sd(num_shots_rank_3)/sqrt(num_shots_rank_3),
            sem_num_shots_rank_4 = sd(num_shots_rank_4)/sqrt(num_shots_rank_4),
            sem_num_shots_rank_5 = sd(num_shots_rank_5)/sqrt(num_shots_rank_5),
            sem_num_shots_rank_6 = sd(num_shots_rank_6)/sqrt(num_shots_rank_6),
            sem_num_shots_rank_7 = sd(num_shots_rank_7)/sqrt(num_shots_rank_7),
            sem_num_shots_rank_8 = sd(num_shots_rank_8)/sqrt(num_shots_rank_8),
            sem_num_shots_rank_9 = sd(num_shots_rank_9)/sqrt(num_shots_rank_9),
            ci_num_shots_rank_0 = 1.96 * sem_num_shots_rank_0,
            ci_num_shots_rank_1 = 1.96 * sem_num_shots_rank_1,
            ci_num_shots_rank_2 = 1.96 * sem_num_shots_rank_2,
            ci_num_shots_rank_3 = 1.96 * sem_num_shots_rank_3,
            ci_num_shots_rank_4 = 1.96 * sem_num_shots_rank_4,
            ci_num_shots_rank_5 = 1.96 * sem_num_shots_rank_5,
            ci_num_shots_rank_6 = 1.96 * sem_num_shots_rank_6,
            ci_num_shots_rank_7 = 1.96 * sem_num_shots_rank_7,
            ci_num_shots_rank_8 = 1.96 * sem_num_shots_rank_8,
            ci_num_shots_rank_9 = 1.96 * sem_num_shots_rank_9) 

block_rankings


df_long <- block_rankings |>
  filter(game_type!='watch') |>
  pivot_longer(
    cols = c(starts_with("mean_num_shots_rank_"), 
             starts_with("sd_num_shots_rank_"),
             starts_with("sem_num_shots_rank_"),
             starts_with("ci_num_shots_rank_")),
    names_to = c(".value", "rank"),
    names_pattern = "(mean_num_shots|sd_num_shots|sem_num_shots|ci_num_shots)_rank_(\\d+)"
  ) |>
  mutate(play_order = if_else(block_index == 2, "second play", "first play"))

# View the result
print(df_long)

new_labels <- setNames(as.character(0:9), paste0("mean_num_shots_rank_", 0:9))

ggplot(df_long, aes(x = rank, y = mean_num_shots, fill = as.factor(teacher_condition))) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  #geom_errorbar(aes(ymin = mean_num_shots - ci_num_shots, ymax = mean_num_shots + ci_num_shots),
  #              position = position_dodge(width = 0.9), width = 0.25) +
  facet_wrap(~block_condition + play_order) +
  scale_fill_discrete(name = "Teacher Condition") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) + 
  labs( x = "Ranked Asteroids (Ascending Payoffs)", y = "% of Shots") +
  scale_x_discrete(labels = new_labels)









block_rankings_2 <- d_ranked |>
  group_by(uid, condition_index, condition_count_index) |>
  arrange(uid, condition_index, block_index, condition_count_index, game_type) |>
  mutate(teacher_condition = teacher[!is.na(teacher)][1], 
         block_condition = reduce(game_type, paste)) |>
  mutate(
    play_order = case_when(
      block_index == 2 ~ "second play",
      game_type == "play" & block_index != 2 ~ "first play",
      TRUE ~ "not play"
    )
  )
block_rankings_2



df <- block_rankings_2


# Assuming your dataframe is named df

# Define the numeric columns explicitly
numeric_columns <- c("num_shots_rank_0", "num_shots_rank_1", "num_shots_rank_2", "num_shots_rank_3", "num_shots_rank_4",
                     "num_shots_rank_5", "num_shots_rank_6", "num_shots_rank_7", "num_shots_rank_8", "num_shots_rank_9")

# Step 1: Check and Clean Data
# Convert the defined numeric columns to numeric and handle non-numeric values
df[numeric_columns] <- lapply(df[numeric_columns], function(x) as.numeric(as.character(x)))

# You can check for NA values here
summary(df[numeric_columns])

# Subset the Data
df_not_play = df %>% filter(play_order == "not play")
df_first_play = df %>% filter(play_order == "first play")
df_second_play = df %>% filter(play_order == "second play")

# Step 2: Calculate Correlations
calculate_correlations <- function(df1, df2) {
  df1 %>%
    inner_join(df2, by = c("uid", "condition_count_index", "block_condition", "teacher_condition")) %>%
    group_by(uid, condition_count_index, block_condition, teacher_condition) %>%
    summarize(correlation = cor(df1[, numeric_columns], df2[, numeric_columns], use = "complete.obs"))
}

correlations_first_play = calculate_correlations(df_not_play, df_first_play)
correlations_second_play = calculate_correlations(df_not_play, df_second_play)

# Step 3: Average Across Correlations
average_correlations_first <- correlations_first_play %>%
  group_by(block_condition, teacher_condition) %>%
  summarize(avg_correlation = mean(correlation, na.rm = TRUE))

average_correlations_second <- correlations_second_play %>%
  group_by(block_condition, teacher_condition) %>%
  summarize(avg_correlation = mean(correlation, na.rm = TRUE))

# Step 4: Prepare Data for Plotting
plot_data <- bind_rows(
  mutate(average_correlations_first, play_comparison = "Not Play to First Play"),
  mutate(average_correlations_second, play_comparison = "Not Play to Second Play")
)

# Step 5: Create the Plots
ggplot(plot_data, aes(x = teacher_condition, y = avg_correlation, fill = play_comparison)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~block_condition) +
  labs(title = "Average Correlation by Play Order and Condition",
       x = "Teacher Condition", y = "Average Correlation") +
  theme_minimal()
