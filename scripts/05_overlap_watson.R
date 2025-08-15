# 05_overlap_watson.R
source(here::here("scripts","00_setup.R"))
df_clean <- readRDS(here::here("data","processed","df_clean.rds"))

sites <- unique(df_clean$Site)
site_pairs <- expand.grid(Site1 = sites, Site2 = sites) %>%
  filter(as.character(Site1) < as.character(Site2))

overlap_df <- site_pairs %>%
  rowwise() %>%
  mutate(
    Delta = overlap::overlapEst(
      df_clean$HourRadians[df_clean$Site == Site1],
      df_clean$HourRadians[df_clean$Site == Site2],
      type = "Dhat4"
    )
  ) %>% ungroup()

# heatmap table
overlap_matrix <- matrix(NA, nrow = length(sites), ncol = length(sites),
                         dimnames = list(sites, sites))
for (i in 1:nrow(overlap_df)) {
  s1 <- as.character(overlap_df$Site1[i]); s2 <- as.character(overlap_df$Site2[i])
  val <- overlap_df$Delta[i]
  overlap_matrix[s1, s2] <- val; overlap_matrix[s2, s1] <- val
}
diag(overlap_matrix) <- 1
write.csv(overlap_matrix, file.path(TABS,"overlap_matrix.csv"))

# Watson U² across pairs (subsample to speed up)
site_combinations <- combn(sites, 2, simplify = FALSE)
watson_results <- purrr::map_dfr(site_combinations, function(pair) {
  s1 <- df_clean$HourRadians[df_clean$Site == pair[1]]
  s2 <- df_clean$HourRadians[df_clean$Site == pair[2]]
  s1 <- sample(s1, min(1000, length(s1))); s2 <- sample(s2, min(1000, length(s2)))
  test <- circular::watson.two.test(circular(s1), circular(s2))
  tibble(site1 = pair[1], site2 = pair[2], U2 = as.numeric(test$statistic))
})
write.csv(watson_results, file.path(TABS,"watson_two_sample_results.csv"), row.names = FALSE)
