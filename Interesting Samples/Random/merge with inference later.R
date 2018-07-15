# Create a data frame of differences in promotion rates
disc_perm <- disc %>%
  rep_sample_n(size = nrow(disc), reps = 1000) %>%
  mutate(prom_perm = sample(promote)) %>%
  group_by(replicate, sex) %>%
  summarize(prop_prom_perm = mean(prom_perm == "promoted"),
            prop_prom = mean(promote == "promoted")) %>%
  summarize(diff_perm = diff(prop_prom_perm),
            diff_orig = diff(prop_prom))  # male - female


# Histogram of permuted differences
ggplot(disc_perm, aes(x = diff_perm)) + 
  geom_histogram(binwidth =  0.01) +
  geom_vline(aes(xintercept = diff_orig), col = "red")

# Find the 0.90, 0.95, and 0.99 quantiles of diff_perm
disc_perm %>% 
  summarize(q.90 = quantile(diff_perm, p = 0.9),
            q.95 = quantile(diff_perm, p = 0.95),
            q.99 = quantile(diff_perm, p = 0.99))

# Calculate the p-value for the original dataset
disc_perm %>%
  summarize(mean( diff_orig<= diff_perm))

#opportunity cost