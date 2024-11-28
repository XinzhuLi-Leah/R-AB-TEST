setwd("/Users/lixinzhu/Desktop/RData/R-AB Test_dataset")

library(readr)
library(dplyr)

df <- read_csv("/Users/lixinzhu/Desktop/RData/R-AB Test_dataset/AB_Test_Results.csv")

str(df)
head(df)

summary(df)
colSums(is.na(df))

summary_df <- df %>% group_by(VARIANT_NAME) %>% summarise(num_users = n_distinct(USER_ID),
                                           total_revenue = sum(REVENUE, na.rm = TRUE),
                                           avg_revenue_per_user = mean(REVENUE, na.rm = TRUE)
                                           )

summary_df


t_test_result <- t.test(REVENUE ~ VARIANT_NAME, data = df, var.equal = FALSE)
# p-value = 0.2047

cat("T-statistic: ", t_test_result$statistic, "\n")
cat("P-value: ", t_test_result$p.value, "\n")


if (t_test_result$p.value < 0.05) {
  cat("结论：A 组和 B 组的收入差异具有统计显著性。\n")
} else {
  cat("结论：A 组和 B 组的收入差异没有统计显著性。\n")
}