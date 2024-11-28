setwd("/Users/lixinzhu/Desktop/RData/R-AB Test_dataset")

library(readr)
library(dplyr)

cookie_data <- read_csv("/Users/lixinzhu/Desktop/RData/R-AB Test_dataset/cookie_cats.csv")


# 检查数据结构
str(cookie_data)

# 查看是否有缺失值
sum(is.na(cookie_data))

retention_summary <- cookie_data %>%
  group_by(version) %>%
  summarize(day_1_retention = mean(retention_1),
            day_7_retention = mean(retention_7))

#retention_1: Did the player come back and play 1 day after installing?
#retention_7: Did the player come back and play 7 days after installing?
# 创建列联表 就是两个逻辑变量。回来了登陆了VS没回来没登陆   不是连续性变量。就是是分类变量 利用频数（人数）进行卡方检验
table_1 <- table(cookie_data$version, cookie_data$retention_1)
chisq_test_1 <- chisq.test(table_1)
print(chisq_test_1)
# p-value = 0.0755
#这表明两组（gate_30 和 gate_40）在 1 天留存 上没有显著差异（p 值 > 0.05），即没有足够的证据拒绝原假设

# 对 7 天留存进行卡方检验
table_7 <- table(cookie_data$version, cookie_data$retention_7)
chisq_test_7 <- chisq.test(table_7)
print(chisq_test_7)
#p-value = 0.001601
#这表明两组在 7 天留存上有显著差异（p 值 < 0.05），可以拒绝原假设，说明 gate_30 和 gate_40 对 7 天留存有统计学上的显著影响。



table_2 <- prop.table(table_1)


