# Translation API

# https://cran.r-project.org/web/packages/googleLanguageR/readme/README.html

# https://cloud.google.com/iam/docs/creating-managing-service-account-keys

# install.packages("data.table")
library(data.table)


health_2 <- fread("datasets/health_tags_len.csv", header = T)

# install.packages("googleLanguageR")
library(googleLanguageR)


hea1_end <- gl_translate(health_2$idea[1:length(health_2$idea)])

hea_cat <- gl_translate(unique(health_2$main_topic))

health_2$idea_english <- hea1_end$translatedText

health_2$main_topic_english <- hea_cat$translatedText[match(
  health_2$main_topic, hea_cat$text)]

saveRDS(health_2,"health_english.rds")
write.csv(health_2, "csv/health_english.csv", row.names = F )

# cash

cash_2   <- fread("datasets/cash_tags_len_1categ.csv", header = T)

cas1_end <- gl_translate(cash_2$idea[1:length(cash_2$idea)])

cash_cat <- gl_translate(unique(cash_2$main_topic))

cash_2$idea_english <- cas1_end$translatedText

cash_2$main_topic_english <- cash_cat$translatedText[match(
  cash_2$main_topic, cash_cat$text)]

saveRDS(cash_2,"rds/cash_english.rds")
write.csv(cash_2, "csv/cash_english.csv", row.names = F )

