library(readxl)
X4_24_10_06_H1b <- read_excel("/home/yui/Downloads/4.24_10 06_H1b.xlsx")
df <- data.frame(X4_24_10_06_H1b)
library(plm)
library(skimr)

#install.packages("fastDummies")
install.packages("sjPlot")
library(fastDummies)
df3=dummy_cols(df,select_columns="category")
## Model Specification
# within estimator
# consider within 'state & month'
df4=pdata.frame(X4_24_10_06_H1b ,index = NULL)

model1 = plm(log(foreign_wage_level) ~ certi_dummy + total_positions + log(prevailing_wage) 
             + df3$`category_Science, Technology, Engineering, and Mathematics (STEM)` 
             + df3$`category_Arts, Media & Entertainment`
             + df3$`category_Business, Finance, and Management`
             + df3$`category_Education & Social Services`
             + df3$`category_Healthcare & Life Sciences`
             + df3$`category_Legal and Compliance`
             + df3$`category_Social Services & Community`
             + df3$`category_Technology & Information Systems`,
             index = c("location","policy_dummy"), 
             model = "within", data = df)
summary(model1)

# consider within 'state'
model2 = plm(log(foreign_wage_level) ~ certi_dummy + total_positions + log(prevailing_wage) 
             + df3$`category_Science, Technology, Engineering, and Mathematics (STEM)` 
             + df3$`category_Arts, Media & Entertainment`
             + df3$`category_Business, Finance, and Management`
             + df3$`category_Education & Social Services`
             + df3$`category_Healthcare & Life Sciences`
             + df3$`category_Legal and Compliance`
             + df3$`category_Social Services & Community`
             + df3$`category_Technology & Information Systems`
             + policy_dummy,
             index = c("location"), 
             model = "within", data = df)
df$p <- predict(model2, df)
write.csv(df, "/home/yui/Downloads/4.24_10 06_H1b.csv", row.names=FALSE)
install.packages("writexl")
library("writexl")
write_xlsx(df, "/home/yui/Downloads/4.24_10 06_H1b.xlsx")
summary(model2)

