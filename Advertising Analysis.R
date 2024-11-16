library(readxl)
adanalysis <- read_excel("C:/Users/nisha/Downloads/adanalysis.xlsx")
View(adanalysis)

#Summary
summary(adanalysis)

# Perform one-sample t-test
t_test_result <- t.test(adanalysis$adtype2, mu = 33000)

# Print the results
print(t_test_result)