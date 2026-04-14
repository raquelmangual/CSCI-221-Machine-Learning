# Load and preview the dataset
#install.packages("arules")
#install.packages("arulesViz")

library(arules)
library(arulesViz)

data("Groceries")

summary(Groceries)
inspect(Groceries[1:5])

# Apply Apriori
rules <- apriori(Groceries,
                 parameter = list(supp = 0.01, conf = 0.3))

inspect(rules[1:10])

# Sort by lift value
rules_sorted <- sort(rules, by = "lift", decreasing = TRUE)

inspect(rules_sorted[1:10])

# Filter for "strong" rules (lift greater than 2)
strong_rules <- subset(rules, lift > 2)

inspect(strong_rules)

# Visualize the rules
plot(rules_sorted[1:10], method = "graph")