
library(ggplot2)

# SF detached homes
# Three scenarios: Worst, Base and Best visualizing profit, ROI and net return

scenarios <- data.frame(
  Scenario = c("Worst Case", "Base Case", "Best Case"),
  Units_Sold = c(175, 200, 210),
  Price_Per_Unit = c(550000, 575000, 600000),
  Cost_Per_Unit = c(500000, 505000, 510000),
  Fixed_Costs = c(25000, 20000, 18000),
  Operating_Costs = c(6000, 6000, 6000),
  Revenue_Growth = c(0.02, 0.05, 0.08),       # Annual growth
  Initial_Investment = c(4000000, 3500000, 3000000) # Assumed initial investment
)
scenarios$Revenue_Y1 <- scenarios$Units_Sold * scenarios$Price_Per_Unit
scenarios$Cost_Y1 <- scenarios$Fixed_Costs + (scenarios$Units_Sold * scenarios$Cost_Per_Unit) + (scenarios$Operating_Costs * scenarios$Units_Sold)
scenarios$Profit_Y1 <- scenarios$Revenue_Y1 - scenarios$Cost_Y1

scenarios$Revenue_Y2 <- scenarios$Revenue_Y1 * (1 + scenarios$Revenue_Growth)
scenarios$Revenue_Y3 <- scenarios$Revenue_Y2 * (1 + scenarios$Revenue_Growth)

scenarios$Cost_Y2 <- scenarios$Cost_Y1
scenarios$Cost_Y3 <- scenarios$Cost_Y1

scenarios$Total_Revenue <- scenarios$Revenue_Y1 + scenarios$Revenue_Y2 + scenarios$Revenue_Y3
scenarios$Total_Cost <- scenarios$Cost_Y1 + scenarios$Cost_Y2 + scenarios$Cost_Y3
scenarios$Total_Profit <- scenarios$Total_Revenue - scenarios$Total_Cost

scenarios$Net_Return <- scenarios$Total_Profit - scenarios$Initial_Investment
scenarios$ROI <- scenarios$Net_Return / scenarios$Initial_Investment

output <- scenarios[, c("Scenario", "Total_Profit", "Initial_Investment", "Net_Return", "ROI")]

output[, 2:5] <- round(output[, 2:5], 2)

print(output)

ggplot(scenarios, aes(x = Scenario, y = Net_Return, fill = Scenario)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Net Return Over 3 Years by Scenario",
       y = "Net Return ($)", x = "Scenario") +
  scale_fill_manual(values = c("red", "gray", "green"))




