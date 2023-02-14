#--------------------Making Table of Ratios-----------------------------

#-----------------------------------------------------------------------
#--------------------------------Garch Model----------------------------
#-----------------------------------------------------------------------

library(rugarch)


start.date = as.POSIXct("2018-01-01")
end.date = as.POSIXct("2022-06-30")

# By using an index that is the logical AND of two vectors
c = complete_portfolio[start.date <= index(complete_portfolio) & index(complete_portfolio) <= end.date]

portfolio_sem_cleaned = na.locf(na.locf(c), fromLast = FALSE)
sum(is.na(portfolio_sem_cleaned))



portfolio_sem_cleaned[is.na(portfolio_sem_cleaned)] = 0
cleaned_portfolio = portfolio_sem_cleaned
sum(is.na(cleaned_portfolio))

cl = cleaned_portfolio[-1]


garch_spec = ugarchspec(variance.model = list(model = "sGARCH", 
                                              garchOrder = c(1,1)),
                        distribution.model = 'sstd', 
                        mean.model = list(armaOrder = c(0, 0)))



garch_model_fit = ugarchfit(garch_spec, data = complete_portfolio)


garch_model_fit



par(mfrow = c(2,3))

plot(garch_model_fit, which = 1)
plot(garch_model_fit, which = 3)
plot(garch_model_fit, which = 4)
plot(garch_model_fit, which = 5)
plot(garch_model_fit, which = 6)
plot(garch_model_fit, which = 12)
