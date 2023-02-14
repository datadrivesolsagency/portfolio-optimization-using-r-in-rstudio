
start.date = as.POSIXct("2018-01-01")
end.date = as.POSIXct("2018-12-01")

# By using an index that is the logical AND of two vectors
c = complete_portfolio[start.date <= index(complete_portfolio) & index(complete_portfolio) <= end.date]

portfolio_sem_cleaned = na.locf(na.locf(c), fromLast = FALSE)
sum(is.na(portfolio_sem_cleaned))



portfolio_sem_cleaned[is.na(portfolio_sem_cleaned)] = 0
cleaned_portfolio1 = portfolio_sem_cleaned
sum(is.na(cleaned_portfolio1))

#ASsigning Column names to portfolios

colnames(cleaned_portfolio1) <- c('Crypto','Treasury','NASDAQ','Gold')

#-------------------------------------------------------------------

#Asset Allocation 1


asset_allocation_con1 = c('Crypto'= 0,'Treasury'= 0.3, 'NASDAQ' = 0.2, 'Gold' = 0.5)

asset_allocation_1_con_return = Return.portfolio(cleaned_portfolio1, weights = asset_allocation_con1,
                                                 rebalance_on = 'months')
colnames(asset_allocation_1_con_return) = c("Asset Allocation 1 Conservative")

summary(asset_allocation_1_con_return)


charts.PerformanceSummary(asset_allocation_1_con_return, main = 
                            'Asset Allocation 1 Conservative Strategy')


#Sharpe Ratio


print('Sharpe Ratio of the Portfolio is')
SharpeRatio(asset_allocation_1_con_return)


#Sortino Ratio

SortinoRatio(asset_allocation_1_con_return)

#VaR

VaR(asset_allocation_1_con_return)

#Skewness
skewness(asset_allocation_1_con_return)

#Kurtosis
kurtosis(asset_allocation_1_con_return)


asset_allocation_mod1 = c('Crypto'= 0,'Treasury'= 0.4, 'NASDAQ' = 0.4, 'Gold' = 0.2)

asset_allocation_1_mod_return = Return.portfolio(cleaned_portfolio1, weights = asset_allocation_mod1,
                                                 rebalance_on = 'months')

colnames(asset_allocation_1_mod_return) = c("Asset Allocation 1 Moderate")

summary(asset_allocation_1_mod_return)


charts.PerformanceSummary(asset_allocation_1_mod_return, main = 
                            'Asset Allocation 1 Moderate Strategy')


#Sharpe Ratio


print('Sharpe Ratio of the Portfolio is')
SharpeRatio(asset_allocation_1_mod_return)


#Sortino Ratio

SortinoRatio(asset_allocation_1_mod_return)

#VaR

VaR(asset_allocation_1_mod_return)

#Skewness
skewness(asset_allocation_1_mod_return)

#Kurtosis
kurtosis(asset_allocation_1_mod_return)


asset_allocation_agg1 = c('Crypto'= 0,'Treasury'= 0.3, 'NASDAQ' = 0.65, 'Gold' = 0.05)

asset_allocation_1_agg_return = Return.portfolio(cleaned_portfolio1, weights = asset_allocation_agg1,
                                                 rebalance_on = 'months')

colnames(asset_allocation_1_agg_return) = c("Asset Allocation 1 Aggressive")

summary(asset_allocation_1_agg_return)


charts.PerformanceSummary(asset_allocation_1_agg_return, main = 
                            'Asset Allocation 1 Aggressive Strategy')


#Sharpe Ratio


print('Sharpe Ratio of the Portfolio is')
SharpeRatio(asset_allocation_1_agg_return)


#Sortino Ratio

SortinoRatio(asset_allocation_1_agg_return)

#VaR

VaR(asset_allocation_1_agg_return)

#Skewness
skewness(asset_allocation_1_agg_return)

#Kurtosis
kurtosis(asset_allocation_1_agg_return)

#Visualoze overall Return of Asset Allocation 1 Strategy

charts.PerformanceSummary(merge(asset_allocation_1_con_return, asset_allocation_1_mod_return,
                                asset_allocation_1_agg_return), main = 'Asset Allocation 1')

asset_allocation_1 = merge(asset_allocation_1_con_return, asset_allocation_1_mod_return,
                           asset_allocation_1_agg_return)



#Asset Allocation 2


asset_allocation_con2 = c('Crypto'= 0,'Treasury'= 0.35, 'NASDAQ' = 0.45, 'Gold' = 0.20)

asset_allocation_2_con_return = Return.portfolio(cleaned_portfolio1, weights = asset_allocation_con2,
                                                 rebalance_on = 'months')
colnames(asset_allocation_2_con_return) = c("Asset Allocation 2 Conservative")

summary(asset_allocation_2_con_return)


charts.PerformanceSummary(asset_allocation_2_con_return, main = 
                            'Asset Allocation 2 Conservative Strategy')


#Sharpe Ratio


print('Sharpe Ratio of the Portfolio is')
SharpeRatio.annualized(asset_allocation_2_con_return)


#Sortino Ratio

SortinoRatio(asset_allocation_2_con_return)

#VaR

VaR(asset_allocation_2_con_return)

#Skewness
skewness(asset_allocation_2_con_return)

#Kurtosis
kurtosis(asset_allocation_2_con_return)


asset_allocation_mod2 = c('Crypto'= 0,'Treasury'= 0.40, 'NASDAQ' = 0.55, 'Gold' = 0.05)

asset_allocation_2_mod_return = Return.portfolio(cleaned_portfolio1, weights = asset_allocation_mod2,
                                                 rebalance_on = 'months')

colnames(asset_allocation_2_mod_return) = c("Asset Allocation 2 Moderate")

summary(asset_allocation_2_mod_return)


charts.PerformanceSummary(asset_allocation_2_mod_return, main = 
                            'Asset Allocation 2 Moderate Strategy')


#Sharpe Ratio


print('Sharpe Ratio of the Portfolio is')
SharpeRatio.annualized(asset_allocation_2_mod_return)


#Sortino Ratio

SortinoRatio(asset_allocation_2_mod_return)

#VaR

VaR(asset_allocation_2_mod_return)

#Skewness
skewness(asset_allocation_2_mod_return)

#Kurtosis
kurtosis(asset_allocation_2_mod_return)


asset_allocation_agg2 = c('Crypto'= 0,'Treasury'= 0.20, 'NASDAQ' = 0.75, 'Gold' = 0.05)

asset_allocation_2_agg_return = Return.portfolio(cleaned_portfolio1, weights = asset_allocation_agg2,
                                                 rebalance_on = 'months')

colnames(asset_allocation_2_agg_return) = c("Asset Allocation 2 Aggressive")

summary(asset_allocation_2_agg_return)


charts.PerformanceSummary(asset_allocation_2_agg_return, main = 
                            'Asset Allocation 2 Aggressive Strategy')


#Sharpe Ratio


print('Sharpe Ratio of the Portfolio is')
SharpeRatio.annualized(asset_allocation_2_agg_return)


#Sortino Ratio

SortinoRatio(asset_allocation_2_agg_return)

#VaR

VaR(asset_allocation_2_agg_return)

#Skewness
skewness(asset_allocation_2_agg_return)

#Kurtosis
kurtosis(asset_allocation_2_agg_return)

#Visualoze overall Return of Asset Allocation 1 Strategy

charts.PerformanceSummary(merge(asset_allocation_2_con_return, asset_allocation_2_mod_return,
                                asset_allocation_2_agg_return), main = 'Asset Allocation 2')


asset_allocation_2 = merge(asset_allocation_2_con_return, asset_allocation_2_mod_return,
                           asset_allocation_2_agg_return)


#Asset Allocation 3


asset_allocation_con3 = c('Crypto'= 0.05,'Treasury'= 0.30, 'NASDAQ' = 0.15, 'Gold' = 0.5)

asset_allocation_3_con_return = Return.portfolio(cleaned_portfolio1, weights = asset_allocation_con3,
                                                 rebalance_on = 'months')
colnames(asset_allocation_3_con_return) = c("Asset Allocation 3 Conservative")

summary(asset_allocation_3_con_return)


charts.PerformanceSummary(asset_allocation_3_con_return, main = 
                            'Asset Allocation 3 Conservative Strategy')


#Sharpe Ratio


print('Sharpe Ratio of the Portfolio is')
SharpeRatio(asset_allocation_3_con_return)


#Sortino Ratio

SortinoRatio(asset_allocation_3_con_return)

#VaR

VaR(asset_allocation_3_con_return)

#Skewness
skewness(asset_allocation_3_con_return)

#Kurtosis
kurtosis(asset_allocation_3_con_return)


asset_allocation_mod3 = c('Crypto'= 0.25,'Treasury'= 0.40, 'NASDAQ' = 0.25, 'Gold' = 0.10)

asset_allocation_3_mod_return = Return.portfolio(cleaned_portfolio1, weights = asset_allocation_mod3,
                                                 rebalance_on = 'months')

colnames(asset_allocation_3_mod_return) = c("Asset Allocation 3 Moderate")

summary(asset_allocation_3_mod_return)


charts.PerformanceSummary(asset_allocation_3_mod_return, main = 
                            'Asset Allocation 3 Moderate Strategy')


#Sharpe Ratio


print('Sharpe Ratio of the Portfolio is')
SharpeRatio(asset_allocation_3_mod_return)


#Sortino Ratio

SortinoRatio(asset_allocation_3_mod_return)

#VaR

VaR(asset_allocation_3_mod_return)

#Skewness
skewness(asset_allocation_3_mod_return)

#Kurtosis
kurtosis(asset_allocation_3_mod_return)


asset_allocation_agg3 = c('Crypto'= 0.6,'Treasury'= 0, 'NASDAQ' = 0.40, 'Gold' = 0)

asset_allocation_3_agg_return = Return.portfolio(cleaned_portfolio1, weights = asset_allocation_agg3,
                                                 rebalance_on = 'months')

colnames(asset_allocation_3_agg_return) = c("Asset Allocation 3 Aggressive")

summary(asset_allocation_3_agg_return)


charts.PerformanceSummary(asset_allocation_3_agg_return, main = 
                            'Asset Allocation 3 Aggressive Strategy')


#Sharpe Ratio


print('Sharpe Ratio of the Portfolio is')
SharpeRatio(asset_allocation_3_agg_return)


#Sortino Ratio

SortinoRatio(asset_allocation_3_agg_return)

#VaR

VaR(asset_allocation_3_agg_return)

#Skewness
skewness(asset_allocation_3_agg_return)

#Kurtosis
kurtosis(asset_allocation_3_agg_return)

#Visualoze overall Return of Asset Allocation 1 Strategy

charts.PerformanceSummary(merge(asset_allocation_3_con_return, asset_allocation_3_mod_return,
                                asset_allocation_3_agg_return), main = 'Asset Allocation 3')

asset_allocation_3 = merge(asset_allocation_3_con_return, asset_allocation_3_mod_return,
                           asset_allocation_3_agg_return)


#Asset Allocation 4


asset_allocation_con4 = c('Crypto'= 0.1,'Treasury'= 0.40, 'NASDAQ' = 0.3, 'Gold' = 0.2)

asset_allocation_4_con_return = Return.portfolio(cleaned_portfolio1, weights = asset_allocation_con4,
                                                 rebalance_on = 'months')
colnames(asset_allocation_4_con_return) = c("Asset Allocation 4 Conservative")

summary(asset_allocation_4_con_return)


charts.PerformanceSummary(asset_allocation_4_con_return, main = 
                            'Asset Allocation 4 Conservative Strategy')


#Sharpe Ratio


print('Sharpe Ratio of the Portfolio is')
SharpeRatio(asset_allocation_4_con_return)


#Sortino Ratio

SortinoRatio(asset_allocation_4_con_return)

#VaR

VaR(asset_allocation_4_con_return)

#Skewness
skewness(asset_allocation_4_con_return)

#Kurtosis
kurtosis(asset_allocation_4_con_return)


asset_allocation_mod4 = c('Crypto'= 0.3,'Treasury'= 0.30, 'NASDAQ' = 0.3, 'Gold' = 0.1)

asset_allocation_4_mod_return = Return.portfolio(cleaned_portfolio1, weights = asset_allocation_mod4,
                                                 rebalance_on = 'months')

colnames(asset_allocation_4_mod_return) = c("Asset Allocation 4 Moderate")

summary(asset_allocation_4_mod_return)


charts.PerformanceSummary(asset_allocation_4_mod_return, main = 
                            'Asset Allocation 4 Moderate Strategy')


#Sharpe Ratio


print('Sharpe Ratio of the Portfolio is')
SharpeRatio(asset_allocation_4_mod_return)


#Sortino Ratio

SortinoRatio(asset_allocation_4_mod_return)

#VaR

VaR(asset_allocation_4_mod_return)

#Skewness
skewness(asset_allocation_4_mod_return)

#Kurtosis
kurtosis(asset_allocation_4_mod_return)


asset_allocation_agg4 = c('Crypto'= 0.48,'Treasury'= 0.2, 'NASDAQ' = 0.32, 'Gold' = 0)

asset_allocation_4_agg_return = Return.portfolio(cleaned_portfolio1, weights = asset_allocation_agg4,
                                                 rebalance_on = 'months')

colnames(asset_allocation_4_agg_return) = c("Asset Allocation 4 Aggressive")

summary(asset_allocation_4_agg_return)


charts.PerformanceSummary(asset_allocation_4_agg_return, main = 
                            'Asset Allocation 4 Aggressive Strategy')


#Sharpe Ratio


print('Sharpe Ratio of the Portfolio is')
SharpeRatio(asset_allocation_4_agg_return)


#Sortino Ratio

SortinoRatio(asset_allocation_4_agg_return)

#VaR

VaR(asset_allocation_4_agg_return)

#Skewness
skewness(asset_allocation_4_agg_return)

#Kurtosis
kurtosis(asset_allocation_4_agg_return)

#Visualoze overall Return of Asset Allocation 1 Strategy

charts.PerformanceSummary(merge(asset_allocation_4_con_return, asset_allocation_4_mod_return,
                                asset_allocation_4_agg_return), main = 'Asset Allocation 4')

asset_allocation_4 = merge(asset_allocation_4_con_return, asset_allocation_4_mod_return,
                           asset_allocation_4_agg_return)


asset_allocation_returns_all = cbind(asset_allocation_1, asset_allocation_2, asset_allocation_3,
                                     asset_allocation_4)

#----------------------Summary of Asset Allocation 2018---------------------------------

Asset_Allocation_summary = table.AnnualizedReturns(asset_allocation_returns_all, Rf = 0.0000717)
Asset_Allocation_summary
A1 = data.table::as.data.table(Asset_Allocation_summary)
#----------------------Portfolio Optimization for this period---------------------------



#Optimization portfolio using random

funds = colnames(cleaned_portfolio1)

initial_port <- portfolio.spec(assets = funds)
initial_port <- add.constraint(portfolio = initial_port, type = "full_investment")
initial_port <- add.constraint(portfolio = initial_port, type = "long_only")
initial_port <- add.objective(portfolio = initial_port, type = 'return', name = 'mean')

initial_port


#The minimum sum of full investment must be 0.99

initial_port$constraints[[1]]$min_sum = 0.99
initial_port$constraints[[1]]$max_sum = 1.01

#Adding aother constraint of risk in our portfolio

initial_port <- add.constraint(portfolio = initial_port, type = 'risk', name = 'StdDev',
                               multiplier = 0)

c.min = rep(0.05, ncol(cleaned_portfolio))
c.max = rep(0.50, ncol(cleaned_portfolio))

#

initial_port = add.constraint(portfolio = initial_port, type = 'box', enabled = TRUE, min=c.min, max = c.max)

first_port <- add.constraint(portfolio = initial_port, type = 'diversification',
                             min = 0, max = 1, indexnum = 2)

first_port <- add.constraint(portfolio = initial_port, type = 'risk', name = 'StdDev')



#Optimize the portfolio

#source('optimizer_func.R')

maxSrpot <- optimize.portfolio(R = cleaned_portfolio1, portfolio = first_port,
                               optimize_method = 'random', search_size = 2000,
                               maxSR = TRUE, trace = TRUE)

maxSrpot


wegiths = extractWeights(maxSrpot)

optimized_weights_random = data.table::as.data.table(wegiths)

port_return = Return.portfolio(cleaned_portfolio, weights = wegiths)

colnames(port_return) = c('Random Portfolio Return')

charts.PerformanceSummary(port_return)

#Optimization using ROI


initial_port1 <- portfolio.spec(assets = funds)
initial_port1 <- add.constraint(portfolio = initial_port1, type = "full_investment")
initial_port1 <- add.constraint(portfolio = initial_port1, type = "long_only")
initial_port1 <- add.objective(portfolio = initial_port1, type = 'return', name = 'mean')

initial_port1


#The minimum sum of full investment must be 0.99

initial_port1$constraints[[1]]$min_sum = 1
initial_port1$constraints[[1]]$max_sum = 1

#Adding aother constraint of risk in our portfolio

initial_port1 <- add.constraint(portfolio = initial_port1, type = 'risk', name = 'StdDev',
                                multiplier = 0)

c.min = rep(0.05, ncol(cleaned_portfolio))
c.max = rep(0.50, ncol(cleaned_portfolio))

#

initial_port1 = add.constraint(portfolio = initial_port1, type = 'box', enabled = TRUE, min=c.min, max = c.max)


first_port1 <- add.constraint(portfolio = initial_port1, type = 'diversification',
                              min = 0, max = 1, indexnum = 2)

first_port1 <- add.constraint(portfolio = initial_port1, type = 'risk', name = 'StdDev')


first_port1


maxSrpot.roi <- optimize.portfolio(R = cleaned_portfolio1, portfolio = first_port1,
                                   optimize_method = 'ROI', search_size = 2000,
                                   maxSR = TRUE, trace = TRUE)
maxSrpot.roi

wegiths.roi = extractWeights(maxSrpot.roi)

sum(wegiths.roi)

port_return.roi = Return.portfolio(cleaned_portfolio, weights = wegiths.roi)

colnames(port_return.roi) = c('ROI Portfolio Return')

charts.PerformanceSummary(port_return.roi)


com = cbind(port_return, port_return.roi)

#The top chart in the graph gives the cumulative return while bottom gives the daily return

charts.PerformanceSummary(com, main = 'Comparison of Charts return using
                          two different optimization techniques', plot.engine = 'plotly')

table_ratios = table.AnnualizedReturns(com, Rf = 0.0000717)

#Converting it into the dataframe

table_ratios_df = data.table::as.data.table(table_ratios) 




#--------------------------------Garch Modeling---------------------------

library(rugarch)

chartSeries(cleaned_portfolio1)

chartSeries(cleaned_portfolio1['2018'])

chart_Series(cleaned_portfolio1)


chart.Histogram(cleaned_portfolio1, methods = c('add.density','add.normal')
                ,colorset = c('red','blue','black'))
legend('topright', legend = c('return','kernel','normal.dist'), fill = c('red','blue','black'))




#-------------Calculating the annualized volatility--------------------

sd(cleaned_portfolio1)
sqrt(252)*sqrt(cleaned_portfolio1['2018'])






#--------------------Univariate Analysis of the Crypto portfolio using GARCH



garch_spec = ugarchspec(mean.model = list(armaOrder = c(1,0)))



garch_model_fit = ugarchfit(garch_spec, data = crypto_port_return)


garch_model_fit



plot(garch_model_fit, which = 1)
plot(garch_model_fit, which = 3)
plot(garch_model_fit, which = 4)
plot(garch_model_fit, which = 5)
plot(garch_model_fit, which = 6)
plot(garch_model_fit, which = 12)



#Extracting the coefficients
garch_model_fit@fit$coef

garch_var_cr = garch_model_fit@fit$var
garch_res = (garch_model_fit@fit$residuals) ^2

plot(garch_res, type = 'l')
lines(garch_var_cr, col = 'green')


#-------------------Model forecasting---------------------------

garchfor = ugarchforecast(garch_model_fit, n.ahead = 10)
garchfor

garrch_for = garchfor@forecast$sigmaFor
plot(garrch_for, type = 'l') #for next ten periods the volatity is increasing


garchfor1 = ugarchforecast(garch_model_fit, n.ahead = 100)
garchfor1

garrch_for1 = garchfor1@forecast$sigmaFor
plot(garrch_for1, type = 'l')


#-----------------Multivariate Analysis of Whole Portfolio-------

library(rugarch)
library(rmgarch)

garch_m_spec = multispec(replicate(4, ugarchspec(mean.model = list(armaOrder = c(1,0)))))

multi_fit_garch = multifit(garch_m_spec, cleaned_portfolio1)
multi_fit_garch

 
#---------------SPecs for Correlation Part-----------------------


spec1 = dccspec(uspec = garch_m_spec, dccOrder = c(1,1), distribution = 'mvnorm')

fit1 = dccfit(spec1, data = cleaned_portfolio1, fit.control = list(eval.se = TRUE),
              fit = multi_fit_garch)

fit1



cov1 = rcov(fit1) #Extract Covariance Matrix
cor1 = rcor(fit1) #Extract correlation martrix
dim(cor1)


cor1[,,dim(cor1)[3]]

cor_bg = cor1[4,1,]

cor_bg_ts = as.xts(cor_bg)

plot(cor_bg_ts)

#Correlation between different Assets

plot(as.xts(cor1[1,2,]), main = 'Crypto & Treasury')
plot(as.xts(cor1[1,3,]), main = 'Crypto & NASDAQ')
plot(as.xts(cor1[1,4,]), main = 'Crypto & Gold')


#Correlation between Treasury and Others
plot(as.xts(cor1[2,3,]), main = 'Treasury & NASDAQ')
plot(as.xts(cor1[2,4,]), main = 'Treasury & Gold')

#Correlation between NASDAQ and Gold

plot(as.xts(cor1[3,4,]), main = 'NASDAQ & Gold')



#-----------------------Forecasting-------------------------------


dcfit1_m = dccforecast(fit = fit1, n.head = 100)
dcfit1_m


#-----------------Acutal forecast for correlation----------------

correlation_forecast <- dcfit1_m@mforecast$R #using R for correlation forecast
covaraince_forecast <- dcfit1_m@mforecast$H #using H for covariance forecast



corf_CT <- correlation_forecast[[1]][1,2,]  # Correlation forecasts between 
corf_CN <- correlation_forecast[[1]][1,3,]  # Correlation forecasts between 
corf_CG <- correlation_forecast[[1]][1,4,]  # Correlation forecasts between


corf_TN <- correlation_forecast[[1]][2,3,]  # Correlation forecasts between 
corf_TG <- correlation_forecast[[1]][2,4,]  # Correlation forecasts between


corf_NG <- correlation_forecast[[1]][3,4,]  # Correlation forecasts between

#Crypto and Treasury

c_CT <- c(tail(cor1[1,2,],20),rep(NA,10))  # gets the last 20 correlation observations

cf_CT <- c(rep(NA,20),corf_CT) # gets the 10 forecasts
plot(c_CT,type = "l",main="Correlation Crypto and Treasury")
lines(cf_CT,type = "l", col = "orange")


#Crypto and NASDAQ

c_CN <- c(tail(cor1[1,3,],20),rep(NA,10))  # gets the last 20 correlation observations

cf_CN <- c(rep(NA,20),corf_CN) # gets the 10 forecasts

plot(c_CN,type = "l",main="Correlation Crypto and NASDAQ")
lines(cf_CN,type = "l", col = "orange")



#Crypto and Gold

c_CG <- c(tail(cor1[1,4,],20),rep(NA,10))  # gets the last 20 correlation observations

cf_CG <- c(rep(NA,20),corf_CN) # gets the 10 forecasts

plot(c_CG,type = "l",main="Correlation Crypto and Gold")
lines(cf_CG,type = "l", col = "orange")


#-------------------------------------------------------------------
#Treasury and NASDAQ

c_TN <- c(tail(cor1[2,3,],20),rep(NA,10))  # gets the last 20 correlation observations

cf_TN <- c(rep(NA,20),corf_TN) # gets the 10 forecasts

plot(c_TN,type = "l",main="Correlation Treasury and NASDAQ")
lines(cf_TN,type = "l", col = "orange")



#Treasury and Gold

c_TG <- c(tail(cor1[2,4,],20),rep(NA,10))  # gets the last 20 correlation observations

cf_TG <- c(rep(NA,20),corf_TG) # gets the 10 forecasts

plot(c_TG,type = "l",main="Correlation Treasury and Gold")
lines(cf_TG,type = "l", col = "orange")



#---------------------NASQAQ and Gold



c_NG <- c(tail(cor1[3,4,],20),rep(NA,10))  # gets the last 20 correlation observations

cf_NG <- c(rep(NA,20),corf_NG) # gets the 10 forecasts

plot(c_TG,type = "l",main="Correlation NASDAQ and Gold")

lines(cf_NG,type = "l", col = "orange")









