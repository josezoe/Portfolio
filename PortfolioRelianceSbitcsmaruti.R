library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(dygraphs)



# Creating portfolio of four Indian stocks
symbols=c("RELIANCE.BO","SBIN.BO","TCS.BO",'MARUTI.BO')


portfolio=lapply(symbols,function(x){
  dailyReturn(na.omit(getSymbols(x,from=as.Date("2017-01-01"),auto.assign = F)))
})

portfolio=do.call(merge.xts,portfolio)

portfolio
#             daily.returns daily.returns.1 daily.returns.2 daily.returns.3
# 2017-01-02  1.618347e-03   -0.0362017567   -2.515836e-03    2.192060e-02
# 2017-01-03  6.970777e-03    0.0051313628    4.005829e-03    8.086150e-03

# inorder to identify daily based on company let change the colnames



colnames(portfolio)=c("daily RELIANCE","daily SBIN","daily TCS",'dailyMARUTI')
#          daily RELIANCE    daily SBIN     daily TCS   dailyMARUTI
# 2017-01-02   1.618347e-03 -0.0362017567 -2.515836e-03  2.192060e-02
# 2017-01-03   6.970777e-03  0.0051313628  4.005829e-03  8.086150e-03


# assigning wt to portfolio to initiate 
port_wt=c(0.25,0.25,0.25,0.25)

port.port_wt=Return.portfolio(portfolio,weights = port_wt,rebalance_on = "months",verbose = T)
tail(port.port_wt)
# Return.portfolio returns contribution ,BOP,weight"balance of payments ",EOP "Endofplay


# Rebalnce on week
port.port_wt.week=Return.portfolio(portfolio,weights = port_wt,rebalance_on = "weeks",verbose = F)
tail(port.port_wt.week)
# tail(port.port_wt.week)
#                 portfolio.returns
# 2021-10-14      0.0025995253
# 2021-10-18      0.0122990860
# 2021-10-19     -0.0033324363
# 2021-10-20     -0.0009340991
# 2021-10-21     -0.0113086627
# 2021-10-22     -0.0073631694

# rebalnce based on year
port.port_wt.year=Return.portfolio(portfolio,weights = port_wt,rebalance_on = "year",verbose = F)
tail(port.port_wt.year)

# Verbose =T "return list of Intermediatiry value"
# 2017-12-04      0.4168401  0.3098912 0.2783668   0.3978846
# 2017-12-05      0.4214639  0.3158259 0.2784144   0.3971064
# 2017-12-06      0.4290933  0.3086053 0.5560254   0.4019556
# 2017-12-07      0.4303186  0.3131553 0.2767072   0.4150939
# 2017-12-08      0.4258335  0.3097428 0.2749471   0.4225604
# 2017-12-11      0.4234984  0.3148368 0.2807611   0.4272577
# 2017-12-12      0.4231054  0.3149357 0.2763372   0.4273232
# 2017-12-13      0.4226199  0.3098417 0.2777273   0.4258205
# 2017-12-14      0.4256485  0.3106330 0.2704598   0.4261640
# 2017-12-15      0.4253480  0.3093472 0.2693288   0.4283164
# 2017-12-18      0.4251168  0.3154303 0.2724101   0.4350491
# 2017-12-19      0.4272437  0.3159743 0.2722040   0.4582527
# 2017-12-20      0.4248162  0.3137982 0.2735729   0.4551282
# 2017-12-21      0.4221806  0.3131058 0.2742178   0.4500570
# 2017-12-22      0.4247006  0.3163699 0.2790486   0.4533802
# 2017-12-26      0.4290933  0.3134026 0.2793658   0.4520060
# 2017-12-27      0.4264577  0.3107319 0.2770666   0.4508983
# 2017-12-28      0.4274287  0.3049456 0.2778911   0.4501201
# 2017-12-29      0.4258797  0.3061325 0.2854545   0.4548337
# 2018-01-01      0.3642787  0.3651614 0.3618392   0.3655826
# 2018-01-02      0.3642188  0.3602260 0.3589905   0.3610324

# Verbose=F only calulation needed 
# port.port_wt.year=Return.portfolio(portfolio,weights = port_wt,rebalance_on = "year",verbose = F)
# > tail(port.port_wt.year)
# portfolio.returns
# 2021-10-14       0.003656101
# 2021-10-18       0.011898524
# 2021-10-19      -0.004808153
# 2021-10-20       0.001601046
# 2021-10-21      -0.010343683
# 2021-10-22      -0.005752525

# Benchmark Sensex or Nifty 
getSymbols("^NSEI",from=as.Date("2017-01-01"))
nifyreturn=Return.calculate(NSEI$NSEI.Close)
tail(nifyreturn)

#             NSEI.Close
# 2021-10-14  0.009734788
# 2021-10-18  0.007552396
# 2021-10-19 -0.003155308
# 2021-10-20 -0.008260625
# 2021-10-21 -0.004844908
# 2021-10-22 -0.003476668





# Information ratio indicating that if positive that portfolio is doing better than the sensex and like wise other way
# comparing portfolio with nify stock exchange

InformationRatio(port.port_wt$returns,nifyreturn)




# tracking ratio standard deviation between portfolio and benchmark

TrackingError(port.port_wt$returns,nifyreturn)


barplot(TrackingError(port.port_wt$returns,nifyreturn))
BetaCoVariance(port.port_wt$returns,nifyreturn)
BetaCoSkewness(port.port_wt$returns,nifyreturn)
plot(BetaCoKurtosis(port.port_wt$returns,nifyreturn))



# downside risk measure Var Value at risk 
VaR(na.omit(port.port_wt$returns))
# Leveraged inverse ETFs use the same concept as 
# leveraged products and aim to deliver a magnified 
# return when the market is falling

# > VaR(port.port_wt$returns)
# VaR calculation produces unreliable result 
# (inverse risk) for column: 1 : -0.00284610737722177

# Expected Shortfall ES 
ES(port.port_wt$returns)

optimport=portfolio.optim(portfolio)
portfolionewwt=optimport
volatility=StdDev(portfolio,portfolio_method = "component",weights = portfolionewwt))

# if we want and portfolio 10% return than mean 
optimumportfolio2=portfolio.optim(na.omit(portfolio),pm=1.1*mean(na.omit(portfolio)))
optimumportfolio2$pw
optimumportfolio2$pw

# [1] 0.4803010 0.1803839 0.1419336 0.1973815

# Adding meaningfullweights to the repective company

names(optimumportfolio2$pw)=c(" RELIANCE"," SBIN","TCS",'MARUTI')

# optimumportfolio2$pw
# RELIANCE      SBIN       TCS    MARUTI 



# portforlio optimization 2 with constrainsts
port_spec=portfolio.spec(colnames(portfolio))
port_spec


# Constarinst 1 
port_spec=add.constraint(portfolio = port_spec,type = "full_investment")

# Constarint 2
port_spec2=add.constraint(portfolio=port_spec,type="long_only")


# obejective 1 maximize porforlio return
port_spec_max=add.objective(portfolio = port_spec,type = "return",name="mean")
port_spec_max

# obejective 2 minimizing risk of  porforlio means minizing standard deviation
port_spec_min=add.objective(portfolio = port_spec,type = "risk",name="StdDev")


optimumfinal=optimize.portfolio(na.omit(portfolio),
                                portfolio = port_spec,
                                optimize_method = "random ",trace = T)


optimumfinal
