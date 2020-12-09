rm(list=ls())

library(sqldf)
library(Hmisc)
library(xts)
library(PerformanceAnalytics)
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))  
setwd('..')
setwd('./data/')

tickers <- c("SNP","PTR",'RDS',"BP","XOM","TOT","CVX","MPC","LUKOY")
comp_names <- c("China Petroleum & Chemical","PetroChina","Royal Dutch Shell PLC", 
                "BP PLC", "Exxon Mobil Corp.","Total SE", "Chevron Corp.", "Marathon Petroleum Corp.", "PJSC Lukoil")

n <- length(tickers)
date_start = as.numeric(as.Date('2015-01-01'))
date_end = as.numeric(as.Date('2020-12-01'))
beta <- array(dim=9)

# Load S&P
gspcfile <- data.frame(read.csv('GSPC.csv')) 

# S&P Fix date
gspcfile$Date <- as.numeric(as.Date(gspcfile$Date))
gspc <- sqldf(paste('SELECT DATE as Date, CLOSE FROM gspcfile WHERE DATE BETWEEN ',date_start, ' AND ',date_end,sep=""))

# S&P Fix date back
gspc[1] <- lapply(gspc[1], as.numeric)
gspc[1] <- lapply(gspc[1], as.Date)

gspc[[2]] <- xts(gspc[[2]], order.by=gspc[[1]])
gspc_returns <- as.data.frame(Return.calculate(gspc[[2]]))

# Load stock prices
for (i in 1:n) {
  filename <- paste(tickers[i],'.csv', sep="")
  stockfiles <- data.frame(read.csv(filename))
  stockfiles$Date <- as.numeric(as.Date(stockfiles$Date))
  temp_table <- sqldf(paste('SELECT DATE as Date',i,', CLOSE as Close',i,' FROM stockfiles WHERE DATE BETWEEN ',date_start, ' AND ',date_end,sep=""))
  temp_table[1] <- lapply(temp_table[1], as.numeric)
  temp_table[1] <- lapply(temp_table[1], as.Date)
  temp_table[[2]] <- xts(temp_table[[2]], order.by = temp_table[[1]])
  if (i==1)
    stockdata <- as.data.frame(Return.calculate(temp_table[[2]]))
  else
    stockdata <- cbind(stockdata,as.data.frame(Return.calculate(temp_table[[2]])))
#  stockdata[i*2 - 1] <- lapply(stockdata[i*2 - 1], as.numeric)
#  stockdata[i*2 - 1] <- lapply(stockdata[i*2 - 1], as.Date)
  model <- lm(stockdata[[i]] ~ gspc_returns[[1]])
  beta[i] <- round(model$coef[2],2)
}

# Load table from arrays
beta_df <- data.frame(comp_names)
beta_df <- cbind(beta_df, as.data.frame(beta))
beta_df <- cbind(beta_df, as.data.frame(tickers))

r_m = as.double(gspc$Close[gspc$Date=='2020-12-01'])/as.double(gspc$Close[gspc$Date=='2019-12-02']) -1

r_e <- data.frame(round(beta_df$beta * r_m,2))
# make latex table
latex(beta_df, file='beta.tex', caption = "Beta coefficients")

r_e <- cbind(as.data.frame(comp_names),r_e)
latex(r_e, file='r_e.tex', caption = "Cost of Capital")



# make graph
adj <- ifelse(comp_names=='PJSC Lukoil',2,1)
g <- ggplot(beta_df,aes(x = tickers, y=beta, fill = "fruit")) + 
     geom_bar(stat='identity') + 
  #   geom_text(aes(label=comp_names),hjust=0.5, vjust=ifelse(comp_names=='PJSC Lukoil',-1,1.5)) + 
     theme(axis.title = element_blank(), legend.position = "none") 
  #   scale_x_discrete(name = element_blank()) +
   #  theme(panel.background=element_blank()) + 
  #   scale_y_continuous(breaks = seq(0.8, 1.6, by = 0.1))


  
print(g)



