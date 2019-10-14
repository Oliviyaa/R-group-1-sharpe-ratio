# 7.0 安装并调用所需包
install.packages("tidyverse")
install.packages("lubridate")
install.packages("readxl")
install.packages("highcharter")
install.packages("tidyquant")
install.packages("timetk")
install.packages("tibbletime")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("scales")
install.packages('data.table')
install.packages('dplyr')
install.packages('DBI')
install.packages('RMySQL')
install.packages('plyr')
install.packages('xts')

library(dplyr)
library(plyr)
library(tidyverse)
library(lubridate)
library(xts)
library(readxl)
library(highcharter)
library(quantmod)
library(tidyquant)
library(timetk)
library(tibbletime)
library(PerformanceAnalytics)
library(scales)
library(data.table)
library(DBI)
library(RMySQL)

# 从数据库下载数据
# —— 将指数作为研究的投资组合，选取上证指数、综合指数和创业板指数2013.01.01-2019.09.30的收盘价日数据
mydb= dbConnect(MySQL(),user='ktruc002', password='35442fed', dbname='cn_stock_quote', host='172.19.3.250')

## 提取上证指数数据至Indextable1
SQL_statement<- "SELECT  `trade_date`,  `index_code`, `last` 
                  FROM `cn_stock_index`.`daily_quote`
                  WHERE index_code='000001' and trade_date>'2013-01-01 00:00:00'
                  ORDER BY `trade_date` DESC, `index_name` DESC "
Indextable1 <- dbGetQuery(mydb,SQL_statement)

## 提取综合指数数据至Indextable2
SQL_statement<- "SELECT  `trade_date`,  `index_code`,  `last`
                  FROM `cn_stock_index`.`daily_quote`
                  WHERE index_code='000008'and trade_date>'2013-01-01 00:00:00'
                  ORDER BY `trade_date` DESC, `index_name` DESC "
Indextable2 <- dbGetQuery(mydb,SQL_statement)

## 提取创业板指数数据至Indextable3
SQL_statement<- "SELECT  `trade_date`,  `index_code`,  `last`
                  FROM `cn_stock_index`.`daily_quote`
                  WHERE index_code='399635'and trade_date>'2013-01-01 00:00:00'
                  ORDER BY `trade_date` DESC, `index_name` DESC "
Indextable3 <- dbGetQuery(mydb,SQL_statement)

## 将三张表按日期进行等值连接，对日期字段进行处理，生成时间序列
IndexTable<-inner_join(inner_join(Indextable1,Indextable2,by="trade_date"),Indextable3,by="trade_date")
data <- as.data.table(IndexTable)
date <- as.Date(data$trade)
data[,':='(date=date)]
table <- data[,!1]
setcolorder(table,c('date','index_code.x','last.x','index_code.y','last.y' ,'index_code','last'))

## 释放（删除）无用变量
rm(data,Indextable1,Indextable2,Indextable3,date)

## 得到原始数据表 xts_table,并将列变量名修改为“指数简称_指数代码”
xts_table <- as.xts.data.table(table) %>%
  rename(c(last.x='SZZS_000001',last.y='ZHZS_000008',last='CYBZ_399635'))
head(xts_table,5)
### save(xts_table,file="D://xtsdata.Rdata") #保存Rdata类型数据
### load("D://xtsdata.Rdata")  #自本地保存的rdata读取数据


## 相关参数设置
prices <- xts_table #指数价格序列
rfr<-0.03 #无风险收益率
symbols <- c("SZZS_000001","ZHZS_000008", "CYBZ_399635")#定义标志向量，即价格矩阵与收益率矩阵变量名

# 7.1-7.4 夏普比率
# 7.1 xts包
## 从日收盘价得到月收益率
## indexAt选择月初(firstog),月末(lastof)
prices_monthly <- to.monthly(prices,
                             indexAt="lastof",
                             OHLC=FALSE)

## 计算收益率,对数收益率(log)，若计算简单收益率（discrete)
asset_returns_xts <- Return.calculate(prices_monthly,method="log") %>% 
  na.omit()
head(asset_returns_xts,3)

## 计算夏普比率
sharpe_xts <-SharpeRatio(asset_returns_xts,
                         Rf = rfr,
                         FUN = "StdDev") %>%
  `colnames<-`(c("SZZS_000001","ZHZS_000008","CYBZ_399635"))
sharpe_xts <-data.table(sharpe_xts) %>%
  gather(asset, sharpe_xts)
sharpe_xts

#7.2 tidyverse包
## 计算月收益率
asset_returns_dplyr_byhand<-
  prices %>%
  to.monthly(indexAt="lastof", OHLC =FALSE) %>%
  # 将数据框索引转化为date日期序列
  data.frame(date= index(.)) %>%
  # 移除行名序列
  remove_rownames() %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  # 计算对数收益率
  mutate(returns =(log(prices)-log(lag(prices)))) %>%  
  select(-prices) %>%
  spread(asset, returns) %>%
  select(date, symbols) %>%
   #去除空值
  na.omit()
head(asset_returns_dplyr_byhand,3)

## 计算夏普比率 
sharpe_tidyverse<-
  asset_returns_dplyr_byhand %>%
  summarise(SZZS_000001 = mean(SZZS_000001 - rfr)/
              sd(SZZS_000001 - rfr),ZHZS_000008 = mean(ZHZS_000008 - rfr)/
              sd(ZHZS_000008- rfr),CYBZ_399635 = mean(CYBZ_399635- rfr)/
              sd(CYBZ_399635- rfr)) %>%
  gather(asset, sharpe_tidyverse)
sharpe_tidyverse

#7.3 tidyquant包
## 计算月收益率
asset_returns_tq_monthly<-
  prices %>%
  tk_tbl(preserve_index=TRUE,
         rename_index="date") %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  tq_transmute(mutate_fun=periodReturn,
               period ="monthly",
               type ="log") %>%
  # 剔除日期非共有的数据
  spread(asset, monthly.returns) %>%
  select(date, symbols) %>%
  slice(-1) %>% 
  # 调整数据格式
  gather(asset, returns, -date) %>%
  group_by(asset)
head(asset_returns_tq_monthly,3)

## 计算三个指数的夏普比率
sharpe_tq<-asset_returns_tq_monthly %>%
  tq_performance( Ra=returns,
                  performance_fun = SharpeRatio,
                  Rf = rfr,
                  FUN = "StdDev")
colnames(sharpe_tq) <- c("asset","sharpe_tq")
sharpe_tq

## 比较三种方法计算的同一指数夏普比率
IndexCompare<-inner_join(inner_join(sharpe_tq,
                                    sharpe_tidyverse,
                                    by="asset"),
                         sharpe_xts,
                         by="asset")
IndexCompare

## 计算同时期(2013.01.01-2019.09.30)S&P500的夏普比率
### 从yahoo获取S&P500收益率数据
SPY_returns_xts <-getSymbols("SPY",
                                src = 'yahoo',
                                from = "2013-01-01",
                                to = "2019-09-30",
                                auto.assign = TRUE,
                                warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`("SPY") %>%
  to.monthly(indexAt = "lastof",
             OHLC = FALSE)

### 计算夏普比率
sharpe_SPY <- SPY_returns_xts %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  mutate(returns =
           (log(SPY) - log(lag(SPY)))) %>%
  na.omit() %>%
  summarise(ratio =
              mean(returns - rfr)/sd(returns - rfr))

sharpe_SPY

#7.4 夏普比率可视化
## 7.4.1 区分上证指数收益率序列中 月收益率大于无风险利率与小于无风险利率的数据，分别输出
sharpe_byhand_with_return_columns <-
  asset_returns_dplyr_byhand %>%
  mutate(ratio =
           mean(SZZS_000001 - rfr)/sd(SZZS_000001 - rfr)) %>%
  mutate(returns_below_rfr =
           if_else(SZZS_000001 < rfr, SZZS_000001, as.numeric(NA))) %>%
  mutate(returns_above_rfr =
           if_else(SZZS_000001 > rfr,SZZS_000001, as.numeric(NA))) %>%
  mutate_if(is.numeric, funs(round(.,4)))
sharpe_byhand_with_return_columns %>%
  head(5)

## 7.4.2 创建散点图来了解高于无风险利率与低于无风险利率的上证指数月收益率
##      （紫色虚线代表了无风险利率，蓝线是日期为2016-06-30的垂直线）
sharpe_byhand_with_return_columns %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = returns_below_rfr),
             colour = "red") +
  geom_point(aes(y = returns_above_rfr),
             colour = "green") +
  geom_vline(xintercept =
               as.numeric(as.Date("2016-06-30")),
             color = "blue") +
  geom_hline(yintercept = rfr,
             color = "purple",
             linetype = "dotted") +
  annotate(geom = "text",
           x = as.Date("2016-06-30"),
           y = -.04,
           label = "Election",
           fontface = "plain",
           angle = 90,
           alpha = .5,
           vjust = 1.5) +
  ylab("percent monthly returns") +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_date(breaks = pretty_breaks( n = 8))

## 7.4.3 上证指数月收益率偏离无风险收益率的直方图（绿线代表了无风险收益率）
sharpe_byhand_with_return_columns %>%
  ggplot(aes(x = SZZS_000001)) +
  geom_histogram(alpha = 0.45,
                 binwidth = .01,
                 fill = "cornflowerblue") +
  geom_vline(xintercept = rfr,
             color = "green") +
  annotate(geom = "text",
           x = rfr,
           y = 13,
           label = "rfr",
           fontface = "plain",
           angle = 90,
           alpha = .5,
           vjust = 1)

## 7.4.4 标准差-夏普比率图像- 比较三个指数与S&P500指数的标准差与夏普比率
## 使用ggplot包

### 计算SPY的标准差
SPY_sd_xts <- 
  SPY_returns_xts %>% 
  Return.calculate(method="log") %>% 
  na.omit() %>%
  StdDev()

detach("package:plyr", unload = TRUE) #对sharpe_tq<-asset_returns_tq_monthly进行分组数据分析时成功运行需要先卸载plyr包
### 加入SPY数据点并画图
asset_returns_tq_monthly %>%
  group_by(asset) %>%
  summarise(stand_dev = sd(returns),
            sharpe = mean(returns - rfr)/
              sd(returns - rfr))%>%
  add_row(asset = "SPY",
          stand_dev =
            SPY_sd_xts[1],
          sharpe =
            sharpe_SPY$ratio) %>%
  ggplot(aes(x = stand_dev,
             y = sharpe,
             color = asset)) +
  geom_point(size = 2) +
  ylab("Sharpe Ratio") +
  xlab("standard deviation") +
  ggtitle("Sharpe Ratio versus Standard Deviation") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits = c(0,0.12)) +
  scale_y_continuous(limits = c(-0.7,-0.15))


# 7.5-7.8 滚动夏普比率
library(plyr)
# 7.5 xts包
## 设置滚动计算的窗宽为12个月
window <- 12

## 计算滚动夏普比率
rolling_sharpe_xts <-
  rollapply(asset_returns_xts, #在7.1中使用xts包计算得到的各指数收益率序列
            window,
            function(x)
              SharpeRatio(x,
                          Rf = rfr,
                          FUN = "StdDev")) %>%
  na.omit() %>%
  `colnames<-`(c("rol_sharpe_000001_xts","rol_sharpe_000008_xts", "rol_sharpe_399635_xts"))
head(rolling_sharpe_xts,5)

# 7.6 tidyverse和tibbletime包
## 定义滚动夏普比率计算函数
sharpe_roll_12 <-
  rollify(function(returns) {
    ratio = mean(returns - rfr)/sd(returns - rfr)
  },
  window = window)

## 计算滚动夏普比率
rolling_sharpe_tidy_tibbletime <-
  asset_returns_dplyr_byhand %>%  #在7.2中使用tidyverse包计算得到的各指数收益率序列
  as_tbl_time(index = date) %>%
  mutate(rol_sharpe_000001_tbltime = sharpe_roll_12(SZZS_000001),
         rol_sharpe_000008_tbltime = sharpe_roll_12(ZHZS_000008),
         rol_sharpe_399635_tbltime = sharpe_roll_12(CYBZ_399635)) %>%
  na.omit() %>%
  select(-SZZS_000001,-ZHZS_000008,-CYBZ_399635)
head(rolling_sharpe_tidy_tibbletime,5)

#7.7 tidyquant包
## 定义夏普比率计算函数
sharpe_tq_roll <- function(df){
  SharpeRatio(df,
              Rf = rfr,
              FUN = "StdDev")
}

## 计算滚动夏普比率
rolling_sharpe_tq <-
  asset_returns_tq_monthly %>%  #在7.3中使用tidyquant包计算得到的各指数收益率序列
  spread(asset, returns) %>%   #调整格式
  tq_mutate(
    select = symbols,
    mutate_fun = rollapply,
    width = window,
    align = "right",
    FUN = sharpe_tq_roll,
    col_rename = c("rol_sharpe_000001_tq","rol_sharpe_000008_tq", "rol_sharpe_399635_tq")
  ) %>%
  na.omit()
head(rolling_sharpe_tq,5)

## 对比三个包得到的计算结果
## 分别比较各指数三个包计算结果
rolling_sharpe_xts1 <- rolling_sharpe_xts %>% data.frame(date = index(.))
for(i in c("000001","000008","399635")) { 
  rolling_sharpe_temp <- 
    merge(rolling_sharpe_tidy_tibbletime[c("date",paste("rol_sharpe_", i,"_tbltime", sep = ""))],
          rolling_sharpe_tq[c("date",paste("rol_sharpe_", i,"_tq", sep = ""))],by="date")
  a <- paste("rolling_sharpe_", i,sep = "")
  assign(a, 
         merge(rolling_sharpe_temp,rolling_sharpe_xts1[c("date",paste("rol_sharpe_", i,"_xts", sep = ""))],
               by="date"))
}
head(rolling_sharpe_000001)
head(rolling_sharpe_000008)
head(rolling_sharpe_399635)

# 7.8 滚动夏普比率可视化
## highcharter和xts包
## 绘制各指数夏普比率在2014.01-2019.09的变化曲线
highchart(type = "stock") %>%
  hc_title(text = "Rolling 12-Month Sharpe") %>%
  hc_add_series(rolling_sharpe_xts$rol_sharpe_000001_xts,
                name = "sharpe_000001",
                color = "blue") %>%
  hc_add_series(rolling_sharpe_xts$rol_sharpe_000008_xts,
                name = "sharpe_000008",
                color = "red") %>%
  hc_add_series(rolling_sharpe_xts$rol_sharpe_399635_xts,
                name = "sharpe_399635",
                color = "black") %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE)

## ggplot包
### 绘制单指数的滚动夏普比率变化图
rolling_sharpe_xts$rol_sharpe_000001_xts %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  ggplot(aes(x = date,
             y = rol_sharpe_000001_xts)) +
  geom_line(color = "cornflowerblue") +
  ggtitle("Rolling 12-Month Sharpe Ratio") +
  labs(y = "rolling sharpe ratio") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))

### 绘制多指数的滚动夏普比率变化图
rolling_sharpe_xts_long <-
  rolling_sharpe_xts %>%
  data.frame(date = index(.)) %>%
  gather(asset, sharpe, -date) %>%
  group_by(asset)
rolling_sharpe_xts_long %>%
  ggplot(aes(x = date, y=sharpe,color= asset)) +
  geom_line() +
  ggtitle("Rolling 12-Month Sharpe Ratio") +
  labs(y = "rolling sharpe ratio") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))
