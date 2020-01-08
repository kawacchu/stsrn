setwd("~/sotsuron")

library(sf)
library(tidyverse)
library(readxl)
library(spdep)
library(lmtest)
library(tseries)

options(stringsAsFactors = FALSE)
par(mar = c(0, 0, 0, 0))

#------------------------------#

# 小選挙区別のデータ
data <- read_xlsx("data.xlsx")
data$"65歳以上人口率(%)" = 100 * (data$"総数_（再掲）65歳以上" / data$"選挙当日の有権者数_計")

# 隣接行列をつくる
Japan <- st_read(dsn = "senkyoku289polygon.shp",
                 options = "ENCODING=CP932")

# inner_join のために列名を変更
colnames(Japan)[1] <- "code"

# 全国の投票率を可視化
Japan_ <- Japan %>% inner_join(data, by = "code") %>% st_as_sf(.)
plot(Japan_ %>% dplyr::select("投票率_計"))
plot(Japan_ %>% dplyr::select("65歳以上人口率(%)"))

# 隣接行列をつくる
contnb <- poly2nb(Japan_, queen = TRUE)
W <- nb2listw(contnb, glist = NULL, style = "W")

# 日本語は lm 関数の変数名に使えないので、列名を変える
names(Japan_)[16] <- "vote_rate"
names(Japan_)[28] <- "density"
names(Japan_)[468] <- "over65_rate"

# モデル作成
model1 <- lm(vote_rate ~ over65_rate, data = Japan_)
model1_ <- errorsarlm(formula = vote_rate ~ over65_rate, data = Japan_, listw = W)

# 結果
base::summary(model1)
bptest(model1)
jarque.bera.test(model1$residuals)
lm.morantest(model1, W, alternative = "two.sided")

model1_ <- errorsarlm(formula = vote_rate ~ over65_rate, data = Japan_, listw = W)
base::summary(model1_)
jarque.bera.test(model1_$residuals)
