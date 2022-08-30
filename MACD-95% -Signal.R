#https://www.youtube.com/watch?v=Ic8V1Pue-2I&ab_channel=%E6%8A%95%E6%9C%BA%E5%AE%9E%E9%AA%8C%E5%AE%A4
#MACD
#ATR
library(quantmod)
library(lubridate)
library(TTR)
library(zoo)
library(pkr)
stocknum = 2330
#basic information
x = get(getSymbols(paste0(stocknum,".TW"), from="2020-01-01", to=Sys.Date()))
chartSeries(x)
x = x[complete.cases(x),]
macd_data = MACD(x[,4], nFast = 13, nSlow = 34, percent = F)
macd = macd_data$macd - macd_data$signal
addMACD(fast = 13, slow = 34)
atr = ATR(x[,c(2, 3, 4)], n = 13)
bdata = na.omit(cbind(x, macd, atr))

#================================SIGNAL
#1. 抓低點
macdline = function(df, i){#虛心 or 實心
  if(as.numeric(df[i,7]) < 0){#negative MACD
    if(as.numeric(df[i,7]) <= as.numeric(df[(i-1),7])){
      return(0)#-下跌
    }
    else{
      return(1)#-上升
    } 
  }
  else{#positive MACD
    if(as.numeric(df[i,7]) > as.numeric(df[(i-1),7])){
      return(2)#+上升
    }
    else{
      return(3)#+下跌
    }
  }
}#抓3個頂點
sig = NULL
for(x in length(bdata[,1]):3){
  #上漲點
  if((macdline(bdata, x) == 1) & (macdline(bdata, (x-1)) == 0)){
    temp = c(x, 1)
    sig = rbind(sig, temp)
  }
  else if((macdline(bdata, x) == 3) & (macdline(bdata, (x-1)) == 2)){
    temp = c(x, 2)
    sig = rbind(sig, temp)
  }
  if(length(sig[,1]) == 3){
    break
  }
}


rep = 0
for(i in 1:2){
  #Sig1 same value
  if(sig[i,2] != sig[(i+1),2]){
    print("正負值不同")
    break
  }
  #Sig2 increase/decrease
  if(sig[i,2] == 1 & bdata[as.numeric(sig[i,1]),7] > bdata[as.numeric(sig[(i+1),1]),7]){#MACD為負數
    print("負數MACD沒有上升")
    break
    #Sig3 K_line
    slope1 = bdata[(as.numeric(sig[i,1]):as.numeric(sig[i+1,1])),3]
    sp = Slope((1:length(slope1[,1])), as.numeric(slope1))
    if(sp[6] > -0.2){
      print("MACD為負數，但價格非下跌趨勢")
      break
    }
  }
  else if(sig[i,2] == 2 & bdata[as.numeric(sig[i,1]),7] < bdata[as.numeric(sig[(i+1),1]),7]){#MACD為正數
    print("正數MACD沒有下降")
    break
    #Sig3 K_line
    slope1 = bdata[(as.numeric(sig[i,1]):as.numeric(sig[i+1,1])),2]
    sp = Slope((1:length(slope1[,1])), as.numeric(slope1))
    if(sp[6] < 0.2){
      print("MACD為正數，但價格非上升趨勢")
      break
    }
  }
  if(i == 2){
    rep = sig[i,2]
    gdsell2 = as.numeric(bdata[as.numeric(sig[i,1]),2])
    gdsell1 = as.numeric(bdata[as.numeric(sig[i,1]),3])
    inprice = as.numeric(bdata[as.numeric(sig[i,1]),4])
    print("買進")
  }
}

nowprice = as.numeric(bdata[i,4])#現價
#買進後行為
if(rep != 0){
  for(i in as.numeric(sig[1,1]):length(bdata[,1])){
    if(i == length(bdata[,1])){
      print("Keep holding")
      break
    }
    if(rep == 1){#做多
      qqsell = as.numeric(bdata[i,11])#止損
      gdsell1 = max(gdsell1, as.numeric(bdata[i,3]))
      if(nowprice < qqsell){
        print("SELL-QQ")
        price = nowprice - inprice
        break
      }
      else if(nowprice < gdsell1*0.98){
        print("SELL-GD")
        price = nowprice - inprice
        break
      }
    }
    else if(rep == 2){#做空
      qqsell = as.numeric(bdata[i,11])#止損
      gdsell2 = max(gdsell2, as.numeric(bdata[i,2]))
      if(nowprice > qqsell){
        print("SELL-QQ")
        price = nowprice - inprice
        break
      }
      else if(nowprice < gdsell2*0.98){
        print("SELL-GD")
        price = nowprice - inprice
        break
      }
    }
  }
}


