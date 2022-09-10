#https://www.youtube.com/watch?v=Ic8V1Pue-2I&ab_channel=%E6%8A%95%E6%9C%BA%E5%AE%9E%E9%AA%8C%E5%AE%A4
#MACD
#ATR
library(quantmod)
library(lubridate)
library(TTR)
library(zoo)
library(pkr)
library(stats)
stock = get(getSymbols("AAPL", from="2015-01-01", to=Sys.Date()))


main = function(stocknum, holdm){
  
  #------------------------------------------ 1.基本資訊
  x = stocknum[complete.cases(stocknum),]
  macd_data = MACD(x[,4], nFast = 13, nSlow = 34, percent = F)
  macd = macd_data$macd - macd_data$signal
  atr = ATR(x[,c(2, 3, 4)], n = 13)
  bdata = na.omit(cbind(x, macd, atr))
  bdata = bdata[-(length(bdata[,1]):length(bdata[,1])),]
  colnames(bdata) = c("O", "H", "L", "C", "V", "A", "macd", "tr", "atr", "TH", "TL")
  
  #------------------------------------------ 2.進場條件信號
  
  #MACD
  macdline = function(df, i){#虛心 or 實心
    if(as.numeric(df[i,7]) < 0){#negative MACD
      if(as.numeric(df[i,7]) <= as.numeric(df[(i-1),7])){
        return(0)#-下跌 L
      }
      else{
        return(1)#-上升 R
      } 
    }
    else{#positive MACD
      if(as.numeric(df[i,7]) > as.numeric(df[(i-1),7])){
        return(2)#+上升 L
      }
      else{
        return(3)#+下跌 R
      }
    }
  }
  
  #3signals
  sig = NULL#3個低點
  for(x in length(bdata[,1]):3){
    #上漲點
    if((macdline(bdata, x) == 1) & (macdline(bdata, (x-1)) == 0)){
      temp = c(x, 1)
      sig = rbind(sig, temp)
    }
    #下跌點
    else if((macdline(bdata, x) == 3) & (macdline(bdata, (x-1)) == 2)){
      temp = c(x, 2)
      sig = rbind(sig, temp)
    }
    if(length(sig[,1]) == 3){
      break
    }
  }
  
  #如果訊號不足 直接跳
  if(length(sig[,1])!=3){
    return(holdm)
  }
  
  for(ai in 1:2){
    if((holdm == 0) & (sig[1,1] == length(bdata[,1]))){#現在沒有持倉 且在訊號時間點
      #rep = 0#操作的信號 0無動作1多2空
      #Sig1 same value
      if(sig[ai,2] != sig[(ai+1),2]){
        print("正負值不同")
        return(0)
      }
      #Sig2 increase/decrease
      if(sig[ai,2] == 1 & as.numeric(bdata[as.numeric(sig[ai,1]),7]) > as.numeric(bdata[as.numeric(sig[(ai+1),1]),7])){#MACD為負數
        print("負數MACD沒有上升")
        return(0)
      }
      else if(sig[ai,2] == 2 & as.numeric(bdata[as.numeric(sig[ai,1]),7]) < as.numeric(bdata[as.numeric(sig[(ai+1),1]),7])){#MACD為正數
        print("正數MACD沒有下降")
        return(0)
      }
      #Sig3 K_line
      if(sig[ai,2]==1){
        slope1 = cbind(bdata[(as.numeric(sig[ai,1]):as.numeric(sig[ai+1,1])),3], (1:length(bdata[(as.numeric(sig[ai,1]):as.numeric(sig[ai+1,1])),3])))
        colnames(slope1) = c("L", "nm")
        xx = lm(L ~ nm, slope1)
        #sp = Slope((1:length(slope1[,1])), as.numeric(slope1))
        if(xx$coefficients[2]> -0.2){
          print("MACD為負數，但價格非下跌趨勢")
          return(0)
        }
      }
      if(sig[ai,2]==2){
        slope1 = cbind((bdata[(sig[ai,1]:sig[ai+1,1]),2]), (1:length(bdata[(sig[ai,1]:sig[ai+1,1]),2])))
        colnames(slope1) = c("H", "nm")
        xx = lm(H ~ nm, slope1)
        #sp = Slope((1:length(slope1[,1])), as.numeric(slope1))
        if(xx$coefficients[2] < 0.2){
          print("MACD為正數，但價格非上升趨勢")
          return(0)
        }
      }
      #買進
      if(ai == 2){
        print("操作")
        return(as.numeric(sig[ai,2]))
        #gdsell2 = as.numeric(bdata[as.numeric(sig[ai,1]),2])
        #gdsell1 = as.numeric(bdata[as.numeric(sig[ai,1]),3])
        #inprice = as.numeric(bdata[as.numeric(sig[ai,1]),4])
        
        #表示現在持有中
      }
    }
  }
  gdsell2 = as.numeric(bdata[as.numeric(sig[ai-1,1]),2])
  gdsell1 = as.numeric(bdata[as.numeric(sig[ai-1,1]),3])
  inprice = as.numeric(bdata[as.numeric(sig[ai-1,1]),4])
  nowprice = as.numeric(bdata[length(bdata[,1]),4])#現價
  #持倉狀態
  if(holdm == 1){#做多
    qqsell = as.numeric(bdata[i,11])#止損
    gdsell1 = max(gdsell1, as.numeric(bdata[i,3]))
    if(nowprice < qqsell){
      print("SELL-QQ")
      price = nowprice - inprice
      return(0)
    }
    else if(nowprice < gdsell1*0.97){
      print("SELL-GD")
      price = nowprice - inprice
      return(0)
    }
    else{
      print("KEEP HOLD")
      return(1)
    }
  }
  else if(holdm == 2){#做空
    qqsell = as.numeric(bdata[i,10])#止損
    gdsell2 = max(gdsell2, as.numeric(bdata[i,2]))
    if(nowprice > qqsell){
      print("SELL-QQ")
      price = nowprice - inprice
      return(0)
    }
    else if(nowprice < gdsell2*0.97){
      print("SELL-GD")
      price = nowprice - inprice
      return(0)
    }
    else{
      print("KEEP HOLD")
      return(2)
    }
  }
  return(holdm)
}

stock = get(getSymbols("AAPL", from="2015-01-01", to=Sys.Date()))
main(stock, 0)
#回測(length(stock[,1])-100)
for(ii in 50:720){#length(stock[,1])
  if(ii == 50){
    hold = 0
  }
  datas = stock[1:ii,]
  print(ii)
  hold = main(datas, hold)
}
stocknum = stock[1:ii,]
holdm = hold
chartSeries(stocknum)
addMACD(stocknum)
addLines(211)
