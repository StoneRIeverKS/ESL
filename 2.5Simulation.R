#次元の呪い

##########################関数定義#######################
euclidDistance = function(vecA, vecB){
  return(sqrt(sum((vecA-vecB)^2)))
}
#########################################################

#########################パラメータ設定##################
##シミュレーションの回数
K = 10^3

##最大次元数
P = 15

##データ数
datasize = 10^3

##結果を格納するベクトル
###最近傍点までの距離の平均
aveDistance = rep(1, P)
###バイアスの平均
aveBias = rep(1, P)
###分散の平均
aveVar = rep(1, P)
######################################################

######################シミュレーション################
for(p in 1:P){
  #最近傍点までの距離を格納するベクトル
  minDistance = rep(1, K)
  #Yの予測値を格納するベクトル
  vecHatY = rep(1, K)
  
  for(k in 1:K){
    #データ作成
    ##入出力データを格納するベクトル
    X = matrix(nrow=datasize, ncol=p)
    Y = c()
    
    ##入出力データの作成
    for(d in 1:datasize){
      X[d,] = runif(n=p, min=-1, max=1)
      Y[d] = exp(-8*sum(X[d,]^2))
    }#d
    
    ##原点データの作成
    X0 = rep(0, p)
    
    #1最近傍法の実施
    ##原点からのユークリッド距離の計算
    distance = c()
    for(d in 1:datasize){
      distance[d] = euclidDistance(X[d,], X0)
    }#d
    ##予測値を求める
    hatY = Y[which.min(distance)]
    
    ##結果を保存
    minDistance[k] = min(distance)
    vecHatY[k] = hatY
  }#k
  
  #最近傍点までの距離の平均
  aveDistance[p] = mean(minDistance)
  #バイアスの計算
  aveBias[p] = (mean(vecHatY)-1)^2
  #分散の計算
  aveVar[p] = var(vecHatY)
}#p
#MSEの計算
aveMSE = aveVar+aveBias

#########################図の作成######################
png("次元数と最近傍点との距離の平均.png", height=300, width=300)
plot(1:P, aveDistance, type="b", col="red", xlab="次元数", ylab="最近傍点との距離")
dev.off()

png("次元数と最近傍点との距離の平均と立方体の対角線の割合.png", height=300, width=300)
plot(1:P, aveDistance/(sqrt(1:P)/2), type="b", col="red", xlab="次元数", ylab="最近傍点との距離と対角線の半分との割合")
dev.off()

png("MSE, bias, 分散.png", height=400, width=400)
plot(1:P, aveMSE, type="b", col="red", ylim=c(0,1), xlab="次元数", ylab="MSE,Bias,分散")
par(new=T)
plot(1:P, aveBias, type="b", col="blue", ylim=c(0,1), xlab="", ylab="")
par(new=T)
plot(1:P, aveVar, type="b", col="green", ylim=c(0,1), xlab="", ylab="")
legend("topleft", legend = c("MSE", "Bias", "Var"), col = c("red", "blue", "green"), lty=rep(1, 3))
dev.off()
       