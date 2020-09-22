#次元の呪い

##########################関数定義#######################
euclidDistance = function(vecA, vecB){
  return(sqrt(sum((vecA-vecB)^2)))
}
#########################################################

#########################パラメータ設定##################
##シミュレーションの回数
K = 1000

##最大次元数
P = 15

##データ数
datasize = 500

##結果を格納するベクトル
###EPE
####1最近傍法
EPE_1 = rep(1, P)
####線形回帰モデル
EPE_Linear = rep(1, P)

######################################################

######################シミュレーション################
for(p in 1:P){
  #最近傍点までの距離を格納するベクトル
  #Yの予測値を格納するベクトル
  ##1最近傍法
  vecHatY_1 = rep(1, K)
  ##線形回帰モデル
  vecHatY_Linear = rep(1, K)
  
  for(k in 1:K){
    #データ作成
    ##入出力データを格納するベクトル
    X = matrix(nrow=datasize, ncol=p)
    Y = c()
    
    ##入出力データの作成
    for(d in 1:datasize){
      X[d,] = runif(n=p, min=-1, max=1)
      Y[d] = 1/2*(X[d,1]+1)^3+rnorm(n=1, mean=0, sd=1)
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
    hatY_1 = Y[which.min(distance)]
    
    #線形回帰モデル
    hatBeta = solve(t(X)%*%X)%*%t(X)%*%Y
    hatY_Linear = t(X0)%*%hatBeta
    
    ##結果を保存
    ###1最近傍法
    vecHatY_1[k] = hatY_1
    ###線形回帰モデル
    vecHatY_Linear[k] = hatY_Linear
  }#k
  
  #EPEの計算
  ##1最近傍法
  Y = 1/2+rnorm(n=K, mean=0, sd=1)
  EPE_1[p] = mean((Y-vecHatY_1)^2)
  ##線形回帰モデル
  EPE_Linear[p] = mean((Y-vecHatY_Linear)^2)
  
}#p


#########################図の作成######################
png("次元数EPE_2.png", height=500, width=500)
plot(1:P, EPE_1, type="b", col="red", ylim=c(1.2,2.5), xlab="次元数", ylab="EPE")
par(new=T)
plot(1:P, EPE_Linear, type="b", col="blue", ylim=c(1.2,2.5), xlab="", ylab="")
dev.off()
