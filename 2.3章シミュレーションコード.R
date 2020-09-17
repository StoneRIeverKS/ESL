#p16,18,19のシミュレーション

#データ作成
##1. alを10個生成する

###正規分布の平均と共分散
mu1 = c(1,0)
S1 = matrix(c(1,0,0,1), nrow=2)

###乱数の発生
library(MASS)
dat1 = mvrnorm(10, mu1, S1)

##2. blを10個生成する

###正規分布の平均と共分散
mu2 = c(0,1)
S2 = matrix(c(1,0,0,1), nrow=2)

###乱数の発生
dat2 = mvrnorm(10, mu1, S1)

##3., 5. 青クラスの出力データを生成する
select1 = ceiling(runif(n=100, min=0, max=10))
ave1 = dat1[select1,]
X_blue = matrix(nrow=100, ncol=2)
for(i in 1:100){
 X_blue[i,] = mvrnorm(n=1, ave1[i,], S1/5)  
}

##4., 5. 赤クラスの出力データを生成する
select2 = ceiling(runif(n=100, min=0, max=10))
ave2 = dat2[select2,]
X_red = matrix(nrow=100, ncol=2)
for(i in 1:100){
  X_red[i,] = mvrnorm(n=1, ave2[i,], S2/5)  
}
###入力データを結合する
X = rbind(X_blue,X_red)

##6. 出力データを作成する
Y_blue = rep(0, 100)
Y_red = rep(1, 100)
y = c(Y_blue, Y_red)



#線形回帰モデル
##1. 係数を求める
beta = solve(t(X)%*%X)%*%t(X)%*%y

##2. Yの予測値を求める
hatY = X%*%beta

##3. Gの予測値を求める
hatG = (hatY>0.5)*1

#最近傍法
##1. kの値を設定する
k=1

##2. 各データに対して距離を計算する
euclid = function(a, b){
  ia = length(a)
  wa = 0
  for(i in 1:ia){
    wa = wa+(a[i]-b[i])^2
  }
  return(sqrt(wa))
}

NN = matrix(nrow=200, ncol=200)
for(i in 1:200){
  for(j in 1:200){
    NN[i,j] = euclid(X[i,], X[j,])
  }  
}

##3. 距離が短いk個のデータを取り出し、出力データの平均を計算する
haty = c()
for(i in 1:200){
  NNtmp = NN[i,]
  ytmp = y
  ywa = 0
  for(kk in 1:k){
    del = which.min(NNtmp)
    ywa = ywa+ytmp[del]
    NNtmp = NNtmp[-del]
    ytmp = ytmp[-del]
  }
  haty[i] = ywa/k
}

#線形回帰モデルと最近傍法の比較
##1000回シミュレーションを行う
##最近傍法については、k=1から10までの結果を用いる

###正解率を保存するデータ
result = matrix(nrow=11, ncol=1000)

###libraryの読み込み
library(MASS)

###ユークリッド距離を計算する関数
##2. 各データに対して距離を計算する
euclid = function(a, b){
  ia = length(a)
  wa = 0
  for(i in 1:ia){
    wa = wa+(a[i]-b[i])^2
  }
  return(sqrt(wa))
}

###1000セットのシミュレーションを実行
for(l in 1:1000){
  #データ作成
  ##1. alを10個生成する

  ###正規分布の平均と共分散
  mu1 = c(1,0)
  S1 = matrix(c(1,0,0,1), nrow=2)

  ###乱数の発生
  dat1 = mvrnorm(10, mu1, S1)

  ##2. blを10個生成する

  ###正規分布の平均と共分散
  mu2 = c(0,1)
  S2 = matrix(c(1,0,0,1), nrow=2)

  ###乱数の発生
  dat2 = mvrnorm(10, mu1, S1)

  ##3., 5. 青クラスの出力データを生成する
  select1 = ceiling(runif(n=100, min=0, max=10))
  ave1 = dat1[select1,]
  X_blue = matrix(nrow=100, ncol=2)
  for(i in 1:100){
    X_blue[i,] = mvrnorm(n=1, ave1[i,], S1/5)  
  }#iについてのfor文

  ##4., 5. 赤クラスの出力データを生成する
  select2 = ceiling(runif(n=100, min=0, max=10))
  ave2 = dat2[select2,]
  X_red = matrix(nrow=100, ncol=2)
  for(i in 1:100){
    X_red[i,] = mvrnorm(n=1, ave2[i,], S2/5)  
  }#iについてのfor文
  ###入力データを結合する
  X = rbind(X_blue,X_red)

  ##6. 出力データを作成する
  Y_blue = rep(0, 100)
  Y_red = rep(1, 100)
  y = c(Y_blue, Y_red)

  #線形回帰モデル
  ##1. 係数を求める
  beta = solve(t(X)%*%X)%*%t(X)%*%y

  ##2. Yの予測値を求める
  hatY = X%*%beta

  ##3. Gの予測値を求める
  hatG = (hatY>0.5)*1

  ##結果を格納する
  ###線形モデルだとうまく推定できず、tableが1×2行列に
  ###なることがある。その場合でも正答率を計算できるような
  ###処理を行う
  if(dim(table(hatG,y))[1]==1){
    result[1,l] = table(hatG,y)[1,1]/200
  }else{
    result[1,l] = (table(hatG, y)[1,1]+table(hatG, y)[2,2])/200
  }

  #最近傍法
  ##1. kの値を設定する
  K = 10
  for(k in 1:K){
  ##距離行列の作成
    NN = matrix(nrow=200, ncol=200)
    for(i in 1:200){
      for(j in 1:200){
        NN[i,j] = euclid(X[i,], X[j,])
      }#jについてのfor文  
    }#iについてのfor文

    ##3. 距離が短いk個のデータを取り出し、出力データの平均を計算する
    haty = c()
    for(i in 1:200){
      NNtmp = NN[i,]
      ytmp = y
      ywa = 0
      for(kk in 1:k){
        del = which.min(NNtmp)
        ywa = ywa+ytmp[del]
        NNtmp = NNtmp[-del]
        ytmp = ytmp[-del]
      }
      haty[i] = ywa/k
    }#iについてのfor文
    hatg = (haty>0.5)*1
    result[k+1,l] = (table(hatg, y)[1,1]+table(hatg, y)[2,2])/200
  }#kについてのfor文
}#lについてのfor文

#結果をまとめた図作成
setwd("C:\\Users\\stone\\Documents\\統計\\統計的機械学習の基礎")
png("233_シミュレーション結果.png", height=300, width=300)
plot(seq(1,10,by=1), apply(result, MARGIN=1, mean)[2:11], ,ylim=c(0.5, 1.1), col='red', lty=2, type="b", xaxt='n', yaxt='n', xlab="kの値", ylab="正答率の平均")
axis(side=1, at=seq(1,10, by=1))
axis(side=2, at=seq(0.5, 1.10, by=0.1))
abline(h=apply(result, MARGIN=1, mean)[1])
dev.off()