#p16,18,19�̃V�~�����[�V����

#�f�[�^�쐬
##1. al��10��������

###���K���z�̕��ςƋ����U
mu1 = c(1,0)
S1 = matrix(c(1,0,0,1), nrow=2)

###�����̔���
library(MASS)
dat1 = mvrnorm(10, mu1, S1)

##2. bl��10��������

###���K���z�̕��ςƋ����U
mu2 = c(0,1)
S2 = matrix(c(1,0,0,1), nrow=2)

###�����̔���
dat2 = mvrnorm(10, mu1, S1)

##3., 5. �N���X�̏o�̓f�[�^�𐶐�����
select1 = ceiling(runif(n=100, min=0, max=10))
ave1 = dat1[select1,]
X_blue = matrix(nrow=100, ncol=2)
for(i in 1:100){
 X_blue[i,] = mvrnorm(n=1, ave1[i,], S1/5)  
}

##4., 5. �ԃN���X�̏o�̓f�[�^�𐶐�����
select2 = ceiling(runif(n=100, min=0, max=10))
ave2 = dat2[select2,]
X_red = matrix(nrow=100, ncol=2)
for(i in 1:100){
  X_red[i,] = mvrnorm(n=1, ave2[i,], S2/5)  
}
###���̓f�[�^����������
X = rbind(X_blue,X_red)

##6. �o�̓f�[�^���쐬����
Y_blue = rep(0, 100)
Y_red = rep(1, 100)
y = c(Y_blue, Y_red)



#���`��A���f��
##1. �W�������߂�
beta = solve(t(X)%*%X)%*%t(X)%*%y

##2. Y�̗\���l�����߂�
hatY = X%*%beta

##3. G�̗\���l�����߂�
hatG = (hatY>0.5)*1

#�ŋߖT�@
##1. k�̒l��ݒ肷��
k=1

##2. �e�f�[�^�ɑ΂��ċ������v�Z����
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

##3. �������Z��k�̃f�[�^�����o���A�o�̓f�[�^�̕��ς��v�Z����
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

#���`��A���f���ƍŋߖT�@�̔�r
##1000��V�~�����[�V�������s��
##�ŋߖT�@�ɂ��ẮAk=1����10�܂ł̌��ʂ�p����

###���𗦂�ۑ�����f�[�^
result = matrix(nrow=11, ncol=1000)

###library�̓ǂݍ���
library(MASS)

###���[�N���b�h�������v�Z����֐�
##2. �e�f�[�^�ɑ΂��ċ������v�Z����
euclid = function(a, b){
  ia = length(a)
  wa = 0
  for(i in 1:ia){
    wa = wa+(a[i]-b[i])^2
  }
  return(sqrt(wa))
}

###1000�Z�b�g�̃V�~�����[�V���������s
for(l in 1:1000){
  #�f�[�^�쐬
  ##1. al��10��������

  ###���K���z�̕��ςƋ����U
  mu1 = c(1,0)
  S1 = matrix(c(1,0,0,1), nrow=2)

  ###�����̔���
  dat1 = mvrnorm(10, mu1, S1)

  ##2. bl��10��������

  ###���K���z�̕��ςƋ����U
  mu2 = c(0,1)
  S2 = matrix(c(1,0,0,1), nrow=2)

  ###�����̔���
  dat2 = mvrnorm(10, mu1, S1)

  ##3., 5. �N���X�̏o�̓f�[�^�𐶐�����
  select1 = ceiling(runif(n=100, min=0, max=10))
  ave1 = dat1[select1,]
  X_blue = matrix(nrow=100, ncol=2)
  for(i in 1:100){
    X_blue[i,] = mvrnorm(n=1, ave1[i,], S1/5)  
  }#i�ɂ��Ă�for��

  ##4., 5. �ԃN���X�̏o�̓f�[�^�𐶐�����
  select2 = ceiling(runif(n=100, min=0, max=10))
  ave2 = dat2[select2,]
  X_red = matrix(nrow=100, ncol=2)
  for(i in 1:100){
    X_red[i,] = mvrnorm(n=1, ave2[i,], S2/5)  
  }#i�ɂ��Ă�for��
  ###���̓f�[�^����������
  X = rbind(X_blue,X_red)

  ##6. �o�̓f�[�^���쐬����
  Y_blue = rep(0, 100)
  Y_red = rep(1, 100)
  y = c(Y_blue, Y_red)

  #���`��A���f��
  ##1. �W�������߂�
  beta = solve(t(X)%*%X)%*%t(X)%*%y

  ##2. Y�̗\���l�����߂�
  hatY = X%*%beta

  ##3. G�̗\���l�����߂�
  hatG = (hatY>0.5)*1

  ##���ʂ��i�[����
  ###���`���f�����Ƃ��܂�����ł����Atable��1�~2�s���
  ###�Ȃ邱�Ƃ�����B���̏ꍇ�ł����������v�Z�ł���悤��
  ###�������s��
  if(dim(table(hatG,y))[1]==1){
    result[1,l] = table(hatG,y)[1,1]/200
  }else{
    result[1,l] = (table(hatG, y)[1,1]+table(hatG, y)[2,2])/200
  }

  #�ŋߖT�@
  ##1. k�̒l��ݒ肷��
  K = 10
  for(k in 1:K){
  ##�����s��̍쐬
    NN = matrix(nrow=200, ncol=200)
    for(i in 1:200){
      for(j in 1:200){
        NN[i,j] = euclid(X[i,], X[j,])
      }#j�ɂ��Ă�for��  
    }#i�ɂ��Ă�for��

    ##3. �������Z��k�̃f�[�^�����o���A�o�̓f�[�^�̕��ς��v�Z����
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
    }#i�ɂ��Ă�for��
    hatg = (haty>0.5)*1
    result[k+1,l] = (table(hatg, y)[1,1]+table(hatg, y)[2,2])/200
  }#k�ɂ��Ă�for��
}#l�ɂ��Ă�for��

#���ʂ��܂Ƃ߂��}�쐬
setwd("C:\\Users\\stone\\Documents\\���v\\���v�I�@�B�w�K�̊�b")
png("233_�V�~�����[�V��������.png", height=300, width=300)
plot(seq(1,10,by=1), apply(result, MARGIN=1, mean)[2:11], ,ylim=c(0.5, 1.1), col='red', lty=2, type="b", xaxt='n', yaxt='n', xlab="k�̒l", ylab="�������̕���")
axis(side=1, at=seq(1,10, by=1))
axis(side=2, at=seq(0.5, 1.10, by=0.1))
abline(h=apply(result, MARGIN=1, mean)[1])
dev.off()