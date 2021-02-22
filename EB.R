##########################################################
#  ����͗p�X�N���v�g�@�@�@2021/01/09 �R������
#  ����͗p�X�N���v�g�@�@�@2019/04/18 �R���쐬�@
#�@�}�K����͗p�X�N���v�g�@2018/03/15 ���R�쐬����ɍ쐬
#  �ł��邱��:�摜��́C�Â����w�肵�ď����̎�����~�C���ʂ�csv�`���Ŏ����ۑ��C
#�@�@�@�@�@�@ �@ ��͓��̎������́C�摜�E�}�X�N�E���ʂ̕ۑ���t�H���_�̎w��,�O���t�쐬,���̐��̕\���@ 
#  �摜��͎�@:�������}�X�N������2�l�����I�u�W�F�N�g�T�C�Y���ށ����x�����O                                   
#�@ #   �F�I�v�V�����@�\�D�g�p�������Ƃ���#����������D    
# �@##  �F�����ߒ��̐���
# �@### :���͂��K�v�ȕ��� 
#  �g�p���邽�߂ɂ�EBImage�p�b�P�[�W�̃C���X�g�[�����K�v
##########################################################



## �������Ԃ̌v���J�n
ts<-proc.time()


## EBImage�i�摜�����p�b�P�[�W�j�̋N��
library(EBImage)
library(tcltk)

## �B�e���̓���
place <- "izunuma"  ### �B�e�ꏊ�����
time <- " 04:11"  ### �B�e�J�n���������
today <- Sys.Date()  ##����(��͂����{������)�̓��t��\��
todayc <- format(today, "%Y-%m-%d")
#todayc <- "2018-07-27"  ### �B�e���Ɖ�͓����قȂ�ꍇ�͎����
now <- Sys.time()				##��͎������擾
todayc <- format(now, "-%Y%m%d-%H%M%S")		##��͎�����������
todayc

## ��͂Ɏg�p����p�����[�^��ݒ�
th <- 0.1 ### 2�l��臒l�����
ob <- 2000 �@### �I�u�W�F�N�g�T�C�Y�����
stop <- 0.43 �@### ��͂��~����Â�(�P�x�l)�����(izunuma171116��0.19)
erth <- 3
dith <- 81

## �}�X�N�摜��ۑ�������ƃf�B���N�g��(�t�@�C���p�X)�̐ݒ�

## ��͂Ɏg�p����摜��ۑ�������ƃf�B���N�g��(�t�@�C���p�X)�̐ݒ�
file.dir <- "C:/test/1029" 

�@###  " "�Ɏw�肵�����t�H���_�̂P�O�܂ł̃p�X����͂��Ă���.�p�X��/�ŋ�؂邱��.�B�e���Ɖ�͓����قȂ�ꍇ�͉��s�Ŏ���͂���
#file.dir <- "result0.10-1.csv" �@### " "�Ƀt�H���_�̃p�X����͂���.�p�X��/�ŋ�؂邱��

## ��͌��ʂƏ������Ԃ̃t�@�C�����o�͂�������ƃf�B���N�g��(�t�@�C���p�X)�̐ݒ�
result.dir <- "C:/result"  ### " "�Ƀt�H���_�̃p�X����͂���.�p�X��/�ŋ�؂邱��

## ��͌��ʂ̏o�̓t�@�C�������w�肷��
result.name <- paste0("result", todayc,th,ob,stop,erth,dith, ".csv") �@### �B�e���Ɖ�͓����قȂ�ꍇ�͉��s�Ŏ���͂���
#result.name <- "result0.10-1.csv" �@### " "�Ɍ��ʂ̃t�@�C���������.�g���q��csv�ɂ���

## �O���t�̏o�̓t�@�C�������w�肷��
#graph.name <- paste0("graph", todayc, ".pdf") �@### �B�e���Ɖ�͓����قȂ�ꍇ�͉��s�Ŏ���͂���
#graph.name <- "graph.10-1.csv" �@### " "�ɃO���t�̃t�@�C���������.�g���q��jpg�ɂ���

## �������Ԃ̏o�̓t�@�C�������w�肷��
#time.name <- paste0("time", todayc, ".csv") �@### �B�e���Ɖ�͓����قȂ�ꍇ�͉��s�Ŏ���͂���
#time.name <- "time0.10-1.csv" �@### ""�ɏ������Ԃ̃t�@�C���������.�g���q��csv�ɂ���


##�@��͏���

## �}�X�N�摜��ۑ�������ƃf�B���N�g��(�t�@�C���p�X)���w��
#setwd(mask.dir)
## �}�X�N�摜�̓ǂݍ���
#mask <- readImage(mask.file)

## ��������摜��ۑ�������ƃf�B���N�g��(�t�@�C���p�X)���w��
setwd(file.dir)
## �f�B���N�g�����̃t�@�C���ꗗ���쐬
files <- list.files()
files
## �uJPG�v�g���q�����t�@�C�������X�g�A�b�v
JPG.files <- grep("\\.jpg$", files)
JPG.files

## �������ʂ��s��Ƃ��Ē�`
result <- matrix(",", nrow = length(JPG.files), ncol = 8)

## ���̐�n���`
n <- 0

n1 <- (length(JPG.files)/3)
pb <- txtProgressBar(min=1, max=n1, style=3)
## �ȉ�,�摜�����f�B���N�g�����̉摜�̖������̏������J��Ԃ�
for (i in 1:(length(JPG.files)/3)) {


## �摜���
## ��͂Ɏg�p����摜��ǂݍ���.img2����͑Ώۂ̉摜�Ƃ�,�O��̉摜���g�p
img1 <- readImage(files[JPG.files[3*i-2]])
img2 <- readImage(files[JPG.files[3*i-1]])
img3 <- readImage(files[JPG.files[3*i]])
#display(img2)�@�@##�@��͑Ώۂ̉摜��\��
#img1 <- img1*mask ##���ʂɔ��˂��Ă���ꍇ�I����
#img2 <- img2*mask
#img3 <- img3*mask
## RGB�̂���R�݂̂𒊏o
imgb1 <- channel(img1,"red")
imgb2 <- channel(img2,"red")
imgb3 <- channel(img3,"red")
#display(imgb2)�@�@## B�摜�̕\��
#writeImage(imgb2,"imgb2.jpg")�@�@## B�摜�̕ۑ�

## �S�Ă̋P�x�l�̕��ς��Z�o
mean <- mean(imgb2[,]) 
## �摜�����邭�Ȃ肷�������͂��~
if(mean >= stop) {
## �S�Ă̋P�x�l�̒����l���Z�o
#median1 <- median(imgb1[,])
#median2 <- median(imgb2[,])
#median3 <- median(imgb3[,])
#median <- abs(median1-median2)
##�S���œ�����
#if(0.005 <= median) {th <- (median*4)+0.01}
##���邳���ς������~�߂�
#if(median <= 0.001) {
##�@�w�i����
## �O��̎ʐ^�Ƃ̍����ɂ��w�i�y�щ_����������D�_�͐��b�œ�����D
## img2��������̂͂̉摜�𔒂̒l�ɂ��邽�߁D
##�@�O�㕽�ω摜�Ƃ̔w�i����
imgd <- imgb1-imgb2
imgd3 <- imgb3-imgb2
imgd2 <- abs(imgd)
imgd4 <- abs(imgd3)

## �P��2�l��
#hist(imgm) ## �P�x�̃q�X�g�O�����̕\��
### �q�X�g�O�����O�������ɂ��臒l��ݒ�.�������l�قǎc��₷��,�傫���l�قǏ����₷��
imgt <- imgd2 >th
imgt3 <- imgd4 >th
#display(imgt)  ## 2�l���摜�̕\��
#writeImage(imgt,"imgt.jpg")  ## 2�l���摜��	�ۑ�


kern <- makeBrush(erth, shape="disc")## �t�B���^�̍쐬

imge1<-erode(imgt,kern)## ���邢���������炷
imge3<-erode(imgt3,kern)## ���邢���������炷

kern2 <- makeBrush(dith, shape="disc")## �t�B���^�̍쐬
imgm1<-dilate(imge1,kern2)## ���邢�����𑝂₷
imgm3<-dilate(imge3,kern2)## ���邢�����𑝂₷

imgm<-fillHull(imgm1) ##���̊J�������𖳂���
imgm2<-fillHull(imgm3) ##���̊J�������𖳂���

#display(imgd)  ## �����摜�̕\��
#writeImage(imgd,"imgd.jpg")�@�@## �����摜�̕ۑ�

##�@�}�X�N����


## �摜�Ƀ}�X�N��������
#imgm <- imgm*mask �@## �����摜�ƃ}�X�N�͈͂��d�Ȃ镔���i=��(�m�C�Y���܂�)�j�݂̂��c��
#display(imgm) �@## �}�X�N�摜�̕\��
#writeImage(imgm,"imgm.jpg")�@ ## �}�X�N�摜�̕ۑ�


## �m�C�Y����
## �I�u�W�F�N�g�T�C�Y�����ɂ��m�C�Y����
## �I�u�W�F�N�g�̃T�C�Y�����X�g�A�b�v
imgt <- bwlabel(imgm)
imgt3 <- bwlabel(imgm2)

sizelist <- computeFeatures.shape(imgt)[,1]
sizelist3 <- computeFeatures.shape(imgt3)[,1]
## �Ώۂ���������Ǝ��Ԃ�������D臒l�Ƃ̃o�����X
#hist(sizelist) ## �I�u�W�F�N�g�T�C�Y�̃q�X�g�O������\��
## �I�u�W�F�N�g�T�C�Y����菬�����Ώۂ��m�C�Y�Ƃ��ď���
## ��������I�u�W�F�N�g�T�C�Y��ݒ�.�q�X�g�O�����Ǝ��ۂ̋��̃I�u�W�F�N�g�T�C�Y���画�f(600m��2.2px,1km��1.2px)
objn <- which(sizelist >= ob)  ##�@�ݒ肵���T�C�Y�ȏ�̃I�u�W�F�N�g�����Ƃ��Ē��o
objn3 <- which(sizelist3 >= ob)

a <- 1:length(sizelist)
a3 <- 1:length(sizelist3)


if(length(objn) == 0){
 b <- a
}else{
b <- a[-objn]
}

if(length(objn3) == 0){
 b3 <- a3
}else{
b3 <- a3[-objn3]
}

imgn <- rmObjects(imgt, b) �@##�@�������I�u�W�F�N�g�i���m�C�Y�j������
imgn3 <- rmObjects(imgt3, b3) �@##�@�������I�u�W�F�N�g�i���m�C�Y�j������

#display(imgn)  ## �m�C�Y�����摜�̕\��
#s <- as.character(i)
#s1 <- paste("imgn",s,".jpg", sep="")
#writeImage(imgn,s1)

##���x�����O�i�I�u�W�F�N�g�ɒʂ��ԍ�������j
imgl <- bwlabel(imgn)
imgl3 <- bwlabel(imgn3)
#}else{imgl <- 0}
}else{
imgl <- 0
imgl3 <- 0
}

## �J�E���g����
## �������ʂɃt�@�C����,�J�E���g��,�P�x�l�̕��ς�����D
result[3*i-2,1] <- 3*i-2 �@### �O���t�̉����Ɏg�p����.�B�e�Ԋu�ɏ]��������͂���
result[3*i-2,3] <- files[JPG.files[3*i-2]]  ##�ʐ^�̃t�@�C����
result[3*i-2,5] <- max(imgl)  ## ���x�����O�̍ő�l�i���I�u�W�F�N�g�̌��j
result[3*i-2,7] <- mean ## �P�x�l�̕���
result[3*i-1,1] <- 3*i-1 �@### �O���t�̉����Ɏg�p����.�B�e�Ԋu�ɏ]��������͂���
result[3*i-1,3] <- files[JPG.files[3*i-1]]  ##�ʐ^�̃t�@�C����
result[3*i-1,5] <- max(imgl3)  ## ���x�����O�̍ő�l�i���I�u�W�F�N�g�̌��j
result[3*i-1,7] <- mean ## �P�x�l�̕���
result[3*i,1] <- 3*i �@### �O���t�̉����Ɏg�p����.�B�e�Ԋu�ɏ]��������͂���
result[3*i,3] <- files[JPG.files[3*i]]  ##�ʐ^�̃t�@�C����
result[3*i,5] <- 0  ## ���x�����O�̍ő�l�i���I�u�W�F�N�g�̌��j
result[3*i,7] <- 0 ## �P�x�l�̕���
setTxtProgressBar(pb, i)

## �������ʂ��o�͕ۑ�
setwd(result.dir)
write(t(result), file=result.name, ncolumns=8)


## �������ʂ̃O���t���쐬�E�o�͕ۑ�
#x <- result[,1]
#y <- result[,5]
#n <- n+max(imgl)
### ���̍ő�E�ŏ��A���x�����A�v���b�g�̐F��`����C�ӂŎw�肷��
#plot(x, y, type = "n", ylim=c(0,80), xlab = "photo [��]", ylab = "fish count [�C]")
#points(x, y , col = "red", pch = 16)
#lines(x, y, col = "red")

## �O���t�ɎB�e����\��
#date <- paste0(todayc, time) 
#n.picture <- paste0("Number of pictures: ", length(JPG.files))
#n.fish <- paste0("Total count of fishes: ", n)
#mtext(place,side=3,line=3,adj=0,cex=1)
#mtext(date,side=3,line=2,adj=0,cex=1)
#mtext(n.picture,side=3,line=1,adj=0,cex=1)
#mtext(n.fish,side=3,line=0,adj=0,cex=1)
#dev.copy(device=pdf, file=graph.name, family="Japan1GothicBBB")
#dev.off()


## �������Ԃ̌v���I��
te <- proc.time()-ts  ## �������ԁ��I������-�J�n����
## �������Ԃ��o�͕ۑ�
#write(te, file=time.name)


## ���̉�͂̂���,��ƃf�B���N�g�����摜���ۑ����ꂽ�t�H���_�ɖ߂�
setwd(file.dir)

}


result
te
