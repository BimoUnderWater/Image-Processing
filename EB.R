##########################################################
#  魚解析用スクリプト　　　2021/01/09 山中改良
#  魚解析用スクリプト　　　2019/04/18 山中作成　
#　マガン解析用スクリプト　2018/03/15 横山作成を基に作成
#  できること:画像解析，暗さを指定して処理の自動停止，結果をcsv形式で自動保存，
#　　　　　　 　 解析日の自動入力，画像・マスク・結果の保存先フォルダの指定,グラフ作成,総個体数の表示　 
#  画像解析手法:差分→マスク処理→2値化→オブジェクトサイズ分類→ラベリング                                   
#　 #   ：オプション機能．使用したいときに#を消去する．    
# 　##  ：処理過程の説明
# 　### :入力が必要な部分 
#  使用するためにはEBImageパッケージのインストールが必要
##########################################################



## 処理時間の計測開始
ts<-proc.time()


## EBImage（画像処理パッケージ）の起動
library(EBImage)
library(tcltk)

## 撮影情報の入力
place <- "izunuma"  ### 撮影場所を入力
time <- " 04:11"  ### 撮影開始時刻を入力
today <- Sys.Date()  ##今日(解析を実施した日)の日付を表示
todayc <- format(today, "%Y-%m-%d")
#todayc <- "2018-07-27"  ### 撮影日と解析日が異なる場合は手入力
now <- Sys.time()				##解析時時刻取得
todayc <- format(now, "-%Y%m%d-%H%M%S")		##解析時時刻文字列
todayc

## 解析に使用するパラメータを設定
th <- 0.1 ### 2値化閾値を入力
ob <- 2000 　### オブジェクトサイズを入力
stop <- 0.43 　### 解析を停止する暗さ(輝度値)を入力(izunuma171116は0.19)
erth <- 3
dith <- 81

## マスク画像を保存した作業ディレクトリ(ファイルパス)の設定

## 解析に使用する画像を保存した作業ディレクトリ(ファイルパス)の設定
file.dir <- "C:/test/1029" 

　###  " "に指定したいフォルダの１つ前までのパスを入力しておく.パスは/で区切ること.撮影日と解析日が異なる場合は下行で手入力する
#file.dir <- "result0.10-1.csv" 　### " "にフォルダのパスを入力する.パスは/で区切ること

## 解析結果と処理時間のファイルを出力したい作業ディレクトリ(ファイルパス)の設定
result.dir <- "C:/result"  ### " "にフォルダのパスを入力する.パスは/で区切ること

## 解析結果の出力ファイル名を指定する
result.name <- paste0("result", todayc,th,ob,stop,erth,dith, ".csv") 　### 撮影日と解析日が異なる場合は下行で手入力する
#result.name <- "result0.10-1.csv" 　### " "に結果のファイル名を入力.拡張子はcsvにする

## グラフの出力ファイル名を指定する
#graph.name <- paste0("graph", todayc, ".pdf") 　### 撮影日と解析日が異なる場合は下行で手入力する
#graph.name <- "graph.10-1.csv" 　### " "にグラフのファイル名を入力.拡張子はjpgにする

## 処理時間の出力ファイル名を指定する
#time.name <- paste0("time", todayc, ".csv") 　### 撮影日と解析日が異なる場合は下行で手入力する
#time.name <- "time0.10-1.csv" 　### ""に処理時間のファイル名を入力.拡張子はcsvにする


##　解析準備

## マスク画像を保存した作業ディレクトリ(ファイルパス)を指定
#setwd(mask.dir)
## マスク画像の読み込み
#mask <- readImage(mask.file)

## 処理する画像を保存した作業ディレクトリ(ファイルパス)を指定
setwd(file.dir)
## ディレクトリ内のファイル一覧を作成
files <- list.files()
files
## 「JPG」拡張子を持つファイルをリストアップ
JPG.files <- grep("\\.jpg$", files)
JPG.files

## 処理結果を行列として定義
result <- matrix(",", nrow = length(JPG.files), ncol = 8)

## 総個体数nを定義
n <- 0

n1 <- (length(JPG.files)/3)
pb <- txtProgressBar(min=1, max=n1, style=3)
## 以下,画像処理ディレクトリ内の画像の枚数分の処理を繰り返す
for (i in 1:(length(JPG.files)/3)) {


## 画像解析
## 解析に使用する画像を読み込む.img2を解析対象の画像とし,前後の画像を使用
img1 <- readImage(files[JPG.files[3*i-2]])
img2 <- readImage(files[JPG.files[3*i-1]])
img3 <- readImage(files[JPG.files[3*i]])
#display(img2)　　##　解析対象の画像を表示
#img1 <- img1*mask ##水面に反射している場合オンに
#img2 <- img2*mask
#img3 <- img3*mask
## RGBのうちRのみを抽出
imgb1 <- channel(img1,"red")
imgb2 <- channel(img2,"red")
imgb3 <- channel(img3,"red")
#display(imgb2)　　## B画像の表示
#writeImage(imgb2,"imgb2.jpg")　　## B画像の保存

## 全ての輝度値の平均を算出
mean <- mean(imgb2[,]) 
## 画像が明るくなりすぎたら解析を停止
if(mean >= stop) {
## 全ての輝度値の中央値を算出
#median1 <- median(imgb1[,])
#median2 <- median(imgb2[,])
#median3 <- median(imgb3[,])
#median <- abs(median1-median2)
##全部で動かす
#if(0.005 <= median) {th <- (median*4)+0.01}
##明るさが変わったら止める
#if(median <= 0.001) {
##　背景差分
## 前後の写真との差分により背景及び雲を除去する．雲は数秒で動き難い．
## img2を減ずるのはの画像を白の値にするため．
##　前後平均画像との背景差分
imgd <- imgb1-imgb2
imgd3 <- imgb3-imgb2
imgd2 <- abs(imgd)
imgd4 <- abs(imgd3)

## 単純2値化
#hist(imgm) ## 輝度のヒストグラムの表示
### ヒストグラムグラム等により閾値を設定.小さい値ほど残りやすく,大きい値ほど消えやすい
imgt <- imgd2 >th
imgt3 <- imgd4 >th
#display(imgt)  ## 2値化画像の表示
#writeImage(imgt,"imgt.jpg")  ## 2値化画像の	保存


kern <- makeBrush(erth, shape="disc")## フィルタの作成

imge1<-erode(imgt,kern)## 明るい部分を減らす
imge3<-erode(imgt3,kern)## 明るい部分を減らす

kern2 <- makeBrush(dith, shape="disc")## フィルタの作成
imgm1<-dilate(imge1,kern2)## 明るい部分を増やす
imgm3<-dilate(imge3,kern2)## 明るい部分を増やす

imgm<-fillHull(imgm1) ##穴の開いた魚を無くす
imgm2<-fillHull(imgm3) ##穴の開いた魚を無くす

#display(imgd)  ## 差分画像の表示
#writeImage(imgd,"imgd.jpg")　　## 差分画像の保存

##　マスク処理


## 画像にマスクをかける
#imgm <- imgm*mask 　## 差分画像とマスク範囲が重なる部分（=魚(ノイズを含む)）のみを残す
#display(imgm) 　## マスク画像の表示
#writeImage(imgm,"imgm.jpg")　 ## マスク画像の保存


## ノイズ除去
## オブジェクトサイズ処理によるノイズ除去
## オブジェクトのサイズをリストアップ
imgt <- bwlabel(imgm)
imgt3 <- bwlabel(imgm2)

sizelist <- computeFeatures.shape(imgt)[,1]
sizelist3 <- computeFeatures.shape(imgt3)[,1]
## 対象が多すぎると時間がかかる．閾値とのバランス
#hist(sizelist) ## オブジェクトサイズのヒストグラムを表示
## オブジェクトサイズがより小さい対象をノイズとして除去
## 除去するオブジェクトサイズを設定.ヒストグラムと実際の魚のオブジェクトサイズから判断(600m→2.2px,1km→1.2px)
objn <- which(sizelist >= ob)  ##　設定したサイズ以上のオブジェクトを魚として抽出
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

imgn <- rmObjects(imgt, b) 　##　小さいオブジェクト（＝ノイズ）を消去
imgn3 <- rmObjects(imgt3, b3) 　##　小さいオブジェクト（＝ノイズ）を消去

#display(imgn)  ## ノイズ除去画像の表示
#s <- as.character(i)
#s1 <- paste("imgn",s,".jpg", sep="")
#writeImage(imgn,s1)

##ラベリング（オブジェクトに通し番号をつける）
imgl <- bwlabel(imgn)
imgl3 <- bwlabel(imgn3)
#}else{imgl <- 0}
}else{
imgl <- 0
imgl3 <- 0
}

## カウント結果
## 処理結果にファイル名,カウント数,輝度値の平均を入れる．
result[3*i-2,1] <- 3*i-2 　### グラフの横軸に使用する.撮影間隔に従い式を入力する
result[3*i-2,3] <- files[JPG.files[3*i-2]]  ##写真のファイル名
result[3*i-2,5] <- max(imgl)  ## ラベリングの最大値（＝オブジェクトの個数）
result[3*i-2,7] <- mean ## 輝度値の平均
result[3*i-1,1] <- 3*i-1 　### グラフの横軸に使用する.撮影間隔に従い式を入力する
result[3*i-1,3] <- files[JPG.files[3*i-1]]  ##写真のファイル名
result[3*i-1,5] <- max(imgl3)  ## ラベリングの最大値（＝オブジェクトの個数）
result[3*i-1,7] <- mean ## 輝度値の平均
result[3*i,1] <- 3*i 　### グラフの横軸に使用する.撮影間隔に従い式を入力する
result[3*i,3] <- files[JPG.files[3*i]]  ##写真のファイル名
result[3*i,5] <- 0  ## ラベリングの最大値（＝オブジェクトの個数）
result[3*i,7] <- 0 ## 輝度値の平均
setTxtProgressBar(pb, i)

## 処理結果を出力保存
setwd(result.dir)
write(t(result), file=result.name, ncolumns=8)


## 処理結果のグラフを作成・出力保存
#x <- result[,1]
#y <- result[,5]
#n <- n+max(imgl)
### 軸の最大・最小、ラベル名、プロットの色や形等を任意で指定する
#plot(x, y, type = "n", ylim=c(0,80), xlab = "photo [枚]", ylab = "fish count [匹]")
#points(x, y , col = "red", pch = 16)
#lines(x, y, col = "red")

## グラフに撮影情報を表示
#date <- paste0(todayc, time) 
#n.picture <- paste0("Number of pictures: ", length(JPG.files))
#n.fish <- paste0("Total count of fishes: ", n)
#mtext(place,side=3,line=3,adj=0,cex=1)
#mtext(date,side=3,line=2,adj=0,cex=1)
#mtext(n.picture,side=3,line=1,adj=0,cex=1)
#mtext(n.fish,side=3,line=0,adj=0,cex=1)
#dev.copy(device=pdf, file=graph.name, family="Japan1GothicBBB")
#dev.off()


## 処理時間の計測終了
te <- proc.time()-ts  ## 処理時間＝終了時間-開始時間
## 処理時間を出力保存
#write(te, file=time.name)


## 次の解析のため,作業ディレクトリを画像が保存されたフォルダに戻す
setwd(file.dir)

}


result
te

