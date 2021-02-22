#py
#pythonの起動
#以下Python内

#画像処理パッケージ
import cv2
#CSVパッケージ
import csv
#ディレクトリ移動パッケージ
import os
#差分パッケージ
import numpy as np
#オブジェクトサイズ分類用関数の定義
def remove_objects(img, lower_size=None, upper_size=None):
 nlabels, labels, stats, centroids = cv2.connectedComponentsWithStats(img)
 sizes = stats[1:, -1]
 _img = np.zeros((labels.shape))
 for i in range(1, nlabels):
  if (lower_size is not None) and (upper_size is not None):
   if lower_size < sizes[i - 1] and sizes[i - 1] < upper_size:
    _img[labels == i] = 255
  elif (lower_size is not None) and (upper_size is None):
   if lower_size < sizes[i - 1]:
    _img[labels == i] = 255
  elif (lower_size is None) and (upper_size is not None):
   if sizes[i - 1] < upper_size:
    _img[labels == i] = 255
 return _img

#対象フォルダ数
for j in range(3):
 #スタート対象のフォルダ名
 num = 1029+j
 #対象フォルダに移動
 os.chdir('C:/test/' + str(num))
 print(os.getcwd())
 #pathを取得
 path = os.getcwd()
 #処理済画像の保存場所を指定
 dirname = r'C:/buffer'
 #対象フォルダのファイルリストの取得
 files = os.listdir(path)
 #対象フォルダのファイル数の取得
 count = len(files)
 #処理結果用行列の作成
 a = np.array([["name", "number", "pop", "bright"]])
 #以下画像毎の処理
 for i in range(count):
  if i%3 <= 1:
   #処理対象の画像を取得
   img_src1 = cv2.imread(path + "/" + files[i])
   #処理対象の画像の一枚後の画像を取得
   img_src2 = cv2.imread(path + "/" + files[i+1])
   #処理対象の画像の輝度値を色ごとに分割
   b1, g1, r1 = cv2.split(img_src1)
   #処理対象の画像の一枚後の画像の輝度値を色ごとに分割
   b2, g2, r2 = cv2.split(img_src2)
   #処理対象の画像の緑輝度値の平均を計算
   br = g1.mean()
   #処理対象の画像の一枚後の画像の緑輝度値の平均を取得
   br2 = g2.mean()
   #緑輝度値の平均の差を取得
   brd = br - br2
   #緑輝度値の平均の差の絶対値を取得
   brdabs = abs(brd)
   #以下輝度値のエラーの無い画像に関する処理
   if br >= 50 and brdabs <=1:
    #フォルダ内の残りの画像数をカウント
    count-i
    #MOGアルゴリズムを定義
    #閾値あり
    fgbg = cv2.createBackgroundSubtractorMOG2(500,13,0)
    #対象画像にMOGアルゴリズムを適用
    fgmask = fgbg.apply(img_src1)
    fgmask = fgbg.apply(img_src2)
    #MOGアルゴリズムによる処理済み画像を保存する場合
    #cv2.imwrite(os.path.join(dirname, 'fg.jpg'),fgmask)
    #二値化処理
    #MOGアルゴリズムにおいて二値化されているため恐らく不要
    ret,thresh1 = cv2.threshold(fgmask,25,255,cv2.THRESH_BINARY)
    #二値化処理済み画像を保存する場合
    #cv2.imwrite(os.path.join(dirname, 'th.jpg'),thresh1)
    #エロード用カーネルの作成
    #サイズ値あり
    kernel = np.ones((12,12),np.uint8)
    #エロード処理
    erosion = cv2.erode(thresh1,kernel,iterations = 1)
    #エロード処理済み画像を保存する場合
    #cv2.imwrite(os.path.join(dirname, 'er.jpg'),erosion)
    #ディレイト処理用カーネルの作成
    #サイズ値あり
    kernel = np.ones((80,80),np.uint8)
    #ディレイト処理
    dilation = cv2.dilate(erosion,kernel,iterations = 1)
    #後の処理を可能にするため形式を変更
    arr2 = np.uint8(dilation)
    #後の処理を可能にするため形式を変更
    arr3 = cv2.cvtColor(arr2, cv2.COLOR_GRAY2BGR)
    #ディレイト処理済み画像を保存する場合
    #cv2.imwrite(os.path.join(dirname, 'di.jpg'),dilation)
    # Lab色空間に変換
    img_Lab = cv2.cvtColor(arr3, cv2.COLOR_BGR2Lab)
    # Lab色空間を分割
    img_L, img_a, img_b = cv2.split(img_Lab)
    #明度の次元を二値化
    _thre, img_greenarea = cv2.threshold(img_L, 0, 255, cv2.THRESH_BINARY)
    #オブジェクトサイズ分類を実行
    img_greenarea_clean = remove_objects(img_greenarea, lower_size=2000, upper_size=None)
    #オブジェクトサイズ分類処理済み画像を保存する場合
    #cv2.imwrite(os.path.join(dirname, 'output.beans_field.cleanobj.after.jpg'), img_greenarea_clean)
    #再ディレイト用カーネル
    kernel = np.ones((100,100),np.uint8)
    #分離したオブジェクトがあった場合合体させるための再ディレイト処理
    dilation = cv2.dilate(img_greenarea_clean,kernel,iterations = 1)
    #再ディレイト処理済み画像を保存する場合
    #cv2.imwrite(os.path.join(dirname, 'di2.jpg'),dilation)
    #後の処理を可能にするため形式を変更
    arr = np.uint8(dilation)
    #オブジェクトのカウント
    contours, hierarchy = cv2.findContours(arr,cv2.RETR_TREE,cv2.CHAIN_APPROX_SIMPLE)
    #カウント数を取得
    d = len(contours)
    #カウント数を表示
    d
   else:
    #エラー画像だった場合のカウント数を0として代入
    d = 0 
    #エラー画像だった場合を識別できるよう輝度値も0として代入
    br = 0
    #カウント数を表示
    d  
  else:
   #3連の画像の三つ目の画像は処理の必要がないため
   d  = 0
   #3連の画像の三つ目の画像は処理の必要がないため
   br = 0
   #3連の画像の三つ目の画像は処理の必要がないため
   d
  #行列用にファイル名を取得
  b = files[i]
  #行列用にファイル番号を取得
  c = i
  #行列用に輝度値
  e = br
  #ファイルごとの行を作成
  f = np.array([[b,c,d,e]])
  #ファイルごとの列を行列に追加
  a = np.vstack((a, f))
 #ディレクトリの変更
 os.chdir('C:/result/')
 #行列をCSVとして出力
 with open('resultbr50MOGth25'+str(num)+'.csv', 'w') as f:
     writer = csv.writer(f)
     writer.writerows(a)




