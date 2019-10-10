---
title: <br><center>資料型態與資料儲存方法</center><br>
author: <center>Ti-Ti Tung</center>
date: <center>July, 2018</center>
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

<br>


<P Align=center><img src="RStudio-Ball.png"  width="100" height="100"  /></p>

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

 Install R
====
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

安裝 R 必須依使用者的作業系統來決定安裝的版本為何。
通常會安裝兩個部份：

-  R 的主程式 (http://www.r-project.org/)
-  R 的使用者介面 RStudio (http://www.rstudio.com)

先安裝主程式，再安裝 RStudio (事實上，R有非常多使用者介面可以選擇)

<br>

        下載玩R後，會有很多Packages，必須先載下來，再library進去才能使用

-----

<img src="use pakage.jpg"  width="400" height="350"  />

<br><br>

 
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

 Basic Data Types
====
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

>口訣 : 把R當成Excel


      在R中，最基本的三種資料型態:
      
      -  「數字」的　numeric  (integer、double)
      -  「布林」的　logical  (TRUE、FALSE)
      -  「字串」的　character

-----

<img src="type.jpg" />


<br><br>

 

###1. 利用 numeric 資料型態來進行四則運算
  
  
```{r }
# <- and = ，在R中都是等於的意思，可是一定要用 <- (快捷鍵:ALT + -)
# 因為這樣比較酷(我是認真的，這樣真的比較酷)

# integer(占的記憶體比較小)
# double (占的記憶體比較大)

#將值輸入Mike和Joe兩個variable之中
Mike <- 20
Joe  <- 20

Total_Age <- Joe + Mike

print(Total_Age)

```

```{r}
# class()這個函數，可以知道變數的資料型態
class(Mike)
class(Total_Age)
```


<br>

###2. logic介紹與加減

-  布林值(Boolean)，只有兩個值 : TRUE、FALSE
-  布林值是**可以加減的**(TRUE = 1 and FALSE = 0)
```{r}
x <- TRUE
y <- FALSE

class(x)

## 任何邏輯判斷式的output都是布林值
1 < 2

1 > 2


## 布林值的運算
print(x + y)
```

<br>

###3. character介紹

-  口訣 : 跟Excel一模一樣的字串定義方法
-  只要用 "Taiwan" or 'Taiwan' 包起來的東西都是字串
-  通常都是用"Taiwan"，但在程式上需要，會有字串中還有字串的現象(" 'Taiwan' ")
```{r}
x <- "I love coffee"
y <- "I love tea"
class(x)
```

<br>

**小習題** :　c()、paste0、str_c 差別在哪裡
-  利用length函數檢查output看看吧
```{r include = F   }
library(stringr)
```

```{r size=10}

paste0(x,y)
str_c(x,y)
c(x,y)
```

字串的處理非常重要，之後再討論Text Mining時，會提到更深入地處理方式(正規表示法)。

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

**資料儲存方式 -- 向量** 
====
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

##1. vector :

請把R中所有的變數，都以向量當成儲存單位 :

-  **使用口訣**:把每一個變數都當成excel中的**一行(A1:Z1)**
    -  如果只有A1有數字 : 長度為**1**的向量
    -  如果A1和B1有數字 : 長度為**2**的向量
    
-  向量具有同質性(homogeneous)
-  R中建立向量的方式為 : c( )


**Creating Vectors**
```{r}
c(2, 4, 6)
1:10
seq(2, 3, by=0.5)
```


<br>

```{r}
# 長度為1的向量
x <- 100
# 長度為5的向量
y <- c(1, 2, 3 ,4 ,5 )

length(y)

```

<br>

```{r}
# 向量可以疊加
z <- c(x,y)
print(z)
length(z)
# homogeneous
# 不論輸入什麼資料進去vector，都只有一種資料型態
class(z)
```

<br>

###1.1. 試想，你常常在excel中做什麼樣的運算 :

-  整排同(+ - * /)一個數
-  整排做邏輯判斷
-  找兩個長度相同的column(兩條向量)來跑回歸(相關係數)

不覺得R跟excel的概念都是一樣的嗎?
```{r}
# 長度為20的向量
x <- 1:20

print(x)

# 向量的四則運算都是整條同時進行
x*10

x > 10



```

<br>
把布林值都加起來
```{r}
x_logic <- x > 10
# 整條布林值的四則運算
sum(x_logic)

```

<br>
20個布林值的平均數
```{r}
# mean = excel中的average
mean(x_logic)
```
<br>
```{r}

# 把布林值轉換成數字
x_logic*1
```


<br>

**相關係數**

```{r}
x <- 1:10
y <- c(1,4,5,6,8,3,7,10,12,13)

```

```{r}
print(x)
print(y)
```

```{r}
# correlation test
cor.test(x,y)
```

<br>

##2. data.frame :

-  data.frame，就是Excel的工作表
-  vector只能包含一種資料型態，data.frame的column可想像成很多個vector串聯組成，如此一來就能包含多種資料型態(每個column -- 一種資料型態)
-  heterogeneous



```{r include = FALSE}
library(tidyverse)
```


```{r}
# 顯示前10個variable
# 每一個column有自己的資料型態

head(mpg, 10)

```
<br>

####Example :
<img src="mpg.jpg"  />

<br>
```{r}
# 了解資料
# Data column name
names(mpg)
# Data Stucture
str(mpg)
# Data summary
summary(mpg)
```
所有函數名稱都很直觀，當你有想用的函數時，可以嘗試去猜名稱

##3. matrix :

```{r}
# 較無結構的矩陣，內容與dataframe一樣
A <- matrix(c(1:6),2,3)
#參教依序為資料、列數、行數，是否依列填入資料
show(A)
```
```{r}
B <- matrix(c(1:6),2,3,byrow=TRUE)
#若byrow=true，則依列填入資料，預設為false
show(B)
```
<br>

**3.1. 矩陣的運算 (1)**

```{r}
X1 <- matrix(c(1:4), nrow=2, ncol=2)
show(X1)
X1+1
X1^2
```
<br>

**3.2. 矩陣的運算 (2): 轉置矩陣**

```{r}
X2 <- matrix(c(4:9),nrow=2, ncol=3)
show(X2)
t(X2)
```
<br>

**3.3. 矩陣的運算 (3): 維度計算**
```{r}
dim(X2)
nrow(X2)
ncol(X2)
```
<br>

**3.4. 矩陣的運算 (4): 反矩陣**
```{r}
X3 <- solve(X1) # inverse matrix
X1*X3 # 元素間相加
X1%*%X3  # 矩陣相乘

```
<br>

**3.5. 矩陣的運算 (5): Joining Rows (Columns)**
```{r}
X1 <- matrix(c(1:6),nrow=2, ncol=3)
X2 <- matrix(c(4:9),nrow=2, ncol=3)
rbind(X1,X2)  #Joining Rows 矩陣合併

cbind(X1,X2)  #Joining Columns 矩陣合併

```
<br>

**3.2 Data type transfer**
```{r}
A <- matrix(c(1:6),2,3) 
colnames(A)=c("C1", "C2","C3")
B <- data.frame(A)
show(B)

show(B$C1)

str(B)
```



##4. list :

-  list是R特有的資料儲存方式，屬於異質性的vector
-  一般的vector，一個位置就只能放一個值(numeric、logical、character)
-  list的位置，能放任何東西(list、vector、data.frame)
-  heterogeneous
```{r}
# 沒有標名子的list
x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))
x1
x2
x3
```
####I’ll draw them as follows::
<img src="list_draw.jpg"  />

####A list with title names::
```{r}
# 有標名子的list

Mike <- list(age = 20, gender = "male", 
             drama = c("天龍八部","倚天屠龍記"), 
             dataframe = head(mpg[,1:5]))

print(Mike)

str(Mike)

```

<br>

####A list without title names::
```{r}
# 沒有標名子的list
# 他就用[[]] -- 標記順序

Mike <- list( 20,
              "male", 
              c("天龍八部","倚天屠龍記"), 
              head(mpg[,1:5]))

```

####replace title names by [ [ ] ]::

```{r}

print(Mike[1:3])

str(Mike[1:3])
```

<br>

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

讀取資料元素
====
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

現在我們知道R是怎麼儲存資料了!那我們要怎麼把存好的資料取出來呢?這裡的讀去資料是指**subset**，並不只是觀察資料長什麼樣子，而是從一個大的Dataset中，讀取我們想要的部分就好

      本節目標 : 
      
      -  怎麼讀取vector
      -  怎麼讀取data.frame (matrix)
      -  怎麼讀取list


**1. 所有形式括號的用途 : **
```{r results = F }
# 用中括號來做讀取動作

"1.  [ ]   -- 用來選取"   
"2. [[ ]]  -- 用來選取"  

"3.  ( )   -- 用來建立vector"     
"4.  { }    -- 用來包住Loop、if-else判斷式、Function"
```
**2. 用名子來讀取資料 : **
```{r results = F }
"用 variable name + $ + title name 讀取資料"   
```

----- 


###**vector**
```{r}
FGU_number <- c(1,0,3,1,3,3,2,1)

# 讀取vector中第 1 個字元
FGU_number[1]
# 讀取vector中第 3 個字元
FGU_number[3]
```

<br>
<br>

###**data.frame**

----- 


<img src="select_DF.jpg"  />

<br>
```{r}
test <- head(mpg,5)
print(test)
```

<br>

先讀**row**再讀**column**
```{r results = F }
"data.frame[ (for row) , (for column) ]" 
```

```{r}
# 讀第3條row
test[3,]

# 讀第4條column(name = "year")
test[,4]
# 用title name來讀取
test[,"year"]

```

<br>

**練習 : 讀取第4條column -- "year"中的2008**

```{r}
# 先讀row再讀column
test[3,4]

# title name -- 一定要用 " " 括起來
test[3,"year"]

```
-  **~~test [ 3, year ]~~**

<br>


**練習 : 用錢符號讀取"year"中的2008**
```{r}
# 用 錢符號選取 ( $ ) 
test$year
test$year[3]
```

<br>

**練習 : 用vector取值**


```{r }
# select by []

test[2:3,"year"]

test[,c(1,5)]

```

<br>

**練習 : 用減號(-)來刪除**


```{r }

test[,-c(1:7)]

```

<br>

#####**Important Practice :**

-----

    請問 test $ year 和 test [ , "year" ] 的差別
    

```{r results = F}
# select by []
test[,"year"]
test[3,"year"]

# select by [[]]
test[["year"]]
test[["year"]][3]

# select by $
test$year
test$year[3]
```

-----

<br>

##**list**

把list想像一輛列車，怎麼讀取列車中**不同節次**的東西我們已經會了(讀取vector or data.frame)。

因此我們只要，把想要的列車**節次**抓出來，在用一樣的方法讀取裡面的資料就行了唷

<img src="select_list.jpg"  />

<br>

```{r}
Mike <- list(age = 20, gender = "male", 
             drama = c("天龍八部","倚天屠龍記"), 
             dataframe = head(mpg[1:5,1:5]))

print(Mike)
```

<br>

**選取列車節次**
```{r}
# 第一節次
Mike[[1]]
# 第三節次
Mike[[3]]
```

<br>

**只要選取第三節次中的 -- "倚天屠龍記"**，選完列車節次，後面直接照樣貼上

```{r}
# 倚天屠龍記 br []
Mike[[3]][2]

# 倚天屠龍記 br ( $ + name )
Mike$drama[2]


```

<br>

-----

**選取列車節次**後，讀取方式都跟前面一樣

-  **以下兩項有什麼差別?**
```{r}
# select by []
Mike$dataframe[3,"year"]
# select by $
Mike$dataframe$year[3]
```

-----

###**note : **

        務必注意，資料選取完資後的資料型態 :
        
        -  Is Vector or Dataframe ?
    


<br>

-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

Programming
====
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

<br>

###For Loop***

      for (variable in sequence){
      
        -- Do something --
      
      }

-----

```{r}
for (i in 1:4){
  
  j <- i + 10
  
  print(j)
    
}
```

<br>


###While Loop

        while (condition){
        
          -- Do something --
        
        }

-----

```{r}
while (i < 5){
  
  print(i)
    
  i <- i + 1

}
```

<br>


###If Statements***

        if (condition){
        
          -- Do something --
        
        } else {
        
          -- Do something different --
        
        }

-----

```{r}

for (i in 1:6){
  
  j <- i * 10
  print(j)
  
    if (j > 59){
      print("終於過了!!!!!!!!!!!!!!!!!!!")
    } else if(j > 49 & j < 59) {
      print("還是被當..")
    } else {
      print("......")
    }
}
```

<br>


###Functions

        function_name <- function(var){
        
          -- Do something --
          
          -- return(new_variable) --
        
        }

-----

```{r}

age <- function(x){
  
  age <- round( rnorm(1, mean = 20, sd = 10) )
  
  return( paste0(x, "今年才", age, "歲") )

}
```

<br>

give a try ~
```{r}
# 我改名了
age("金城武")

age("我昨天抓到的神奇寶貝")


```

<br>



-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

Plot
====
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

<br>

###Plot (1)
```{r}
# plot(): type 參數的設定
 x <- rnorm(20) 
 par(mfrow=c(2,3))  # 以下有6個圖形，排列成 2 x 3
 plot(x, type="p", main="f1")
 plot(x, type="l", main="f2")
 plot(x, type="b", main="f3")
 plot(x, type="c", main="f4")
 plot(x, type="o", main="f5")
 plot(x, type="h", main="f6")
```


<br>

###Plot (2)
```{r}
# plot(): pch 和 lty 參數的設定
x <- rnorm(20)  
par(mfrow=c(2,3))  
# pch 的值為 0~25，呈現不同點的型態
# lty 的值為 1~6，呈現不同的線條

plot(x, type="o", main="f1", pch=0, lty=1)
plot(x, type="o", main="f2", pch=3, lty=2)
plot(x, type="o", main="f3", pch=15, lty=3)
plot(x, type="o", main="f4", pch=21, lty=4)
plot(x, type="o", main="f5", pch=22, lty=5)
plot(x, type="o", main="f6", pch=25, lty=6)
```

<br>

###Plot (3)
```{r}
# plot(): color 和 lwd 參數的設定
 x <- rnorm(20) 
 par(mfrow=c(2,3)) > # col 可以改變座標軸(col.axi), X與Y軸說明文字 (col.lab)
 # 主標題 (col.main)和 副標題 (col.sub)的顏色
 # R常用的顏色編號可直接用1~6
 plot(x, type="o", main="f1", pch=0, lty=1)
 plot(x, type="o", main="f2", xlab="x lab", col.axis=4)
 plot(x, type="o", main="f3", ylab="y lab", col.lab="red")
 plot(x, type="o", main="f4", col.main=4)
 plot(x, type="o", main="f5", col=4)
 plot(x, type="o", main="f6", col=5, lwd=3)
```


<br>

###Plot (4)
```{r}
 # mai, xaxt, axis, las, cex, font 參數
 x <- rnorm(20)  # 從常態分配N(0,1)中隨機抽出20筆資料
 par(mfrow=c(2,3), mai=c(0.7,0.3,0.3,0.3)) # 以下有六個圖形，排列成 2 x 3
 # mai 參數可以設定小圖之間的距離
 # xaxt="n" 表示x軸將有使用者自行設定，若無設定，則為空白
 plot(x, type="o", main="xaxt", pch=0, lty=1, xaxt="n")
 # 加上 axis(1, at=..., label=,...)用以設定x軸
 plot(x, type="o", main="axis", pch=0, lty=1, xaxt="n")
 axis(1, at=seq(3,20,by=5), label=c("ABC", "TEST", "Hello", "abc"))
 # las=1 (預設值) 座標值平行，las=2 與座標軸垂直
 plot(x, type="o", main="las", pch=0, lty=1, xaxt="n")
 axis(1, at=seq(3,20,by=5), label=c("ABC", "TEST", "Hello", "abc"), las=2)
 # cex: 設定文字大小是標準字體的幾倍大
 plot(x, type="o", main="cex", pch=0, lty=1, xaxt="n")
 axis(1, at=seq(3,20,by=5), label=c("ABC", "TEST", "Hello", "abc"), cex.axis=1.5)
 plot(x, type="o", main="cex", pch=0, lty=1, xaxt="n", yaxt="n")
 axis(1, at=seq(3,20,by=5), label=c("ABC", "TEST", "Hello", "abc"), cex.axis=1.5)
 axis(2, cex.axis=1.5)
 # font: 使用文字字體的類型，1表示正常，2為粗體，3為斜體，4為斜粗體
 plot(x, type="o", main="font", pch=0, lty=1, xaxt="n")
 axis(1, at=seq(3,20,by=5), label=c("ABC", "TEST", "Hello", "abc"), font=4)
```

<br>

###Plot (5)
```{r}
xx <- c(3, 6, 6, 13, 18, 12, 9, 6, 5, 4)
yy <- c(3, 4, 6, 10, 12, 13, 15, 9, 9, 8)
plot(xx, col=1, type="l", lty=1, lwd=2, xlab="new_xlab", ylab="new_ylab")
lines(yy, col=2, type="l", lty=2, lwd=3)
legend(1, 18, c("xx","yy"), col=c(1, 2), lty=c(1, 2), lwd=c(2,3))
```











<br><br>
    
    
    
-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------

<br>
