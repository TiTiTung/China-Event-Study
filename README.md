

# <P Align=center>智能驅動投資研究</p>


>該案子由位於深圳的智道智慧科技所發起, 由中央大學與重慶郵電大學的教授群共同合作, 為大陸境內最早為機構客戶提供智能資產配置解決方案的金融科技企業。案子全名為智能事件驅動投資研究, 內容為利用財經劑量的演算法 (事件研究法), 分析大陸地區的重要事件, 並研究該事件發現前後的個股的股價變化, 研究其套利空間。

其中我所負責的工作可分為三個部分：

1. 定義有意義的事件, 從資料庫中做回測, 檢驗投資人對於事件的敏感度, 分析其是否具備研究價值 (是對於股價具有影響力)。

2. 第二部分為自然語言分析。由於必須從大陸報紙文本中定義的出有意義的事件, 我們花費大量時間在處理文本資料, 包含刪除字詞、文本意義分析、字詞敏感度分析。

3. 最後部分為R語言分析與系統開發。除了結案報告外, 我們利用R系統的Shiny套件建構了或互動式網站, 讓客戶能夠觀看自己有興趣的事件, 檢視其短期與長期績效, 以及報酬率分析。

最後為了因應大陸投資人習慣, 我們加入了「萬份收益參數」供投資人調整, 其含義為投入一萬元人民幣, 損益為多少錢。



## 智能驅動投資概念


智能事件趨動投資 (Intelligence Event-Driven Investment, IEDI) 的思路, 是以「智能」科技讓投資人在多變且覆雜的金融環境下, 面對突發「事件」能快速做出正確的「投資」決策。對於投資人而言, 「做決策」(decision making) 是最重要也是最困難的任務, 其原因來自於金融資產的市場價值往往受到各種不同因素的影響。投資人往往利用外在資訊, 經個人思考判斷, 試著做出即時、正確的決策。

<P Align=center><img src="https://i.imgur.com/RPqGTsS.png" width="60%"></p>


外在資訊可以分為兩種類型 : 結構型數據 (structured data) 和非結構型數據 (unstructured data)。而非結構型數據 (例如:新聞內容、網民評論), 由於收集、分析上並不容易。最常見的處理方式就是投資人看一看, 然後依直覺下來決策。我們應該需要更聰明、更科學的方式，來面對此類型的數據。IEDI最主要的任務不只是處理非結構型數據, 它還同時整合了傳統結構型數據庫。IEDI的思路 (如下圖所示) 即是整合文本數據以創建事件庫, 再搭配現有結構化數據即時提供有效可視化信息給投資人, 用以提高其決策質量。

>研究思路流程
>
    1. 收集新聞數據
    2. 文本數據分析
    3. 建立事件庫
    4. 相關事件分析
    5. 投資決策支持
    
<P Align=center><img src="https://i.imgur.com/3ha4JVL.png" width="70%"></p>

## IEDI 研究架構
在前一個小節中我們已經說明了IEDI的思路, 在這個小節中我們將IEDI具體的研究架構整理於下圖中, 利用收集好的金融新聞文本進行「文本預處理」的工作, 其中包括文本分割、個股代碼標示與數據清理, 然後我們將其匯整於「文本數據庫」中。利用 「智能金融新聞判別」原則篩選出特定事件, 再配合「交易數據庫」進行「事件建立處理」，完成後的結果置於「事件庫」中，未來的分析應用則 可以從事件庫取得所需要的信息，搭配「事件研究函數」有效率的進行分析。

<P Align=center><img src="https://i.imgur.com/GvwXuho.png" width="70%"></p>

>由於該專案為公司之財產, 因此無法提供數據庫。


## 實證分析成果
* **實證數據**：以隨機取樣方式獲得約45萬則新聞全文, 數據期間分布於2007年1月至2017年8月。另外取得所有上市公司的歷史交易數據進行綜合分析。由於此項目進行主要以測試為目標, 在正式應用時, 應收集更多新聞數據 (千萬則以上), 同時采用網絡爬蟲技術取得即時的數據。
* **完整案例 (定向增發)**:在這個小節中, 我們以「定向增發」為例, 說明在第二章的框架下可以獲得什麽具體的結果。首先我們仔細描述金融事件判定的原則, 進而可以得到符合條件的文本。再從短期績效、長期績效與投資分析三個面向探討該事件對於金融市場的影響。
* **部分 R code** :

```{r }
# RCode  3-1
# 定向增发

checkindx <-
grepl( '(定向增发|定增)' ,alldat$news)&
( 
!grepl('稳定增|稳定增|决定增|确定增|肯定增|一定增|铁定增加',alldat$news)& 
!grepl('(未能|终止|撤回|暂停|停止).*(定向增发|定增)',alldat$news)& 
!grepl('(定向增发|定增).*(终止|失败|撤回|暂停|停止|未能)',alldat$news)& 
!grepl('(接近|跌破).*(定向增发|定增|增发)价',alldat$news)
)

```
* **事件基本信息**：
根據RCode 3-1所制定的規則, 定向增發事件在測試的金融新聞文本數據庫中共出現了3418次。其中由於不同媒體可能報導同一個事件造成此事件被重覆計算多次, 或是事件發生後幾日不斷地被媒體討論, 因此在我們的計算中, 若同一個股同一事件在前10日內已發生過, 則該次事件不列入計算。在考慮上述二條件後, 我們留下了2853次事件, 後續其它事件皆已調整重覆發生事件。定向增發在2015年至2017年發生的次數較多, 主要的行業別是工業, 詳細的事件數據列表如圖所示。
![](https://i.imgur.com/jIkxarA.png)
<P Align=center><img src="https://i.imgur.com/cLOpzar.png" width="60%"></p>
<P Align=center><img src="https://i.imgur.com/ekuPXHv.png" width="60%"></p>
<P Align=center><img src="https://i.imgur.com/xf484mr.png" width="60%"></p>
<P Align=center><img src="https://i.imgur.com/5wnn21C.png" width="60%"></p>


* **投資分析**：
「定向增發」對於企業來說應該是一個正面的信息, 然而在我們的實證中卻得到不同的結 果。短期績效 (10日) 中的累積異常報酬並不明顯, 而長期績效 (60日) 中的累積異常報酬甚至 為反向的結果。在這個小節中, 我們擴大事件窗口為事件日前60日到事件發生後60日以, 進 一步去探討為什麽定向增發事件會產生與文獻上不同的結果。相關結果如圖所示, 其中有 二個結論是值得註意的。首先, 定向增發事件發生的期間, 市場普遍呈現多頭, 從事件發生前 60日到事件日後60日, 平均而言市場累積報酬率超過7.5%。再者, 個股的原始報酬率也與市場報酬相似呈現向上的趨勢, 但是在事件出現在媒體之前, 原始報酬率就已經上漲約7.5%, 我們認為這可能是「信息不對稱」所造成的結果。換句話說, 在中國市場中很多對於個股有利的信息在公開之前, 早已經被部分投資人所掌握, 所以提前反應了該事件的影響。
<P Align=center><img src="https://i.imgur.com/yFpnUGw.png" width="60%"></p>
<P Align=center><img src="https://i.imgur.com/ek0FLOC.png" width="60%"></p>
<P Align=center><img src="https://i.imgur.com/fSO338j.png" width="60%"></p>