.. highlight:: cl
   :linenothreshold: 0

Chapter 4 特殊資料結構 (Specialized Data Structure)
***************************************************

在之前的章節裡，我們討論了列表，Lisp 最多功能的資料結構。本章展示如何使用 Lisp 其它的資料結構：陣列 (包含向量與字串），結構以及雜湊表。他們或許不像列表這麼彈性，但他們可以更快地存取並使用更少空間。

Common Lisp 有另一個資料結構：實體 (instance)。實體在 11 章討論，講述 CLOS。

4.1 陣列 (Array)
===================

在 Common Lisp ，你可以呼叫 ``make-array`` 創建一個陣列，第一個參數為一個列表，指定陣列的維度。要創建一個 ``2 x 3`` 的陣列我們可以：

::

   > (setf arr (make-array '(2 3) :initial-element nil))
   #<Simple-Array T (2 3) BFC4FE>

陣列在 Common Lisp 裡至少可以有七個維度，每個維度至少可以有 1023 個元素。

``:initial-element`` 參數是選擇性的。如果提供了這個參數，整個陣列會用指定的值作初始化。想要取出一個未初始化的陣列內元素是沒有定義的 (undefined)。

取出陣列內的元素我們呼叫 ``aref`` 。跟 Common Lisp 的存取函數相同， ``aref`` 是零索引的 (zero-indexed)：

::

   > (aref arr 0 0)
   NIL

要替換陣列的某個元素，我們使用 ``setf`` 和 ``aref`` ：

::

   > (setf (aref arr 0 0) 'b)
   B
   > (aref arr 0 0) 
   B

要表示一個字面陣列 (literal array)，我們使用 ``#na`` 語法，其中 n 是陣列的維度。舉例來說，我們可以這樣表示一個跟 ``arr`` 一樣的陣列：

::

   #2a((b nil nil) (nil nil nil))

如果全域變數 ``*print-array*`` 為真，陣列會用這種形式顯示：

::

   > (setf *print-array* t)
   T
   > arr
   #2A((B NIL NIL) (NIL NIL NIL))

如果我們只想要一個一維的陣列，你可以給 ``make-array`` 第一個參數一個整數，而不是一個列表：

::

   > (setf vec (make-array 4 :initial-elment nil))
   #(NIL NIL NIL NIL)

一個一維陣列又稱為向量 ( *vector* )。你可以用一個步驟來創建及填滿向量，藉由呼叫 ``vector`` ，它會回傳一個無論你給入什麼參數的向量：

::

   > (vector "a" 'b 3)
   #("a" b 3)

一個字面向量 (literal vector)可以用這種語法表達，如同字面陣列可以表示成 ``#na`` 。

你可以用 ``aref`` 來存取向量，但有一個更快的函數叫做 ``svref`` 給存取向量使用。

::

   > (svref vec 0)
   NIL

在 ``svref`` 內的 "sv" 代表 "簡單向量" ("simple vector") ，它是所有向量的預設值。 [1]_

4.2 範例：二分搜索 (Example: Binary Search)
=============================================

作為一個範例，這小節展示如何寫一個在排序好的向量裡搜索一個物件的函數。如果我們知道一個向量是排序好的，我們可以比 ``find`` (65頁）做的更好， ``find`` 必須依序檢視每一個元素。取而代之的，我們跳到向量中間開始。如果中間的元素是我們要找的物件，搜索完畢。不然，我們持續往左半部或往右半部搜索，取決於物件是小於或大於中間的元素。

圖 4.1 包含了一個這樣工作的函數。其實這兩個函數： ``bin-search`` 設置初始範圍及發送控制信號給 ``finder`` ，它尋找向量 ``vec`` 內 ``obj`` 是否介於 ``start`` 及 ``end`` 之間。

::

   (defun bin-search (obj vec)
     (let ((len (length vec)))
       (and (not (zerop len))
            (finder obj vec 0 (- len 1)))))

   (defun finder (obj vec start end)
     (let ((range (- end start)))
       (if (zerop range)
           (if (eql obj (aref vec start))
               obj
               nil)
           (let ((mid (+ start (round (/ range 2)))))
             (let ((obj2 (aref vec mid)))
               (if (< obj obj2)
                   (finder obj vec start (- mid 1))
                   (if (> obj obj2)
                       (finder obj vec (+ mid 1) end)
                       obj)))))))

圖 4.1: 搜索一個排序好的向量

如果要找的 ``range`` 縮小至一個元素，而如果這個元素是 ``obj`` 的話，則 ``finder`` 回傳這個元素，反之回傳 ``nil`` 。如果 ``range`` 包含了數個元素，我們比對 ``middle`` ( ``round`` 回傳離參數最近的整數) 與 ``obj2`` 。如果 ``obj`` 小於 ``obj2`` ，則繼續遞迴地往向量的左半部尋找。如果 ``obj`` 大於 ``obj2`` ，則繼續遞迴地往向量的右半部尋找。剩下的一個選擇是 ``obj=obj2`` ，這個情況我們找到要找的元素，直接返回這個元素。

如果我們插入下面這行至 ``finder`` 的起始處：

::

   (format t "~A~%" (subseq vec start (+ end 1)))

我們可以觀察被搜索的元素的數量是每一步往左減半的：

::

   > (bin-search 3 #(0 1 2 3 4 5 6 7 8 9))
   #(0 1 2 3 4 5 6 7 8 9)
   #(0 1 2 3)
   #(3)
   3


4.3 字元與字串 (Strings and Characters)
=============================================

字串是字元向量。我們用一系列由雙引號包住的字元來表示一個字串常數，一個字元 ``c`` 用 ``#\c`` 表示。

每個字元都有一個相關的整數--，通常是用ASCII碼，但不一定是。在多數的 Lisp 實現裡，函數 ``char-code`` 回傳與字元相關的數字，而 ``code-char`` 回傳與數字相關的字元。

字元比較函數 ``char<`` (小於)， ``char<=`` (小於等於)， ``char=`` (等於)， ``char>=`` (大於等於)， ``char>`` (大於)，以及 ``char/=`` (不同)。他們的工作方式和 146 頁(譯註 9.3 節)的數字比較運算元一樣。

::

   > (sort "elbow" #'char<)
   "below"

因為字串是向量，序列與陣列的函數都可以給字串使用。你可以使用 ``aref`` 來取出元素，舉例來說，

::

   > (aref "abc" 1)
   #\b

但對一個字串，你可以使用更快的 ``char`` 函數：

::

   > (char "abc" 1)
   #\b

你可以使用 ``setf`` 搭配 ``char`` (或 ``aref`` )來替換元素：

::

   > (let ((str (copy-seq "Merlin")))
       (setf (char str 3) #\k)
       str)

如果你想要比較兩個字串，你可以使用通用的 ``equal`` 函數，但還有一個忽略大小寫的比較函數 ``string-equal`` ：

::

   > (equal "fred "fred")
   T
   > (equal "fred" "Fred")
   NIL
   >(string-equal "fred" "Fred")
   T

Common Lisp 提供大量的操控及比較字串的函數。他們收錄在附錄D，從 364 頁開始。

有很多種方式可以創造一個字串。最普遍的方式是使用 ``format`` 。將第一個參數設為 ``nil`` 來呼叫 ``format`` ，使它回傳一個它本來會印出來的字串：

::
   
   > (format nil "~A or ~A" "truth" "dare")
   "truth or dare"

但若你只想把數個字串連結起來，你可以使用 ``concatenate`` ，它接受一個指定型態的符號，加上一個或多個序列：

::

   > (concatenate 'string "not " "to worry")
   "not to worry"

4.4 序列 (Sequences)
===========================

在 Common Lisp 裡， 序列型態包含了列表與向量（因此也包含了字串）。有些我們在列表上使用的函數其實是序列函數，包括 ``remove`` , ``length`` , ``subseq`` , ``reverse`` , ``sort`` , ``every`` 以及 ``some`` 。所以 46 頁 （譯註 3.11 的 ``mirror?`` 函數）我們寫的函數，對其他種類的序列也有效：

::

   > (mirror? "abba")
   T

我們已經看過四個取出序列元素的函數： 給列表使用的 ``nth`` ， 給向量使用的 ``aref`` 及 ``svref`` ，以及給字串使用的 ``char`` 。 Common Lisp 也提供了函數 ``elt`` 對任何種類的序列都有效：

::

   > (elt '(a b c) 1)
   B

針對特定型態的序列，我們之前見過的存取函數應該比較快，所以使用 ``elt`` 是沒有意義的，除非在程式碼中，有普遍要支援序列的地方..xxx...。



使用 ``elt`` ，我們可以寫一個對於向量更有效率的 ``mirror?`` 版本：

::

   (defun mirror? (s)
     (let ((len (length s)))
       (and (evenp len)
            (do ((forward 0 (+ forward 1))
                 (back (- len 1) (- back 1)))
                ((or (> forward back)
                     (not (eql (elt s forward)
                               (elt s back))))
                 (> forward back))))))

這個版本也可以給列表使用，但這個實現比較適合向量使用。頻繁的對列表呼叫 ``elt`` 是昂貴的，因為列表僅允許循序存取。在向量裡，向量允許隨機存取，存取每一個元素都是容易的。

很多序列函數接受一個或多個關鍵字參數，：

+-----------+------------------------+----------+
| parameter |         purpose        |  default |
+===========+========================+==========+
| :key      | 應用至每個元素的一個函數   | identity |
+-----------+------------------------+----------+
| :test     | 作為比較的測試函數        | eql      |
+-----------+------------------------+----------+
| :from-end | 若為真，從結尾開始作用。   | nil      |
+-----------+------------------------+----------+
| :start    | 起始位置                | 0        |
+-----------+------------------------+----------+
| :end      | 結束位置                | nil      |
+-----------+------------------------+----------+

+------------+------------+-----------+ 
| Header 1   | Header 2   | Header 3  | 
+============+============+===========+ 
| body row 1 | column 2   | column 3  | 
+------------+------------+-----------+ 
| body row 2 | Cells may span columns.| 
+------------+------------+-----------+ 
| body row 3 | Cells may  | - Cells   | 
+------------+ span rows. | - contain | 
| body row 4 |            | - blocks. | 
+------------+------------+-----------+

test

4.5 範例：解析資料 (Example: Parsing Data)
=============================================

4.6 結構 (Structures)
===========================

4.7 範例：二元搜索樹 (Example: Binary Search Tree)
======================================================

4.8 雜湊表 (Hash Table)
=====================================


.. rubric:: 腳註

.. [1] 一個簡單的陣列是不可調整的(neither adjustable)、不可替換的 (nor displaced)，且沒有填充指標 (fill-pointer)。陣列預設是簡單的。一個簡單向量是一個一維簡單陣列，可以含有任何型態的元素。