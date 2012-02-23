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

4.4 序列 (Sequences)
===========================

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