.. highlight:: cl
   :linenothreshold: 0

Chapter 6 函數 (Functions)
***************************************************

理解函數是理解 Lisp 的關鍵之一。概念上來說，函數是 Lisp 的核心所在。實際上呢，函數是一個任你使用的最有用工具之一。

6.1 全域函數 (Global Functions)
==================================

判斷式 ``fboundp`` 告訴我們是否有一個函數的名字是一個給定的符號。如果一個符號是函數的名字， ``symbol-name`` 會回傳它：

::

  > (fboundp '+)
  T
  > (symbol-function '+)
  #<Compiled-function + 17BA4E>

透過 ``symbol-function`` 函數設定某個名字給一個函數：

::

  (setf (symbol-function 'add2)
    #'(lambda (x) (+ x 2)))

我們可以定義一個新的全域函數，我們可以像是使用 ``defun`` 所定義的函數那樣使用它：

::

  > (add2 1)
	3

實際上 ``defun`` 做了稍微多的工作，將某些像是

::

	(defun add2 (x) (+ x 2))

翻譯成上述的 ``setf`` 表達式。使用 ``defun`` 讓程式看起來看美觀並且或許可以幫助編譯器，但嚴格來說你不需要它來撰寫程式。

藉由把 ``defun`` 的第一個參數變成一個這樣形式的列表 ``(setf f)`` ，你定義了當 ``setf`` 第一個參數是一個 ``f`` 的函數呼叫所會發生的事情。下列這對函數把 ``primo`` 定義成 ``car`` 的同義詞：

::

  (defun primo (lst) (car lst))

  (defun (setf primo) (val lst)
    (setf (car lst) val))

在一個名字是這種形式 ``(setf f)`` 的函數定義中，第一個參數代表新的數值，而剩下的參數代表了傳給 ``f`` 的參數。

現在任何 ``primo`` 的 ``setf`` 會是一個呼叫上面後者的函數：

::

  > (let ((x (list 'a 'b 'c)))
      (setf (primo x ) 480)
      x)
  (480 b c)

不需要為了定義 ``(setf primo)`` 而定義 ``primo`` ，但這樣的定義通常是成對的。

由於字串是 Lisp 表達式，沒有理由它們不能出現在程式碼的主體。一個自成一格的字串是沒有副作用的，所以不會造成任何差別，除非它是最後一個表達式。如果你想要讓一個字串是由 ``defun`` 所定義的函數的主體的第一個表達式，

::

  (defun foo (x)
    "Implements an enhanced paradigm of diversity"
    x)

那麼這個字串會變成函數的文件字串。一個全域定義的函數的文件可以藉由呼叫 ``documentation`` 來取得：

::

  > (documentation 'foo 'function)
  "Implements an enhanced paradigm of diversity"

6.2 區域函數 (Local Functions)
===============================

藉由 ``defun`` 或 ``symbol-function`` 搭配 ``setf`` 所定義的函數是全域函數。你可以像存取全域變數那樣，在任何地方存取它們。定義區域函數也是有可能的，區域函數和區域變數一樣，只在某些語境內可以存取。

區域函數可以使用 ``labels`` 定義，是一種像是給函數使用的 ``let`` 。它的第一個參數是一個新區域函數的列表，而不是變數規格說明的列表。每一個列表中的元素都有如下形式：

::

  (name parameters . body)

而 ``labels`` 表達式剩下的部份，呼叫 ``name`` 等同於呼叫 ``(lambda parameters . body)`` 。

::

  (labels ((add 10 (x) (+ x 10))
           (consa  (x) (cons 'a x)))
    (consa (add10 3)))

這方面與 ``let`` 類似。由 ``labels`` 表達式所定義的區域函數，可以被其他任何在此定義的函數參照，包括自己。所以是可能可以這樣定義一個遞迴的區域函數：

::

  (labels ((len (lst)
             (if (null lst)
                 0
                 (+ (len (cdr lst)) 1))))
    (len '(a b c)))

5.2 小節展示了一個 ``let`` 表達式，怎麼理解為一個函數呼叫。一個 ``do`` 表達式可以同樣被解釋成一個遞迴函數的呼叫。一個這樣形式的 ``do`` :

::

  (do ((x a (b x))
       (y c (d y)))
      ((test x y) (z x y))
    (f x y))

等同於

::

  (labels ((rec (x y)
             (cond ((test x y)
                    (z x y))
                   (t
                    (f x y)
                    (rec (b x) (d y))))))
    (rec a c))

這個模型可以用來解決任何你仍然對於 ``do`` 行為有疑惑的問題。

6.3 參數列表 (Parameter Lists)
================================

6.4 範例：實用函數 (Example: Utilities)
=========================================

6.5 閉包 (Closures)
=======================================

6.6 範例：函數建構器 (Example: Function Builders)
======================================================

6.7 動態作用域 (Dynamic Scope)
====================================================

6.8 編譯 (Compilation)
========================================

6.9 使用遞迴 (Using Recursion)
==========================================================

Chapter 6 總結 (Summary)
============================

Chapter 6 練習 (Exercises)
==================================