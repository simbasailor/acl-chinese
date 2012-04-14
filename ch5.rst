.. highlight:: cl
   :linenothreshold: 0

Chapter 5 控制流程 (Control)
***************************************************

在 2.2 節介紹了 Common Lisp 的求值規則，直到現在你應該很熟悉了。本章的運算元都有一個共同點，它們都違反了求值規則。他們讓你決定在程式當中何時要求值。如果普通的函數呼叫是 Lisp 程式的樹葉的話，那這些運算元就是用來建造樹枝。

5.1 區塊 (Blocks)
==========================

Common Lisp 有三個創造區塊 (block) 的基本運算元： ``progn`` , ``block`` 以及 ``tagbody`` 。我們已經看過 ``progn`` 了。在它的主體中的表達式會依序求值，並回傳最後一個表達式的值：

::

  > (progn
      (format t "a")
      (format t "b")
      (+ 11 12))
  ab
  23

由於只回傳最後一個表達式的值，代表使用 ``progn`` （或任何區塊）意味著副作用。

一個 ``block`` 像是帶有名字及緊急出口的 ``progn`` 。第一個參數應為符號。這變成了區塊的名字。在主體中的任何地方，你可以停止求值，並透過使用 ``return-from`` 指定區塊的名字，來立即回傳一個數值：

::

  > (block head
      (format t "Here we go.")
      (return-from head 'idea)
      (format t "We'll never see this."))
  Here we go.
  IDEA

呼叫 ``return-from`` 允許你的程式從程式碼的任何地方，突然但優雅地退出。第二個傳給 ``return-from`` 的參數，用來作為以第一個參數為區塊名的回傳值。在 ``return-from`` 之後的表達式不會被求值。

也有一個 ``return`` 巨集，它把傳入的參數當做封閉區塊 ``nil`` 的回傳值：

::

  > (block nil
      (return 27))
  27

許多 Common Lisp 運算元，接受一個表達式主體，皆被隱含在一個叫做 ``nil`` 的區塊裡。比如，所有由 ``do`` 建構的迭代函數：

::

  > (dolist (x '(a b c d e))
      (format t "~A " x)
      (if (eql x 'c)
          (return 'done)))
  A B C
  DONE

使用 ``defun`` 定義的函數主體，都被隱含在一個與函數同名的區塊，所以你可以：

::

  (defun foo ()
    (return-from foo 27))
	

在一個顯式或隱式的 ``block`` 外，不論是 ``return-from`` 或 ``return`` 都不會工作。

使用 ``return-from`` ，我們可以寫出一個更好的 ``read-integer`` 版本：

::

	(defun read-integer (str)
	  (let ((accum 0))
	    (dotimes (pos (length str))
	      (let ((i (digit-char-p (char str pos))))
	        (if i
	            (setf accum (+ (* accum 10) i))
	            (return-from read-integer nil))))
	    accum))

68 頁的版本在建構整數之前，需要檢查所有的字元。現在兩個步驟可以結合，因為如果我們遇到非數字的字元時，我們可以捨棄計算結果。出現在主體的原子（atom）被解讀為標籤（labels)；把這樣的標籤傳給 ``go`` ，會把控制權交給標籤後的表達式。以下是一個非常醜的程式片段，用來印出一至十的數字：

::

  > (tagbody
      (setf x 0)
      top
        (setf x (+ x 1))
        (format t "~A " x)
        (if (< x 10) (go top)))
  1 2 3 4 5 6 7 8 9 10
  NIL

這個運算元主要用來實現其它的運算元，不是你自己會使用的東西。大多數迭代運算元都有隱含一個 ``tagbody`` ，所以可能可以在主體裡（雖然很少想要）使用標籤及 ``go`` 。

你如何決定要使用哪一個區塊建構元（block construct）？幾乎所有的時間，你會使用 ``progn`` 。如果你想要突然退出的話，使用 ``block`` 來取代。多數程式設計師永遠不會顯式地使用 ``tagbody`` 。

5.2 語境 (Context)
==========================

5.3 條件 (Conditionals)
===========================

5.4 迭代 (Iteration)
==========================

5.5 多值 (Multiple Values)
=======================================

5.6 中止 (Aborts)
==========================

5.7 範例：日期運算 (Example: Date Arithmetic)
====================================================