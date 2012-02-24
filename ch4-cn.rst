.. highlight:: cl
   :linenothreshold: 0

Chapter 4 特殊数据结构 (Specialized Data Structure)
***************************************************

在之前的章节里，我们讨论了列表，Lisp 最多功能的数据结构。本章演示如何使用Lisp 其它的数据结构：数组(包含向量与字串），结构以及哈希表。他们或许不像列表这么弹性，但他们可以更快地存取并使用更少空间。

Common Lisp 有另一个数据结构：实例(instance)。实体在 11 章讨论，讲述 CLOS。

4.1 数组 (Array)
===================

在 Common Lisp ，你可以调用 ``make-array`` 构造一个数组，第一个参数为一个列表，指定数组的维度。要构造一个 ``2 x 3`` 的数组我们可以：

::

   > (setf arr (make-array '(2 3) :initial-element nil))
   #<Simple-Array T (2 3) BFC4FE>

数组在 Common Lisp 里至少可以有七个维度，每个维度至少可以有 1023 个元素。

``:initial-element`` 参数是选择性的。如果提供了这个参数，整个数组会用指定的值作初始化。想要取出一个未初始化的数组内元素是没有定义的 (undefined)。

取出数组内的元素我们调用 ``aref`` 。跟 Common Lisp 的存取函数相同， ``aref`` 是零索引的 (zero-indexed)：

::

   > (aref arr 0 0)
   NIL

要替换数组的某个元素，我们使用 ``setf`` 和 ``aref`` ：

::

   > (setf (aref arr 0 0) 'b)
   B
   > (aref arr 0 0)
   B

要表示一个字面量数组 (literal array)，我们使用 ``#na`` 语法，其中 n 是数组的维度。举例来说，我们可以这样表示一个跟 ``arr`` 一样的数组：

::

   #2a((b nil nil) (nil nil nil))

如果全局变量 ``*print-array*`` 为真，数组会用这种形式显示：

::

   > (setf *print-array* t)
   T
   > arr
   #2A((B NIL NIL) (NIL NIL NIL))

如果我们只想要一个一维的数组，你可以给 ``make-array`` 第一个参数一个整数，而不是一个列表：

::

   > (setf vec (make-array 4 :initial-elment nil))
   #(NIL NIL NIL NIL)

一个一维数组又称为向量 ( *vector* )。你可以用一个步骤来创建及填满向量，藉由调用 ``vector`` ，它会返回一个无论你给入什么参数的向量：

::

   > (vector "a" 'b 3)
   #("a" b 3)

一个字面量向量 (literal vector)可以用这种语法表达，如同字面量数组可以表示成 ``#na`` 。

你可以用 ``aref`` 来存取向量，但有一个更快的函数叫做 ``svref`` 给存取向量使用。

::

   > (svref vec 0)
   NIL

在 ``svref`` 内的 "sv" 代表 "简单向量" ("simple vector") ，它是所有向量的预设值。 [1]_

4.2 示例：二分搜索 (Example: Binary Search)
=============================================

作为一个示例，这小节演示如何写一个在排序好的向量里搜索一个对象的函数。如果我们知道一个向量是排序好的，我们可以比 ``find`` (65页）做的更好， ``find`` 必须依序检视每一个元素。取而代之的，我们跳到向量中间开始。如果中间的元素是我们要找的对象，搜索完毕。不然，我们持续往左半部或往右半部搜索，取决于​​物件是小于或大于中间的元素。

图 4.1 包含了一个这样工作的函数。其实这两个函数： ``bin-search`` 设置初始范围及发送控制信号给 ``finder`` ，它寻找向量 ``vec`` 内 ``obj`` 是否介于 ``start`` 及 ``end`` 之间。

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
            

图 4.1: 搜索一个排序好的向量

如果要找的 ``range`` 缩小至一个元素，而如果这个元素是 ``obj`` 的话，则 ``finder`` 返回这个元素，反之返回 ``nil`` 。如果 ``range`` 包含了数个元素，我们設置 ``middle`` ( ``round`` 返回离参数最近的整数) 為 ``obj2`` 。如果 ``obj`` 小于 ``obj2`` ，则继续递归地往向量的左半部寻找。如果 ``obj`` 大于 ``obj2`` ，则继续递回地往向量的右半部寻找。剩下的一个选择是 ``obj=obj2`` ，这个情况我们找到要找的元素，直接返回这个元素。

如果我们插入下面这行至 ``finder`` 的起始处：

::

   (format t "~A~%" (subseq vec start (+ end 1)))

我们可以观察被搜索的元素的数量是每一步往左减半的：

::

   > (bin-search 3 #(0 1 2 3 4 5 6 7 8 9))
   #(0 1 2 3 4 5 6 7 8 9)
   #(0 1 2 3)
   #(3)
   3

4.3 字符与字串 (Strings and Characters)
=============================================

字串是字符向量。我们用一系列由双引号包住的字符来表示一个字串常量，一个字符 ``c`` 用 ``#\c`` 表示。

每个字符都有一个相关的整数--，通常是用ASCII码，但不一定是。在多数的Lisp 实现里，函数``char-code`` 返回与字符相关的数字，而 ``code-char`` 返回与数字相关的字符。

字符比较函数 ``char<`` (小于)， ``char<=`` (小于等于)， ``char=`` (等于)， ``char>=`` (大于等于) ， ``char>`` (大于)，以及 ``char/=`` (不同)。他们的工作方式和 146 页(译注 9.3 节)的数字比较操作符一样。

::

   > (sort "elbow" #'char<)
   "below"

因为字串是向量，序列与数组的函数都可以给字串使用。你可以使用 ``aref`` 来取出元素，举例来说，

::

   > (aref "abc" 1)
   #\b

但对一个字串，你可以使用更快的 ``char`` 函数：

::

   > (char "abc" 1)
   #\b

你可以使用 ``setf`` 搭配 ``char`` (或 ``aref`` )来替换元素：

::

   > (let ((str (copy-seq "Merlin")))
       (setf (char str 3) #\k)
       str)

如果你想要比较两个字串，你可以使用通用的 ``equal`` 函数，但还有一个忽略大小写的比较函数 ``string-equal`` ：

::

   > (equal "fred "fred")
   T
   > (equal "fred" "Fred")
   NIL
   >(string-equal "fred" "Fred")
   T

Common Lisp 提供大量的操控及比较字串的函数。他们收录在附录 D，从 364 页开始。

有很多种方式可以创造一个字串。最普遍的方式是使用 ``format`` 。将第一个参数设为 ``nil`` 来呼叫 ``format`` ，使它返回一个它本来会印出来的字串：

::
   
   > (format nil "~A or ~A" "truth" "dare")
   "truth or dare"

但若你只想把数个字串连结起来，你可以使用 ``concatenate`` ，它接受一个指定類型的符号，加上一个或多个序列：

::

   > (concatenate 'string "not " "to worry")
   "not to worry"

4.4 序列 (Sequences)
===========================

4.5 示例：解析数据 (Example: Parsing Data)
=============================================

4.6 结构 (Structures)
===========================

4.7 示例：二分搜索树 (Example: Binary Search Tree)
======================================================

4.8 哈希表 (Hash Table)
=====================================

.. rubric:: 脚注

.. [1] 一个简单的数组是不可调整的(neither adjustable)、不可替换的(nor displaced)，且没有填充指针(fill-pointer)。数组预设是简单的。一个简单向量是一个一维简单数组，可以含有任何类型的元素。