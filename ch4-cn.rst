.. highlight:: cl
   :linenothreshold: 0

Chapter 4 特殊数据结构 (Specialized Data Structure)
***************************************************

在之前的章节里，我们讨论了列表，Lisp 最多功能的数据结构。本章演示如何使用Lisp 其它的数据结构：数组(包含向量与字串），结构以及哈希表。他们或许不像列表这么弹性，但他们可以更快地存取并使用更少空间。

Common Lisp 有另一个数据结构：实例(instance)。实例在 11 章讨论，讲述 CLOS。

4.1 数组 (Array)
===================

在 Common Lisp 里，你可以调用 ``make-array`` 构造一个数组，第一个参数为一个列表，指定数组的维度。要构造一个 ``2 x 3`` 的数组我们可以：

::

  > (setf arr (make-array '(2 3) :initial-element nil))
  #<Simple-Array T (2 3) BFC4FE>

数组在 Common Lisp 里至少可以有七个维度，每个维度至少可以有 1023 个元素。

``:initial-element`` 参数是选择性的。如果提供了这个参数，整个数组会用指定的值作初始化。想要取出一个未初始化的数组内取出元素的行为，其后果未定义 (undefined)。

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

在 ``svref`` 内的 "sv" 代表 "简单向量" ("simple vector") ，它是所有向量的缺省值。 [1]_

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

每个字符都有一个相关的整数 ― 通常是用ASCII码，但不一定是。在多数的Lisp 实现里，函数``char-code`` 返回与字符相关的数字，而 ``code-char`` 返回与数字相关的字符。

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

有很多种方式可以创造一个字串。最普遍的方式是使用 ``format`` 。将第一个参数设为 ``nil`` 来调用 ``format`` ，使它返回一个它本来会印出来的字串：

::
   
  > (format nil "~A or ~A" "truth" "dare")
  "truth or dare"

但若你只想把数个字串连结起来，你可以使用 ``concatenate`` ，它接受一个指定類型的符号，加上一个或多个序列：

::

  > (concatenate 'string "not " "to worry")
  "not to worry"

4.4 序列 (Sequences)
===========================

在Common Lisp 里，序列类型包含了列表与向量（因此也包含了字串）。有些我们在列表上使用的函数，其实是序列函数，包括 ``remove`` , ``length`` , ``subseq`` , ``reverse`` , ``sort`` , ``every`` 以及 ``some`` 。所以 46 页（译注3.11 的 ``mirror?`` 函数）我们所写的函数，也可以用在别种序列上：

::

  > (mirror? "abba")
  T

我们已经看过四个用来取出序列元素的函数： 给列表使用的 ``nth`` ， 给向量使用的 ``aref`` 及 ``svref`` ，以及给字串使用的 ``char`` 。 Common Lisp 也提供了函数 ``elt`` ，对任何种类的序列都有效：

::

  > (elt '(a b c) 1)
  B

针对特定类型的序列，我们已经见过的存取函数应当比较快，所以使用 ``elt`` 是没有意义的，除非在代码中，有要通用地支援序列的地方。

使用 ``elt`` ，我们可以写一个对向量来说更有效率的 ``mirror?`` 版本：

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

这个版本也可以给列表使用，但这个实现更适合给向量使用。频繁的对列表调用 ``elt`` 的代价是昂贵的，因为列表仅允许循序存取。而向量允许随机存取，从任何元素来存取每一个元素都是廉价的(cheap)。

许多序列函数接受一个或多个，从这个表格所列出的标准关键字参数：

+-----------+----------------------+-----------+
| 参数      | 用途                 | 缺省值    |
+===========+======================+===========+
| :key      | 应用至每个元素的函数 | identity  |
+-----------+----------------------+-----------+
| :test     | 作来比较的函数       | eql       |
+-----------+----------------------+-----------+
| :from-end | 若为真，反向工作。   | nil       |
+-----------+----------------------+-----------+ 
| :start    | 起始位置             | 0         |
+-----------+----------------------+-----------+
| :end      | 若有给定，结束位置。 | nil       |
+-----------+----------------------+-----------+

一个接受全部关键字参数的函数是 ``position`` ，它返回序列中一个元素的位置，而未找到时，返回 ``nil`` 。我们使用 ``position`` 来演示关键字参数所扮演的角色。

::

  > (position #\a "fantasia")
  1
  > (position #\a "fantasia" :start 3 :end 5)
  4

第二个例子我们要找在第四个与第六个字符间，第一个 ``a`` ​​所出现的位置。 ``:start`` 关键字参数是第一个被考虑的元素位置，缺省是序列的第一个元素。 ``:end`` 关键字参数，如果有给的话，是第一个不被考虑的元素位置。

如果我们给入 ``:from-end`` 关键字参数，

::

  > (position #\a "fantasia" :from-end t)
  7

我们得到最靠近结尾的 ``a`` ​​的位置。但位置是用平常的方式计算；它不代表从结尾算回来的距离。

``:key`` 关键字参数是序列中每个元素在被考虑前，应用至元素的函数。如果我们询问像是这样的东西，

::

  > (position 'a '((c d) (a b)) :key #'car)
  1

那么我们要找的是元素的 ``car`` 部分是符号 ``a`` ​​的第一个元素。

``:test`` 关键字参数是一个有两个参数的函数，并定义了怎样是一个成功的匹配。它的缺省函数为 ``eql`` 。如果你想要匹配一个列表，你也许想使用 ``equal`` 来取代：

::

  > (position '(a b) '((a b) (c d)))
  NIL
  > (position '(a b) '((a b) (c d)) :test #'equal)
  0

``:test`` 关键字参数可以是任何接受两个参数的函数。举例来说，给定 ``<`` ，我们可以找到第一个比第一个参数小的元素位置：

::

  > (position 3 '(1 0 7 5) :test #'<)
  2

使用 ``subseq`` 与 ``position`` ，我们可以写出分开序列的函数。举例来说，这个函数

::

  (defun second-word (str)
    (let ((p1 (+ (position #\ str) 1)))
      (subseq str p1 (position #\ str :start p1))))

返回字串中用空格隔开的第二个单字：

::

  > (second-word "Form follows function")
  "follows"

要找到满足接受一个参数的判断式的一个元素，我们使用 ``position-if`` 。它接受一个函数与一个序列，并返回第一个满足此函数的第一个元素：

::

  > (position-if #'oddp '(2 3 4 5))
  1

它接受除了 ``:test`` 之外的所有关键字参数。

有许多相似的函数，如给序列使用的 ``member`` 与 ``member-if`` 。它们分别是， ``find`` （接受全部关键字参数）与 ``find-if`` （接受除了 ``:test`` 之外的所有关键字参数）：

::

  > (find #\a "cat")
  #\a

  > (find-if #'characterp "ham")
  #\h

不像是 ``member`` 与 ``member-if`` ，它们仅返回要寻找的物件。

通常一个 ``find-if`` 的调用，如果解读为 ``find`` 搭配一个 ``:key`` 关键字参数的话，会显得更清楚。举例来说，表达式

::

  (find-if #'(lambda (x)
               (eql (car x) 'complete))
           lst)

可以更好的解读为

::

  (find 'complete lst :key #'car)

函数 ``remove`` (22页)以及 ``remove-if`` 通常都可以用在序列。它们跟 ``find`` 与 ``find-if`` 是一样的关系。一个相关的函数是 ``remove-duplicates`` ，它只保留序列中每个元素的最后一次出现。

::

  > (remove-duplicates "abracadabra")
  "cdbra"

这个函数接受前表所列的所有关键字参数。

函数 ``reduce`` 用来把一个序列压缩成一个值。它接受至少两个参数，一个函数与一个序列。这函数必须是一个接受两个参数的函数。在最简单的情况下，函数起初用前两个元素作为参数来调用，之后接续的元素作为下次调用的第二个参数，而上次返回的值作为下次调用的第一个参数。最后调用所返回的值作为 ``reduce`` 函数的返回值。也就是说像是这样的表达式：

::

  (reduce #'fn '(a b c d))

等同于

::

  (fn (fn (fn 'a 'b) 'c) 'd)

我们可以使用 ``reduce`` 来扩充只接受两个参数的函数。举例来说，要得到三个或多个列表的交集(intersection)，我们可以：

::
  
  > (reduce #'intersection '((brad 's) (bad) (cat)))
  (A)

4.5 示例：解析日期 (Example: Parsing Dates)
=============================================

作为一个序列操作的例子，这小节演示了如何写一个程序来解析日期。我们将编写一个程序，可以接受一个像是 "16 Aug 1980" 的字串，然后返回一个表示日、月、年的整数列表。

::

  (defun tokens (str test start)
    (let ((p1 (position-if test str :start start)))
      (if p1
          (let ((p2 (position-if #'(lambda (c)
                                     (not (funcall test c)))
                                 str :start p1)))
            (cons (subseq str p1 p2)
                  (if p2
                      (tokens str test p2)
                      nil)))
          nil)))

  (defun constituent (c)
    (and (graphic-char-p c)
         (not (char= c #\ ))))

图 4.2 辨别记号(token)

图 4.2 中包含了某些我们在这应用里所需的通用解析函数。第一个， ``tokens`` ，用来从字串中取出记号(token)。给定一个字串及一个测试函数，它返回一个字符满足此函数的子字串的列表。举例来说，如果测试函数是对字母返回真的 ``alpha-char-p`` 函数，我们得到：

::

  > (tokens "ab12 3cde.f" #'alpha-char-p 0)
  ("ab" "cde" "f")

所有不满足此函数的字符被视为空白 – 他们使记号分开，但永远不是记号的一部分。

函数 ``constituent`` 被定义成用来作为 ``tokens`` 的参数。

在 Common Lisp 里， *图形字符* 是我们可见的字符，加上空白字符。所以如果我们用 ``constituent`` 作为测试函数时，

::

  > (tokens "ab12 3cde.f gh" #'constituent 0)
  ("ab12" "3cde.f" "gh")

则记号将会有一般常见的空白概念。

图 4.3 包含了特别为解析日期用的函数。这函数 ``parse-date`` 接受一个特别形式的日期，并返回一个代表其组成的整数列表：

::

  > (parse-date "16 Aug 1980")
  (16 8 1980)

::

  (defun parse-date (str)
    (let ((toks (tokens str #'constituent 0)))
      (list (parse-integer (first toks))
            (parse-month (second toks))
            (parse-integer (third toks)))))

  (defconstant month-names
    #("jan" "feb" "mar" "apr" "may" "jun"
      "jul" "aug" "sep" "oct" "nov" "dec"))

  (defun parse-month (str)
    (let ((p (position str month-names
                           :test #'string-equal)))
      (if p
          (+ p 1)
          nil)))

图 4.3 解析日期的函数

它使用 ``tokens`` 来解开一个日期字串，然后呼叫 ``parse-month`` 及 ``parse-integer`` 来解译这些元素。要找到月份，它呼叫 ``parse-month`` ，由于使用的是 ``string-equal`` 来匹配月份的名字，所以输入可以不分大小写。要找到年和日，它呼叫内建的 ``parse-integer`` ， ``parse-integer`` 接受一个字串并返回对应的整数。

如果我们需要写程序来解析整数，我们也许可以：

::

  (defun read-integer (str)
    (if (every #'digit-char-p str)
        (let ((accum 0))
          (dotimes (pos (length str))
            (setf accum (+ (* accum 10)
                           (digit-char-p (char str pos)))))
          accum)
      nil))

这个定义演示了在 Common Lisp 中，字符是如何转成数字的 – 函数 ``digit-char-p`` 不仅测试一个字符是否为数字，也返回了对应的整数。

4.6 结构 (Structures)
===========================

4.7 示例：二分搜索树 (Example: Binary Search Tree)
======================================================

4.8 哈希表 (Hash Table)
=====================================

.. rubric:: 脚注

.. [1] 一个简单的数组是不可调整的(neither adjustable)、不可替换的(nor displaced)，且没有填充指针(fill-pointer)。数组缺省是简单的。一个简单向量是一个一维简单数组，可以含有任何类型的元素。