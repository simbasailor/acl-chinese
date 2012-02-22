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

4.2 示例：二分搜索 (Example: Binary Search)
=============================================

4.3 字符与字串 (Strings and Characters)
=============================================

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