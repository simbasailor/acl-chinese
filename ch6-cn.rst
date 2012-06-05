.. highlight:: cl
   :linenothreshold: 0

Chapter 6 函数 (Functions)
***************************************************

理解函数是理解 Lisp 的关键之一。概念上来说，函数是 Lisp 的核心所在。实际上呢，函数是一个供你使用的最有用工具之一。

6.1 全局函数 (Global Functions)
==================================

判断式 ``fboundp`` 告诉我们是否有一个函数的名字是一个给定的符号。如果一个符号是函数的名字， ``symbol-name`` 会返回它：

::

  > (fboundp '+)
  T
  > (symbol-function '+)
  #<Compiled-function + 17BA4E>

透过 ``symbol-function`` 函数配置某个名字给一个函数：

::

  (setf (symbol-function 'add2)
    #'(lambda (x) (+ x 2)))

我们可以定义一个新的全局函数，我们可以像是使用 ``defun`` 所定义的函数那样使用它：

::

  > (add2 1)
	3

实际上 ``defun`` 做了稍微多的工作，将某些像是

::

	(defun add2 (x) (+ x 2))

翻译成上述的 ``setf`` 表达式。使用 ``defun`` 让程序看起来看美观并且或许可以帮助编译器，但严格来说你不需要它来撰写程序。

藉由把 ``defun`` 的第一个参数变成一个这样形式的列表 ``(setf f)`` ，你定义了当 ``setf`` 第一个参数是一个 ``f`` 的函数调用所会发生的事情。下列这对函数把 ``primo`` 定义成 ``car`` 的同义词：

::

  (defun primo (lst) (car lst))

  (defun (setf primo) (val lst)
    (setf (car lst) val))

在一个名字是这种形式 ``(setf f)`` 的函数定义中，第一个参数代表新的数值，而剩下的参数代表了传给 ``f`` 的参数。

现在任何 ``primo`` 的 ``setf`` 会是一个调用上面後者的函数：

::

  > (let ((x (list 'a 'b 'c)))
      (setf (primo x ) 480)
      x)
  (480 b c)

不需要为了定义 ``(setf primo)`` 而定义 ``primo`` ，但这样的定义通常是成对的。

由於字串是 Lisp 表达式，没有理由它们不能出现在代码的主体。一个自成一格的字串是没有副作用的，所以不会造成任何差别，除非它是最後一个表达式。如果你想要让一个字串是由 ``defun`` 所定义的函数的主体的第一个表达式，

::

  (defun foo (x)
    "Implements an enhanced paradigm of diversity"
    x)

那麽这个字串会变成函数的文档字串 (documentation string)。一个全局定义的函数的文件可以藉由调用 ``documentation`` 来取得：

::

  > (documentation 'foo 'function)
  "Implements an enhanced paradigm of diversity"

6.2 局域函数 (Local Functions)
===============================

藉由 ``defun`` 或 ``symbol-function`` 搭配 ``setf`` 所定义的函数是全局函数。你可以像存取全局变量那样，在任何地方存取它们。定义局域函数也是有可能的，局域函数和局域变量一样，只在某些语境内可以存取。

局域函数可以使用 ``labels`` 定义，是一种像是给函数使用的 ``let`` 。它的第一个参数是一个新局域函数的列表，而不是变量规格说明的列表。每一个列表中的元素都有如下形式：

::

  (name parameters . body)

而 ``labels`` 表达式剩下的部份，调用 ``name`` 等同於调用 ``(lambda parameters . body)`` 。

::

  (labels ((add 10 (x) (+ x 10))
           (consa  (x) (cons 'a x)))
    (consa (add10 3)))

这方面与 ``let`` 类似。由 ``labels`` 表达式所定义的局域函数，可以被其他任何在此定义的函数参照，包括自己。所以是可能可以这样定义一个递归的局域函数：

::

  (labels ((len (lst)
             (if (null lst)
                 0
                 (+ (len (cdr lst)) 1))))
    (len '(a b c)))

5.2 小节展示了一个 ``let`` 表达式，怎麽理解为一个函数调用。一个 ``do`` 表达式可以同样被解释成一个递归函数的调用。一个这样形式的 ``do`` :

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

这个模型可以用来解决任何你仍然对於 ``do`` 行为有疑惑的问题。

6.3 参数列表 (Parameter Lists)
================================

6.4 示例：实用函数 (Example: Utilities)
=========================================

6.5 闭包 (Closures)
=======================================

6.6 范例：函数构造器 (Example: Function Builders)
=====================================================

6.7 动态作用域 (Dynamic Sc​​ope)
====================================================

6.8 编译 (Compilation)
========================================

6.9 使用递归 (Using Recursion)
================================================

Chapter 6 总结 (Summary)
============================

Chapter 6 练习 (Exercises)
==================================