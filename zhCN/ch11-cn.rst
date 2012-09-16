.. highlight:: cl
   :linenothreshold: 0

Chapter 11 Common Lisp 对象系统 CLOS
**************************************************

Common Lisp 对象系统，或称为 CLOS，是一个用来实作面向对象编程的操作集。因为它们共同的历史，通常将这些操作视为一个群组。`λ <http://ansi-common-lisp.readthedocs.org/en/latest/zhCN/notes-cn.html#notes-176>`_ 技术上来说，它们与其他部分的 Common Lisp 没有不同： ``defmethod`` 只不过和 ``defun`` 一样是语言的一个整合部分。

11.1 面向对象编程 Object-Oriented Programming
======================================

面向对象编程意味著程序组织的方式的改变。这个改变跟已经发生过的处理器能力 (processor power)分配的变化雷同。在 1970 年代，一个多用户的计算机系统代表著，一个或两个大型机 (mainframe)连接到大量的\ `哑终端 <http://zh.wikipedia.org/wiki/%E5%93%91%E7%BB%88%E7%AB%AF>`_\ (dumb terminal)。现在更可能的是大量相互通过网络连接的工作站 (workstation)。系统的处理能力 (processing power)现在分布至个体用户上，而不是集中在一台大型的计算机上。

面向对象编程带来的变革与上例非常相似，前者打破了传统程序的组织方式。不再让单一的程序去操作那些数据，而是告诉数据自己该做什么，程序隐含在这些新的数据“对象”的交互过程之中。

举例来说，假设我们要算出一个二维图形的面积。一个办法是写一个单独的函数，让它检查其参数的类型，然后视类型做处理，如图 11.1 所示。

::

	(defstruct rectangle
	  height width)

	(defstruct circle
	  radius)

	(defun area (x)
	  (cond ((rectange-p x)
	         (* (rectange-height x) (rectange-width x)))
	        ((circle-p x)
	         (* pi (expt (circle-radius x) 2)))))

	> (let ((r (make-rectangle)))
	    (setf (rectangle-height r) 2
	          (rectangle-width r) 3)
	    (area r))
	6

**图 11.1: 使用结构及函数来计算面积**

::

	(defclass circle ()
	  (radius))

	(defmethod area ((x rectangle))
	  (* (slot-value x 'height) (slot-value x 'width)))

	(defmethod area ((x circle))
	  (* pi (expt (slot-value x 'radius) 2)))

	> (let ((r (make-instance 'rectangle)))
	    (setf (slot-value r 'height) 2
	          (slot-value r 'width) 3)
	    (area r))

**图 11.2: 使用类型与方法来计算面积**

使用 CLOS 我们可以写出一个等效的程序，如图 11.2 所示。在面向对象模型里，我们的程序被拆成数个独一无二的方法，每个方法为某些特定种类的参数而生。图 11.2 中的两个方法，隐性地定义了一个与图 11.1 相似作用的 ``area`` 函数，当我们调用 ``area`` 时，Lisp 检查参数的类型，并调用相应的方法。

这种将函数拆成独特方法，面向对象暗指\ *继承* (*inheritance*) –– 槽 (slot)与方法 (method)皆有继承。在图 11.2 中，作为第二个参数传给 ``defclass`` 的空列表是超类的清单。假设我们要定义一个叫上色的圆形 (colored-circle)的新种类，则上色的圆形有两个超类， ``colored`` 与 ``circle`` ：

::

	(defclass colored ()
	  (color))

	(defclass colored-circle (circle colored)
	  ())

当我们创造 ``colored-circle`` 类的实例 (instance)时，我们会看到两种继承：

1. ``colored-circle`` 的实例会有两个槽：从 ``circle`` 类继承而来的 ``radius`` 以及从 ``colored`` 类继承而来的 ``color`` 。

2. 由于没有特别为 ``colored-circle`` 定义的 ``area`` 方法存在，若我们对 ``colored-circle`` 实例调用 ``area`` ，我们会获得替 ``circle`` 类所定义的 ``area`` 方法。

实际上面向对象编程代表的是以方法、类、实例与继承来组织程序。为什么你会想这么组织程序？一个面向对象方法的主张之一是说这样使得程序更容易改动。如果我们想要改变 ``ob`` 类的对象显示的方式，我们只需要改动 ``ob`` 类的 ``display`` 方法。如果我们希望创建一个新的类，大致与 ``ob`` 一样，只在某些方面不同，我们可以创建一个 ``ob`` 类的子类。在这个子类里，我们仅改动我们想要的属性，其他所有的属性会从 ``ob`` 类缺省地继承得到。要是我们只是想让某个 ``ob`` 对象和其他的 ``ob`` 对象不一样，我们可以新建一个 ``ob`` 对象，直接修改这个对象的属性即可。若是当时的程序写的很讲究，我们可以完成种种的改动，甚至不需要看程序中其他的代码一眼。 `λ <http://ansi-common-lisp.readthedocs.org/en/latest/zhCN/notes-cn.html#notes-178>`_

11.2 类与实例 (Class and Instances)
==================================================

11.3 槽的特性 (Slot Properties)
================================

11.4 超类 (Superclasses)
===================================================

11.5 优先级 (Precedence)
=======================================

11.6 通用函数 (Generic Functions)
=======================================

11.7 辅助方法 (Auxiliary Methods)
==================================================

11.8 结合方法 (Method Combination)
=======================================

11.9 封装 (Encapsulation)
===================================

11.10 两种模型 (Two Models)
========================================

Chapter 11 总结 (Summary)
============================

Chapter 11 练习 (Exercises)
==================================