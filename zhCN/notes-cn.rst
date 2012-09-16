Notes 备注
******************************



备注 154 (Notes 154)
==================================

下面这个函数会显示在一个给定实现中，16 个用来标示浮点表示法的限制的全局常量：

::

	(defun float-limits ()
	  (dolist (m '(most least))
	    (dolist (s '(positive negative))
	      (dolist (f '(short single double long))
	        (let ((n (intern (string-upcase
	                            (format nil "~A-~A-~A-float"
	                                          m  s  f)))))
	          (format t "~30A ~A ~%" n (symbol-value n)))))))

备注 164 (Notes 164)
==================================

`快速排序演算法 <http://zh.wikipedia.org/zh-cn/%E5%BF%AB%E9%80%9F%E6%8E%92%E5%BA%8F>`_\ 由\ `霍尔 <http://zh.wikipedia.org/zh-cn/%E6%9D%B1%E5%B0%BC%C2%B7%E9%9C%8D%E7%88%BE>`_\ 于 1962 年发表，并被描述在 Knuth, D. E. *Sorting and Searching.* Addison-Wesley, Reading (MA), 1973.一书中。

备注 173 (Notes 173)
==================================

`Foderaro, John K.  Introduction to the Special Lisp Section. CACM 34:9 (Setember 1991), p.27 <http://www.informatik.uni-trier.de/~ley/db/journals/cacm/cacm34.html>`_

备注 176 (176)
================

关于 CLOS 更详细的信息，参考下列书目：

Keene, Sonya E. `Object Oriented Programming in Common Lisp <http://en.wikipedia.org/wiki/Object-Oriented_Programming_in_Common_Lisp:_A_Programmer's_Guide_to_CLOS>`_ , Addison-Wesley, Reading (MA), 1989

Kiczales, Gregor, Jim des Rivieres, and Daniel G. Bobrow. `The Art of the Metaobject Protocol <http://en.wikipedia.org/wiki/The_Art_of_the_Metaobject_Protocol>`_ MIT Press, Cambridge, 1991

备注 178 (178)
================

让我们再回放刚刚的句子一次： *我们可以完成种种的改动，甚至不需要看程序中其他的代码一眼。* 这个想法或许对某些读者听起来担忧地熟悉。这是写出\ `面条式代码 <http://zh.wikipedia.org/wiki/%E9%9D%A2%E6%9D%A1%E5%BC%8F%E4%BB%A3%E7%A0%81>`_\ 的食谱。

面向对象模型使得通过一点一点的来构造程序变得简单。但这通常意味著，在实践上它提供了一种有结构的方法来写出面条式代码。这不一定是坏事，但也不会是好事。

很多现实世界中的代码是面条式代码，这也许不能很快改变。针对那些终将成为面条式代码的程序来说，面向对象模型是好的：它们最起码会是有结构的面条。但针对那些也许可以避免误入崎途的程序来说，面向对象抽象只是更加危险的，而不是有用的。

备注 213 (Notes 213)
==================================

Knuth, Donald E. `Structured Programming with goto Statements <http://sbel.wisc.edu/Courses/ME964/Literature/knuthProgramming1974.pdf>`_ *Computing Surveys* , 6:4 (December 1974), pp. 261-301

备注 214 (Notes 214)
==================================

Knuth, Donald E. `Computer Programming as an Art <http://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&cad=rja&ved=0CC4QFjAB&url=http%3A%2F%2Fawards.acm.org%2Fimages%2Fawards%2F140%2Farticles%2F7143252.pdf&ei=vl9VUIWBIOWAmQWQu4FY&usg=AFQjCNHAgYS4PiHA0OfgOdiDfPU2i6HAmw&sig2=zZalr-ife4DB4BR2CPORBQ>`_ *In ACM Turing Award Lectures: The First Twenty Years.* ACM Press, 1987

备注 248 (Notes 248)
==================================

关于更深入讲述逻辑推论的资料，参见：\ `Stuart Russell <http://www.cs.berkeley.edu/~russell/>`_ 及 `Peter Norvig <http://www.norvig.com/>`_ 所著的 `Artificial Intelligence A Modern Approach <http://aima.cs.berkeley.edu/>`_\ 。
