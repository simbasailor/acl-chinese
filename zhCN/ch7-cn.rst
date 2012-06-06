.. highlight:: cl
   :linenothreshold: 0

Chapter 7 输入与输出 (Input and Output)
***************************************************

Common Lisp 有着威力强大的 I/O 工具。关於输入以及一些普遍的读取字符的函数，我们有 ``read`` 并包含了一个完整的解析器 (parser)。关於输出以及一些普遍的写出字符的函数，我们有 ``format`` ，它自身几乎就是一个语言了。本章介绍了所有基本的概念。

在 Common Lisp 有两种流 (streams)，字符流与二进制流。本章描述了字符流的操作；二进制流的操作涵盖在 14.2 节。

7.1 流 (Streams)
==================================

流是表示字符来源或终点的 Lisp 物件。要读取或写入文件，你将其作为流打开。但流与文件是不一样的。你可以在顶层读入或打印时使用一个流。你甚至可以构造可以读取或写入字串的流。

输入缺省是从 ``*standard-input*`` 流读取。输出缺省是在 ``*standard-output*`` 流。最初它们在相同的地方：一个表示顶层的流。

我们已经在顶层看过 ``read`` 与 ``format`` 是如何读取及印出的。前者接受一个应是流的选择性参数，缺省为 ``*standard-input*`` 。 ``format`` 的第一个参数可以是一个流，但当它是 ``t`` 时，输出被送到 ``*standard-output*`` 。所以我们目前为止都只用到缺省的流而已。我们可以在任何流上面做同样的 I/O 操作。

一个路径名 (pathname) 是一种指定一个文件的可携方式 (portable way)。一个路径名有六个部分：host, device, directory, name, type 及 version 。你可以透过呼叫 ``make-pathname`` 搭配一个或多个对应的关键字参数来产生一个路径。在最简单的情况下，你可以只指明名字，让其他的部分设为缺省：

::

  > (setf path (make-pathname :name "myfile"))
	#P"myfile"

开启一个文件缺省的基本函数是 ``open`` 。它接受一个路径名 [1]_ 以及大量的选择性关键字参数，而要是开启成功时，返回一个指向文件的流。

你可以在构造流时，指定你想要怎麽使用它。 无论你是要写入流、从流读取或是两者皆是， ``:direction`` 参数会发信号表示。三个对应的数值是 ``:input`` , ``output`` , ``:io`` 。如果是用来输出的流， ``if-exists`` 参数说明了如果文件已经存在时该怎麽做；通常它应该是 ``:supercede`` 。所以要构造一个可以写入 "myfile" 文件的流，你可以：

::

  > (setf str (open path :direction :output
                         :if-exists :supercede))
  #<Stream C017E6>

流的印出表示法 (printed-representation) 因实现而异。

现在我们可以把这个流当成 ``format`` 的第一个参数传入，它会在流印出，而不是顶层：

::

	> (format str "Something~%")
	NIL

如果我们在此时检视这个文件，输出也许会丶也许不会在那里。某些实现会将输出储存成一块 (chunks)再写出。它也许不会出现，直到我们将流关闭：

::

	> (close str)
	NIL

当你使用完时，总是记得关闭文件；在你还没关闭之前，内容是不保证会出现的。现在如果我们检视文件 "myfile" ，应该有一行：

``Something``

如果我们只想从文件读取，我们可以开启一个流搭配 ``:direction :input`` ：

::

	> (setf str (open path :direction :input))
	#<Stream C01C86>

我们可以对文件使用任何输入函数。7.2 节会更详细的描述输入部分。下面是一个我们将使用 ``read-line`` 来从文件读取一行文字的范例：

::

	> (read-line str)
	"Something"
	> (close str)
	NIL

当你读取完毕时，记得关闭文件。

大部分时间我们不使用 ``open`` 与 ``close`` 来操作文件的输入输出。 ``with-open-file`` 宏通常更方便。它的第一个参数应该是一个列表，包含了变数名以及伴随你想传给 ``open`` 的参数。在这之後它接受一个代码主体，它会与随後传给 ``open`` 参数所构造的流一起被求值。然後这个流会被自动关闭。所以整个文件写入动作可以表示为：

::

  (with-open-file (str path :direction :output
                            :if-exists :supercede)
    (format str "Something~%"))

``with-open-file`` 宏将 ``close`` 放在 ``unwind-protect`` 里 (参见 92 页)，即使一个错误打断了主体的求值，文件是保证会被关闭的。

译注: 92 页是 5.6 小节。

7.2 输入 (Input)
===============================

7.3 输出 (Output)
================================

7.4 示例：字串代换 (Example: String Substitution)
==============================================================

7.5 宏字符 (Macro Characters)
=======================================

Chapter 7 总结 (Summary)
============================

Chapter 7 练习 (Exercises)
==================================