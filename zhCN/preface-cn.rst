前言
********

本书的目的是快速而全面的教你 Common Lisp 的有关知识。它实际上包含两本书。前半部分用大量的例子来解释 Common Lisp 里面重要的概念。后半部分是一个最新 Common Lisp 辞典，它里面包括了所有 ANSI Common Lisp 的操作符。

这本书面向的读者
====================

ANSI Common Lisp 这本书适合学生或者是专业的程序员去读。本书假设读者在读它之前没有有关 Lisp 的知识。有别的程序语言的编程经验也许对读本书有益处，但也不是必须的。本书从解释 Lisp 中最基本的概念开始，并且对于初学 Lisp 的人们最容易迷惑的地方进行特别的强调。

本书也可以作为教授 Lisp 编程的课本，也可以作为人工智能课程和其他编程语言课程中有关 Lisp 部分的参考书。想要学习 Lisp 的专业程序员肯定会很喜欢贯穿于本书中的着眼于实践的理念。那些已经在使用Lisp编程的人士，也会发现，本书里面有许多很好的实例可供参考，而且，本书也是一本很方便的 ANSI Common Lisp 参考书。

如何使用这本书
====================

学习 Lisp 最好的办法就是拿它来编程。况且，在学习的同时用你学到的技术进行编程也是非常有趣的一件事。本书的编写目的就是让读者尽快的入门，在对 Lisp 进行简短的介绍之后，
第 2 章开始用 21 页的容量介绍了着手编写 Lisp 程序时可能用到所有知识。
3-9 章讲解了 Lisp 里面一些重要的知识点。这些章节特别强调了一些重要的概念比如 Lisp 里面指针的角色，如何使用递归来解决问题，以及第一级函数 (first-class function)的重要性。

针对那些想要更深层地了解 Lisp 的读者：
10-14 章包含了宏 (macro)，CLOS (Common Lisp Object System)，列表操作 (list operation)，程序优化 (optimization)，以及一些更高级的课题比如包 (package)和读取宏 (read-macro)。

15-17 章用 3 个 Common Lisp 的实际应用，总结了之前章节中讲解的知识：一个是进行逻辑推理的程序，另外一个是一个 HTML 生成器，最后一个是针对面向对象编程的嵌入式语言。

本书的最后一部分包含 4 个附录，这些附录应该对所有的读者都有用：
附录 A-D 包括了一个如何调试程序的指南， 58 个 Common Lisp 操作符的源程序，一个对 ANSI Common Lisp 和之前的 Lisp 语言的区别的总结，以及一个包括所有 ANSI Common Lisp 的参考手册。

本书还包括了一部分的备注。这些备注包括一些说明，一些参考条目，一些额外的代码，以及一些对偶然出现的不正确表述的纠正。备注在文中用一个小圆圈来表示，像这样：○

**译注: 由于小圈圈 ○ 实在太不明显了，译文中使用 λ 符号来表示备注。**

`λ <http://ansi-common-lisp.readthedocs.org/en/latest/zhCN/notes-cn.html#viii-notes-viii>`_

代码
==========

虽然本书介绍的是 ANSI Common Lisp ，但是本书中的代码可以在任何版本的 Common Lisp 中运行。那些依赖 Lisp 语言新特性的例子的旁边，会有注释告诉你如何把它们运行于旧版本的 Lisp 中。

本书中所有的代码都可以在互联网上下载到。你可以在网络上找到这些代码，它们还附带着一个免费软件的链接，一些过去的论文，以及 Lisp 的 FAQ 。还有很多有关 Lisp 的资源可以在此找到：
http://www.eecs.harvard.edu/onlisp/
源代码可以在此 FTP 服务器上下载：
ftp://ftp.eecs.harvard.edu:/pub/onlisp/
读者的问题和意见可以发送到 pg@eecs.harvard.edu 。

译注：下载的链接都坏掉了，参考这里：http://lib.store.yahoo.net/lib/paulgraham/acl2.lisp

On Lisp
==========

在整本 On Lisp 书中，我一直试着指出一些 Lisp 独一无二的特性，这些特性使得 Lisp 更像 “Lisp” 。并且我将展示一些 Lisp 能让你完成的新事情。比如说宏： Lisp 程序员能够并且经常编写一些能够写程序的程序。对于程序生成程序这种特性，因为 Lisp 是主流语言中唯一一个提供一些方便的抽象让你完成这个任务的程序语言，所以 Lisp 是主流语言中唯一一个广泛运用这个特性的语言。我非常乐意邀请那些想要更进一步了解宏和其他高级 Lisp 技术的读者读一下本书的姐妹篇： `On Lisp <http://www.paulgraham.com/onlisp.html>`_。

鸣谢
==========

在所有帮助我完成这本的朋友当中，我想特别的感谢一下 Robert Morris 。他的重要影响反应在整本书中。他的这样影响让这本书更加优秀。本书中好一些实例程序都源自他手。这些程序包括 138 页的 Henley 和 249 页的模式匹配器。

我非常的高兴我有一个高水平的技术审稿小组：Skona Brittain, John Foderaro, Nick Levine, Peter Norvig 和 Dave Touretzky 。本书中几乎所有部分都得益于它们的意见。 John Foderaro 甚至重写了本书 5.7 节中一些代码。

另外一些人通篇阅读了本书的手稿，它们是：Ken Anderson, Tom Cheatham, Richard Fateman, Steve Hain, Barry Margolin, Waldo Pacheco, Wheeler Ruml 和 Stuart Russell。特别提到的是，Ken Anderson 和 Wheeler Ruml 给予了很多很多有帮助的意见。

我非常感谢 Cheatham 教授，更广泛的说，哈佛，给我提供了编写这本书的一些必要的设施。另外也要感谢 Aiken 实验室的人员：Tony Hartman, Dave Mazieres, Janusz Juda, Harry Bochner 和 Joanne Klys。

我非常高兴能再一次有机会和 Alan Apt 一起工作。这些在 Prentice Hall 工作的人士: Alan, Mona, Pompili Shirley McGuire 和 Shirley Michaels, 与你们一起工作我很高兴。

本书用 Leslie Lamport 写的 LaTeX 进行排版。LaTeX 是在 Donald Knuth 编写的 TeX 的基础上，又加了 L.A.Carr, Van Jacobson 和 Guy Steele 所编写的宏完成。书中的图表是由 John Vlissides 和 Scott Stanton 编写的 Idraw完成的。整本书的预览是由 Tim Theisen 写的 Ghostview 完成的。 Ghostview 是根据 L. Peter Deutsch 的 Ghostscript 创建的。

我还需要感谢其他的许多人，包括：Henry Baker, Kim Barrett, Ingrid Bassett, Trevor Blackwell, Paul Becker, Gary Bisbee, Frank Deutschmann, Frances Dickey, Rich和Scott Draves, Bill Dubuque, Dan Friedman, Jenny Graham, Alice Hartley, David Hendler, Mike Hewett, Glenn Holloway, Brad Karp, Sonya Keene, Ross Knights, Mutsumi Komuro, Steffi Kutzia, David Kuznick, Madi Lord, Julie Mallozzi, Paul McNamee, Dave Moon, Howard Mullings, Mark Nitzberg, Nancy Parmet 和其家人, Robert Penny, Mike Plusch, Cheryl Sacks, Hazem Sayed, Shannon Spires, Lou Steinberg, Paul Stoddard, John Stone, Guy Steele, Steve Strassmann, Jim Veitch, Dave Watkins, Idelle and Julian Weber, the Weickers, Dave Yost 和 Alan Yuille。

另外，着重感谢我的父母和 Jackie。

`高德纳 <http://zh.wikipedia.org/zh-cn/%E9%AB%98%E5%BE%B7%E7%BA%B3>`_\ 给他的经典丛书起名为《计算机程序设计艺术》。在他的图灵奖获奖感言中，他解释说这本书的书名来源于他的内心深处的潜意识 –– 潜意识告诉他编程其实就是寻求编写最优美的程序。

就像建筑设计一样，编程即是一门工程技艺也是一门艺术。一个程序既要遵循数学原理也要符合物理定律。但是建筑师的目的不仅仅是建一个不会倒塌的建筑。更重要的是，他们要建一个优美的建筑。

像高德纳一样，很多程序员认为编程的真正目的也不仅仅是编写出正确的程序，更重要是写出优美的代码。几乎所有的 Lisp 黑客都是这么想的。 Lisp 黑客精神可以用两句话来概括：编程应该是很有趣的。程序应该很优美。而这就是我在这本书中想要传达的精神。

`保罗•格雷厄姆 (Paul Graham) <http://paulgraham.com/>`_