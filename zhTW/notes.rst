.. highlight:: cl


備註
******************************

本節既是備註亦作爲參考文獻。所有列於此的書籍與論文皆值得閱讀。

**譯註: 備註後面跟隨的數字即書中的頁碼**

備註 viii (Notes viii)
==================================

`Steele, Guy L., Jr. <http://en.wikipedia.org/wiki/Guy_L._Steele,_Jr.>`_\ , `Scott E. Fahlman <http://en.wikipedia.org/wiki/Scott_Fahlman>`_\ , `Richard P. Gabriel <http://en.wikipedia.org/wiki/Richard_P._Gabriel>`_\ , `David A. Moon <http://en.wikipedia.org/wiki/David_Moon>`_\ , `Daniel L. Weinreb <http://en.wikipedia.org/wiki/Daniel_Weinreb>`_ , `Daniel G. Bobrow <http://en.wikipedia.org/wiki/Daniel_G._Bobrow>`_\ , `Linda G. DeMichiel <http://www.informatik.uni-trier.de/~ley/db/indices/a-tree/d/DeMichiel:Linda_G=.html>`_\ , `Sonya E. Keene <http://www.amazon.com/Sonya-E.-Keene/e/B001ITVL6O>`_\ , `Gregor Kiczales <http://en.wikipedia.org/wiki/Gregor_Kiczales>`_\ , `Crispin Perdue <http://perdues.com/CrisPerdueResume.html>`_\ , `Kent M. Pitman <http://en.wikipedia.org/wiki/Kent_Pitman>`_\ , `Richard C. Waters <http://www.rcwaters.org/>`_\ , 以及 John L White。 `Common Lisp: the Language, 2nd Edition. <http://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html>`_ Digital Press, Bedford (MA), 1990.

備註 1 (Notes 1)
==================================

`McCarthy, John. <http://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)>`_ `Recursive Functions of Symbolic Expressions and their Computation by Machine, Part I. <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.91.4527&rep=rep1&type=pdf>`_ CACM, 3:4 (April 1960), pp. 184-195.

`McCarthy, John. <http://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)>`_ `History of Lisp. <http://www-formal.stanford.edu/jmc/history/lisp/lisp.html>`_ In `Wexelblat, Richard L. <http://en.wikipedia.org/wiki/Richard_Wexelblat>`_ (Ed.) `Histroy of Programming Languages. <http://cs305.com/book/programming_languages/Conf-01/HOPLII/frontmatter.pdf>`_ Academic Press, New York, 1981, pp. 173-197.

備註 3 (Notes 3)
==================================

`Brooks, Frederick P <http://en.wikipedia.org/wiki/Frederick_Brooks>`_\ . `The Mythical Man-Month <http://www.amazon.com/Mythical-Man-Month-Software-Engineering-Anniversary/dp/0201835959>`_\ . Addison-Wesley, Reading (MA), 1975, p. 16.

Rapid prototyping is not just a way to write programs faster or better. It is a way to write programs that otherwise might not get written at all.
Even the most ambitious people shrink from big undertakings. It's easier to start something if one can convince oneself (however speciously) that it won't be too much work. That's why so many big things have begun as small things. Rapid prototyping lets us start small.

備註 4 (Notes 4)
==================================

同上， 第 i 頁。

備註 5 (Notes 5)
==================================

Murray, Peter and Linda. `The Art of the Renaissance <http://www.amazon.com/Art-Renaissance-World/dp/0500200084>`_\ . Thames and Hudson, London, 1963, p.85.

備註 5-2 (Notes 5-2)
==================================

Janson, W.J. `History of Art <http://www.amazon.com/History-Art-H-W-Janson/dp/0810934019/ref=sr_1_1?s=books&ie=UTF8&qid=1365042074&sr=1-1&keywords=History+of+Art>`_\ , 3rd Edition. Abrams, New York, 1986, p. 374.

The analogy applies, of course, only to paintings done on panels and later on canvases. Well-paintings continued to be done in fresco. Nor do I mean to suggest that painting styles were driven by technological change; the opposite seems more nearly true.

備註 12 (Notes 12)
==================================

``car`` 與 ``cdr`` 的名字來自最早的 Lisp 實現裡，列表內部的表示法：car 代表“寄存器位址部分的內容”、cdr 代表“寄存器遞減部分的內容”。

備註 17 (Notes 17)
==================================

對遞迴概念有困擾的讀者，可以查閱下列的書籍：

Touretzky, David S. `Common Lisp: A Gentle Introduction to Symbolic Computation <http://www.amazon.com/Common-Lisp-Introduction-Computation-Benjamin-Cummings/dp/B008T1B8WQ/ref=sr_1_3?s=books&ie=UTF8&qid=1365042108&sr=1-3&keywords=A+Gentle+Introduction+to+Symbolic+Computation>`_\ . Benjamin/Cummings, Redwood City (CA), 1990, Chapter 8.

Friedman, Daniel P., and Matthias Felleisen. The Little Lisper. MIT Press, Cambridge, 1987.

譯註：這本書有再版，可在\ `這裡 <http://www.amazon.com/Common-LISP-Introduction-Symbolic-Computation/dp/0486498204/ref=sr_1_1?s=books&ie=UTF8&qid=1365042108&sr=1-1&keywords=A+Gentle+Introduction+to+Symbolic+Computation>`_\ 找到。

備註 26 (Notes 26)
==================================

In ANSI Common Lisp there is also a ``lambda`` macro that allows you to write ``(lambda (x) x)`` for ``#'(lambda (x) x)`` . Since the use of this macro obscures the symmetry between lambda expressions and symbolic function names (where you still have to use sharp-quote), it yields a specious sort of elegance at best.

備註 28 (Notes 28)
==================================

Gabriel, Richard P. `Lisp Good News, Bad News, How to Win Big <http://www.dreamsongs.com/Files/LispGoodNewsBadNews.pdf>`_ *AI Expert*\ , June 1991, p.34.

備註 46 (Notes 46)
==================================

Another thing to be aware of when using sort: it does not guarantee to preserve the order of elements judged equal by the comparison function. For example, if you sort ``(2 1 1.0)`` by ``<`` , a valid Common Lisp implementation could return either ``(1 1.0 2)`` or ``(1.0 1 2)`` . To preserve as much as possible of the original order, use instead the slower ``stable-sort`` (also destructive), which could only return the first value.

備註 61 (Notes 61)
==================================

A lot has been said about the benefits of comments, and little or nothing about their cost. But they do have a cost. Good code, like good prose, comes from constant rewriting. To evolve, code must be malleable and compact. Interlinear comments make programs stiff and diffuse, and so inhibit the evolution of what they describe.

備註 62 (Notes 62)
==================================

Though most implementations use the ASCII character set, the only ordering that Common Lisp guarantees for characters is as follows: the 26 lowercase letters are in alphabetically ascending order, as are the uppercase letters, and the digits from 0 to 9.

備註 76 (Notes 76)
==================================

The standard way to implement a priority queue is to use a structure called a heap. See: Sedgewick, Robert. `Algorithms <http://www.amazon.com/Algorithms-4th-Robert-Sedgewick/dp/032157351X/ref=sr_1_1?s=books&ie=UTF8&qid=1365042619&sr=1-1&keywords=algorithms+sedgewick>`_\ . Addison-Wesley, Reading (MA), 1988.

備註 81 (Notes 81)
==================================

The definition of progn sounds a lot like the evaluation rule for Common Lisp function calls (page 9). Though ``progn`` is a special operator, we could define a similar function:

::

	(defun our-progn (ftrest args)
	  (car (last args)))

This would be horribly inefficient, but functionally equivalent to the real ``progn`` if the last argument returned exactly one value.

備註 84 (Notes 84)
==================================

The analogy to a lambda expression breaks down if the variable names are symbols that have special meanings in a parameter list. For example,

::

	(let ((&key 1) (&optional 2)))

is correct, but the corresponding lambda expression

::

	((lambda (ftkey ftoptional)) 1 2)

is not. The same problem arises if you try to define do in terms of ``labels`` . Thanks to David Kuznick for pointing this out.

備註 89 (Notes 89)
==================================

Steele, Guy L., Jr., and Richard P. Gabriel. `The Evolution of Lisp <http://www.dreamsongs.com/Files/HOPL2-Uncut.pdf>`_\ . ACM SIGPLANNotices 28:3 (March 1993). The example in the quoted passage was translated from Scheme into Common Lisp.

備註 91 (Notes 91)
==================================

To make the time look the way people expect, you would want to ensure that minutes and seconds are represented with two digits, as in:

::

	(defun get-time-string ()
	  (multiple-value-bind (s m h) (get-decoded-time)
	    (format nil "~A:~2,,,'0@A:~2,,,'O@A" h m s)))

備註 94 (Notes 94)
==================================

In a letter of March 18 (old style) 1751, Chesterfield writes:

“It was notorious, that the Julian Calendar was erroneous, and had overcharged the solar year with eleven days. Pope Gregory the Thirteenth corrected this error [in 1582]; his reformed calendar was immediately received by all the Catholic powers of Europe, and afterwards adopted by all the Protestant ones, except Russia, Sweden, and England. It was not, in my opinion, very honourable for England to remain in a gross and avowed error, especially in such company; the inconveniency of it was likewise felt by all those who had foreign correspondences, whether political or mercantile. I determined, therefore, to attempt the reformation; I consulted the best lawyers, and the most skillful astronomers, and we cooked up a bill for that purpose. But then my difficulty began; I was to bring in this bill, which was necessarily composed of law jargon and astronomical calculations, to both of which I am an utter stranger. However, it was absolutely necessary to make the House of Lords think that I knew something of the matter; and also to make them believe that they knew something of it themselves, which they do not. For my own part, I could just as soon have talked Celtic or Sclavonian to them, as astronomy, and they would have understood me full as well; so I resolved to do better than speak to the purpose, and to please instead of informing them. I gave them, therefore, only an historical account of calendars, from the Egyptian down to the Gregorian, amusing them now and then with little episodes; but I was particularly attentive to the choice of my words, to the harmony and roundness of my periods, to my elocution, to my action. This succeeded, and ever will succeed; they thought I informed them, because I pleased them; and many of them said I had made the whole very clear to them; when, God knows, I had not even attempted it.”

See: Roberts, David (Ed.) `Lord Chesterfield's Letters <http://books.google.com.tw/books/about/Lord_Chesterfield_s_Letters.html?id=nFZP1WQ6XDoC&redir_esc=y>`_\ . Oxford University
Press, Oxford, 1992.

備註 95 (Notes 95)
==================================

In Common Lisp, a universal time is an integer representing the number of seconds since the beginning of 1900. The functions ``encode-universal-time`` and ``decode-universal-time`` translate dates into and out of this format. So for dates after 1900, there is a simpler way to do date arithmetic in Common Lisp:

::

	(defun num->date (n)
	  (multiple-value-bind (ig no re d m y)
	                       (decode-universal-time n)
	    (values d m y)))

	(defun date->num (d m y)
	  (encode-universal-time 1 0 0 d m y))

	(defun date+ (d m y n)
	  (num->date (+ (date->num d m y)
	                (* 60 60 24 n))))

Besides the range limit, this approach has the disadvantage that dates tend not to be fixnums.

備註 100 (Notes 100)
==================================

Although a call to ``setf`` can usually be understood as a reference to a particular place, the underlying machinery is more general. Suppose that a marble is a structure with a single field called color:

::

	(defstruct marble
	  color)

The following function takes a list of marbles and returns their color, if they all have the same color, or n i l if they have different colors:

::

	(defun uniform-color (1st)
	  (let ((c (marble-color (car 1st))))
	    (dolist (m (cdr 1st))
	      (unless (eql (marble-color m) c)
	        (return nil)))
	    c))

Although ``uniform-color`` does not refer to a particular place, it is both reasonable and possible to have a call to it as the first argument to ``setf`` . Having defined

::

	(defun (setf uniform-color) (val 1st)
	  (dolist (m 1st)
	    (setf (marble-color m) val)))

we can say

::

	(setf (uniform-color *marbles*) 'red)

to make the color of each element of ``*marbles*`` be red.

備註 100-2 (Notes 100-2)
==================================

In	older	Common	Lisp	implementations,	you	have to use ``defsetf`` to define how a call should be treated when it appears as the first argument to setf. Be careful when translating, because the parameter representing the new value comes last in the definition of a function whose name is given as the second argument to ``defsetf`` . That is, the call

::

	(defun (setf primo) (val 1st) (setf (car 1st) val))

is equivalent to

::

	(defsetf primo set-primo)

::

	(defun set-primo (1st val) (setf (car 1st) val))

備註 106 (Notes 106)
==================================

C, for example, lets you pass a pointer to a function, but there's less you can pass in a function (because C doesn't have closures) and less the recipient can do with it (because C has no equivalent of apply). What's more, you are in principle supposed to declare the type of the return value of the function you pass a pointer to. How, then, could you write ``map-int`` or ``filter`` , which work for functions that return anything? You couldn't, really. You would have to suppress the type-checking of arguments and return values, which is dangerous, and even so would probably only be practical for 32-bit values.

備註 109 (Notes 109)
==================================

For many examples of the versatility of closures, see: Abelson, Harold, and Gerald Jay Sussman, with Julie Sussman. `Structure and Interpretation of Computer Programs <http://mitpress.mit.edu/sicp/>`_\ . MIT Press, Cambridge, 1985.

備註 109-2 (Notes 109-2)
==================================

For more information about Dylan, see: Shalit, Andrew, with Kim Barrett, David Moon, Orca Starbuck, and Steve Strassmann. `Dylan Interim Reference Manual <http://jim.studt.net/dirm/interim-contents.html>`_\ . Apple Computer, 1994.

At the time of printing this document was accessible from several sites, including http://www.harlequin.com and http://www.apple.com. Scheme is a very small, clean dialect of Lisp. It was invented by Guy L. Steele Jr. and Gerald J. Sussman in 1975, and is currently defined by: Clinger, William, and Jonathan A. Rees (Eds.) :math:`Revised^4` Report on the Algorithmic Language Scheme. 1991.

This report, and various implementations of Scheme, were at the time of printing available by anonymous FTP from swiss-ftp.ai.mit.edu:pub\ .

There are two especially good textbooks that use Scheme—Structure and Interpretation (see preceding note) and: Springer, George and Daniel P. Friedman. `Scheme and the Art of Programming <http://www.amazon.com/Scheme-Art-Programming-George-Springer/dp/0262192888>`_\ . MIT Press, Cambridge, 1989.

備註 112 (Notes 112)
==================================

The most horrible Lisp bugs may be those involving dynamic scope. Such errors almost never occur in Common Lisp, which has lexical scope by default. But since so many of the Lisps used as extension languages still have dynamic scope, practicing Lisp programmers should be aware of its perils.

One bug that can arise with dynamic scope is similar in spirit to variable capture (page 166). You pass one function as an argument to another. The function passed as an argument refers to some variable. But within the function that calls it, the variable has a new and unexpected value.

Suppose, for example, that we wrote a restricted version of mapcar as follows:

::

	(defun our-mapcar (fn x)
	  (if (null x)
	      nil (cons (funcall fn (car x))
	                (our-mapcar fn (cdr x)))))

Then suppose that we used this function in another function, ``add-to-all`` , that would take a number and add it to every element of a list:

::

	(defun add-to-all (1st x)
	  (our-mapcar #'(lambda (num) (+ num x))
	              1st))

In Common Lisp this code works fine, but in a Lisp with dynamic scope it would generate an error. The function passed as an argument to ``our-mapcar`` refers to ``x`` . At the point where we send this function to ``our-mapcar`` , ``x`` would be the number given as the second argument to ``add-to-all`` . But where the function will be called, within ``our-mapcar`` , ``x`` would be something else: the list passed as the second argument to ``our-mapcar`` . We would get an error when this list was passed as the second argument to ``+`` .

備註 123 (Notes 123)
==================================

Newer implementations of Common Lisp include avariable ``*read-eval*`` that can be used to turn off the ``#`` . read-macro. When calling ``read-from-string`` on user input, it is wise to bind ``*read-eval*`` to ``nil`` . Otherwise the user could cause side-effects by using ``#`` . in the input.

備註 125 (Notes 125)
==================================

There are a number of ingenious algorithms for fast string-matching, but string-matching in text files is one of the cases where the brute-force approach is still reasonably fast. For more on string-matching algorithms, see: Sedgewick, Robert. `Algorithms <http://www.amazon.com/Algorithms-4th-Robert-Sedgewick/dp/032157351X/ref=sr_1_1?s=books&ie=UTF8&qid=1365042619&sr=1-1&keywords=algorithms+sedgewick>`_\ . Addison-Wesley, Reading (MA), 1988.

備註 141 (Notes 141)
==================================

In 1984 CommonLisp, reduce did not take a ``:key`` argument, so ``random-next`` would be defined:

::

	(defun random-next (prev)
	  (let* ((choices (gethash prev *words*))
	         (i (random (let ((x 0))
	                      (dolist (c choices)
	                        (incf x (cdr c)))
	                      x))))
	    (dolist (pair choices)
	      (if (minusp (decf i (cdr pair)))
	        (return (car pair))))))

備註 141-2 (Notes 141-2)
==================================

In 1989, a program like Henley was used to simulate netnews postings by well-known flamers. The fake postings fooled a significant number of readers. Like all good hoaxes, this one had an underlying point. What did it say about the content of the original flames, or the attention with which they were read, that randomly generated postings could be mistaken for the real thing?

One of the most valuable contributions of artificial intelligence research has been to teach us which tasks are really difficult. Some tasks turn out to be trivial, and some almost impossible. If artificial intelligence is concerned with the latter, the study of the former might be called artificial stupidity. A silly name, perhaps, but this field has real promise—it promises to yield programs that play a role like that of control experiments.

Speaking with the appearance of meaning is one of the tasks that turn out to be surprisingly easy. People's predisposition to find meaning is so strong that they tend to overshoot the mark. So if a speaker takes care to give his sentences a certain kind of superficial coherence, and his audience are sufficiently credulous, they will make sense of what he says.

This fact is probably as old as human history. But now we can give examples of genuinely random text for comparison. And if our randomly generated productions are difficult to distinguish from the real thing, might that not set people to thinking?

The program shown in Chapter 8 is about as simple as such a program could be, and that is already enough to generate "poetry" that many people (try it on your friends) will believe was written by a human being. With programs that work on the same principle as this one, but which model text as more than a simple stream of words, it will be possible to generate random text that has even more of the trappings of meaning.

For a discussion of randomly generated poetry as a legitimate literary form, see: Low, Jackson M. Poetry, Chance, Silence, Etc. In Hall, Donald (Ed.) Claims for Poetry. University of Michigan Press, Ann Arbor, 1982. You bet.

Thanks to the Online Book Initiative, ASCII versions of many classics are available online. At the time of printing, they could be obtained by anonymous FTP from ftp.std.com:obi.

See also the Emacs Dissociated Press feature, which uses an equivalent algorithm to scramble a buffer.

備註 150 (Notes 150)
==================================

下面這個函數會顯示在一個給定實現中，16 個用來標示浮點表示法的限制的全局常數：

::

	(defun float-limits ()
	  (dolist (m '(most least))
	    (dolist (s '(positive negative))
	      (dolist (f '(short single double long))
	        (let ((n (intern (string-upcase
	                            (format nil "~A-~A-~A-float"
	                                          m  s  f)))))
	          (format t "~30A ~A ~%" n (symbol-value n)))))))

備註 164 (Notes 164)
==================================

`快速排序演算法 <http://zh.wikipedia.org/zh-cn/%E5%BF%AB%E9%80%9F%E6%8E%92%E5%BA%8F>`_\ 由\ `霍爾 <http://zh.wikipedia.org/zh-cn/%E6%9D%B1%E5%B0%BC%C2%B7%E9%9C%8D%E7%88%BE>`_\ 於 1962 年發表，並被描述在 Knuth, D. E. *Sorting and Searching.* Addison-Wesley, Reading (MA), 1973.一書中。

備註 173 (Notes 173)
==================================

`Foderaro, John K.  Introduction to the Special Lisp Section. CACM 34:9 (Setember 1991), p.27 <http://www.informatik.uni-trier.de/~ley/db/journals/cacm/cacm34.html>`_

備註 176 (Notes 176)
===============================

關於 CLOS 更詳細的資訊，參考下列書目：

Keene, Sonya E. `Object Oriented Programming in Common Lisp <http://en.wikipedia.org/wiki/Object-Oriented_Programming_in_Common_Lisp:_A_Programmer's_Guide_to_CLOS>`_ , Addison-Wesley, Reading (MA), 1989

Kiczales, Gregor, Jim des Rivieres, and Daniel G. Bobrow. `The Art of the Metaobject Protocol <http://en.wikipedia.org/wiki/The_Art_of_the_Metaobject_Protocol>`_ MIT Press, Cambridge, 1991

備註 178 (Notes 178)
==============================

讓我們再回放剛剛的句子一次：\ *我們甚至不需要看程式中其他的程式碼一眼，就可以完成種種的改動。*\ 這個想法或許對某些讀者聽起來擔憂地熟悉。這是寫出\ `麵條式程式碼 <http://zh.wikipedia.org/wiki/%E9%9D%A2%E6%9D%A1%E5%BC%8F%E4%BB%A3%E7%A0%81>`_\ 的食譜。

物件導向模型使得通過一點一點的來構造程式變得簡單。但這通常意味著，在實踐上它提供了一種有結構的方法來寫出麵條式程式碼。這不一定是壞事，但也不會是好事。

很多現實世界中的程式碼是麵條式程式碼，這也許不能很快改變。針對那些終將成爲麵條式程式碼的程式來說，物件導向模型是好的：它們最起碼會是有結構的麵條。但針對那些也許可以避免誤入崎途的程式來說，面向物件抽象只是更加危險的，而不是有用的。

備註 183 (Notes 183)
==================================

When an instance would inherit a slot with the same name from several of its superclasses, the instance inherits a single slot that combines the properties of the slots in the superclasses. The way combination is done varies from property to property:

1. The ``:allocation`` , ``:initform`` (if any), and ``:documentation`` (if any), will be those of the most specific classes.

2. The ``:initargs`` will be the union of the ``:initargs`` of all the superclasses. So will the ``:accessors`` , ``:readers`` , and ``:writers`` , effectively.

3. The ``:type`` will be the intersection of the ``:types`` of all the superclasses.

備註 191 (Notes 191)
==================================

You can avoid explicitly uninterning the names of slots that you want to be encapsulated by using uninterned symbols as the names to start with:

::

	(progn
	  (defclass counter () ((#1=#:state :initform 0)))

	  (defmethod increment ((c counter))
	    (incf (slot-value c '#1#)))

	  (defmethod clear ((c counter))
	    (setf (slot-value c '#1#) 0)))

The ``progn`` here is a no-op; it is used to ensure that all the references to the uninterned symbol occur within the same expression. If this were inconvenient, you could use the following read-macro instead:

::

	(defvar *symtab* (make-hash-table :test #'equal))

	(defun pseudo-intern (name)
	  (or (gethash name *symtab*)
	      (setf (gethash name *symtab*) (gensym))))

	(set-dispatch-macro-character #\# #\[
	  #'(lambda (stream char1 char2)
	      (do ((acc nil (cons char acc))
	           (char (read-char stream) (read-char stream)))
	          ((eql char #\]) (pseudo-intern acc)))))

Then it would be possible to say just:

::

	(defclass counter () ((#[state] :initform 0)))

	(defmethod increment ((c counter))
	  (incf (slot-value c '#[state])))

	(defmethod clear ((c counter))
	  (setf (slot-value c '#[state]) 0))


備註 204 (Notes 204)
==================================

下面這個宏將新元素推入二元搜索樹：

::

	(defmacro bst-push (obj bst <)
	  (multiple-value-bind (vars forms var set access)
	                       (get-setf-expansion bst)
	    (let ((g (gensym)))
	      `(let* ((,g ,obj)
	              ,@(mapcar #'list vars forms)
	              (,(car var) (bst-insert! ,g ,access ,<)))
	         ,set))))

備註 213 (Notes 213)
==================================

Knuth, Donald E. `Structured Programming with goto Statements. <http://sbel.wisc.edu/Courses/ME964/Literature/knuthProgramming1974.pdf>`_ *Computing Surveys* , 6:4 (December 1974), pp. 261-301

備註 214 (Notes 214)
==================================

Knuth, Donald E. `Computer Programming as an Art <http://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&cad=rja&ved=0CC4QFjAB&url=http%3A%2F%2Fawards.acm.org%2Fimages%2Fawards%2F140%2Farticles%2F7143252.pdf&ei=vl9VUIWBIOWAmQWQu4FY&usg=AFQjCNHAgYS4PiHA0OfgOdiDfPU2i6HAmw&sig2=zZalr-ife4DB4BR2CPORBQ>`_ *In ACM Turing Award Lectures: The First Twenty Years.* ACM Press, 1987

This paper and the preceding one are reprinted in: Knuth, Donald E. Literate Programming. CSLI Lecture Notes #27, Stanford University Center for the Study of Language and Information, Palo Alto, 1992.

備註 216 (Notes 216)
==================================

Steele, Guy L., Jr. Debunking the “Expensive Procedure Call” Myth or, Procedural Call Implementations Considered Harmful or, LAMBDA: The Ultimate GOTO. Proceedings of the National Conference of the ACM, 1977, p. 157.

Tail-recursion optimization should mean that the compiler will generate the same code for a tail-recursive function as it would for the equivalent ``do``\ . The unfortunate reality, at least at the time of printing, is that many compilers generate slightly faster code for ``do``\ s.

備註 217 (Notes 217)
==================================

For some examples of calls to disassemble on various processors, see: Norvig, Peter. Paradigms ofArtificial Intelligence Programming: Case Studies in Common Lisp. Morgan Kaufmann, San Mateo (CA), 1992.

備註 218 (Notes 218)
==================================

A lot of the increased popularity of object-oriented programming is more specifically the increased popularity of C++, and this in turn has a lot to do with typing. C++ gives you something that seems like a miracle in the conceptual world of C: the ability to define operators that work for different types of arguments. But you don't need an object-oriented language to do this—all you need is run-time typing. And indeed, if you look at the way people use C++, the class hierarchies tend to be flat. C++ has become so popular not because people need to write programs in terms of classes and methods, but because people need a way around the restrictions imposed by C's approach to typing.

備註 219 (Notes 219)
==================================

Macros can make declarations easier. The following macro expects a type name and an expression (probably numeric), and expands the expression so that all arguments, and all intermediate results, are declared to be of that type. If you wanted to ensure that an expression e was evaluated using only fixnum arithmetic, you could say ``(with-type	fixnum e)`` .

::

	(defmacro with-type (type expr)
	  `(the ,type ,(if (atom expr)
			   expr
			 (expand-call type (binarize expr)))))

	(defun expand-call (type expr)
	  `(,(car expr) ,@(mapcar #'(lambda (a)
				      `(with-type ,type ,a))
				  (cdr expr))))

	(defun binarize (expr)
	  (if (and (nthcdr 3 expr)
		   (member (car expr) '(+ - * /)))
	      (destructuring-bind (op a1 a2 . rest) expr
		(binarize `(,op (,op ,a1 ,a2) ,@rest)))
	    expr))

The call to binarize ensures that no arithmetic operator is called with more than two arguments. As the Lucid reference manual points out, a call like

::

	(the fixnum (+ (the fixnum a)
	               (the fixnum b)
	               (the fixnum c)))

still cannot be compiled into fixnum additions, because the intermediate results (e.g. a + b) might not be fixnums.

Using ``with-type`` , we could duplicate the fully declared version of ``poly`` on page 219 with:

::

	(defun poly (a b x)
	  (with-type fixnum (+ (* a (expt x 2)) (* b x))))

If you wanted to do a lot of fixnum arithmetic, you might even want to define a read-macro that would expand into a ``(with-type fixnum ...)`` .

備註 224 (Notes 224)
==================================

在許多 Unix 系統裡， ``/usr/dict/words`` 是個合適的單詞檔案。

備註 226 (Notes 229)
==================================

T is a dialect of Scheme with many useful additions, including support for pools. For more on T, see: Rees, Jonathan A., Norman I. Adams, and James R. Meehan. The T Manual, 5th Edition. Yale University Computer Science Department, New Haven, 1988.

The T manual, and T itself, were at the time of printing available by anonymous FTP from hing.lcs.mit.edu:pub/t3.1 .

備註 229 (Notes 229)
==================================

The difference between specifications and programs is a difference in degree, not a difference in kind. Once we realize this, it seems strange to require that one write specifications for a program before beginning to implement it. If the program has to be written in a low-level language, then it would be reasonable to require that it be described in high-level terms first. But as the programming language becomes more abstract, the need for specifications begins to evaporate. Or rather, the implementation and the specifications can become the same thing.

If the high-level program is going to be re-implemented in a lower-level language, it starts to look even more like specifications. What Section 13.7 is saying, in other words, is that the specifications for C programs could be written in Lisp.

備註 230 (Notes 230)
==================================

Benvenuto Cellini's story of the casting of his Perseus is probably the most famous (and the funniest) account of traditional bronze-casting: Cellini, Benvenuto. Autobiography. Translated by George Bull, Penguin Books, Harmondsworth, 1956.

備註 239 (Notes 239)
==================================

Even experienced Lisp hackers find packages confusing. Is it because packages are gross, or because we are not used to thinking about what happens at read-time?

There is a similar kind of uncertainty about def macro, and there it does seem that the difficulty is in the mind of the beholder. A good deal of work has gone into finding a more abstract alternative to def macro. But def macro is only gross if you approach it with the preconception (common enough) that defining a macro is like defining a function. Then it seems shocking that you suddenly have to worry about variable capture. When you think of macros as what they are, transformations on source code, then dealing with variable capture is no more of a problem than dealing with division by zero at run-time.

So perhaps packages will turn out to be a reasonable way of providing modularity. It is prima facie evidence on their side that they resemble the techniques that programmers naturally use in the absence of a formal module system.

備註 242 (Notes 242)
==================================

It might be argued that ``loop`` is more general, and that we should not define many operators to do what we can do with one. But it's only in a very legalistic sense that loop is one operator. In that sense, ``eval`` is one operator too. Judged by the conceptual burden it places on the user, ``loop`` is at least as many operators as it has clauses. What's more, these operators are not available separately, like real Lisp operators: you can't break off a piece of loop and pass it as an argument to another function, as you could ``map-int`` .

備註 248 (Notes 248)
==================================

關於更深入講述邏輯推論的資料，參見：\ `Stuart Russell <http://www.cs.berkeley.edu/~russell/>`_ 及 `Peter Norvig <http://www.norvig.com/>`_ 所著的 `Artificial Intelligence: A Modern Approach <http://aima.cs.berkeley.edu/>`_\ 。

備註 273 (Notes 273)
==================================

Because the program in Chapter 17 takes advantage of the possibility of having a ``setf`` form as the first argument to ``defun`` , it will only work in more recent Common Lisp implementations. If you want to use it in an older implementation, substitute the following code in the final version:

::

	(proclaim '(inline lookup set-lookup))

	(defsetf lookup set-lookup)

	(defun set-lookup (prop obj val)
	  (let ((off (position prop (layout obj) :test #'eq)))
	    (if off
	        (setf (svref obj (+ off 3)) val)
	        (error "Can't set ~A of ~A." val obj))))

	(defmacro defprop (name &optioanl meth?)
	  `(progn
	     (defun ,name (obj &rest args)
	       ,(if meth?
	          `(run-methods obj ',name args)
	          `(rget ',name obj nil)))
	     (defsetf ,name (obj) (val)
	       `(setf (lookip ',',name ,obj) ,val))))


備註 276 (Notes 276)
==================================

If ``defmeth`` were defined as

::

	(defmacro defmeth (name obj parms &rest body)
	  (let ((gobj (gensym)))
	    `(let ((,gobj ,obj))
	       (setf (gethash ',name ,gobj)
	             #'(lambda ,parms
	                 (labels ((next ()
	                            (funcall (get-next ,gobj ',name)
	                                     ,@parms)))
	                   ,@body))))))

then it would be possible to invoke the next method simply by calling ``next`` :

::

	(defmeth area grumpy-circle (c)
	  (format t "How dare you stereotype me!""/,")
	  (next))

備註 284 (Notes 284)
==================================

For really fast access to slots we would use the following macro:

::

	(defmacro with-slotref ((name prop class) &rest body)
	  (let ((g (gensym)))
	    `(let ((,g (+ 3 (position ,prop (layout ,class)
	                              :test #'eq))))
	       (macrolet ((,name (obj) `(svref ,obj ,',g)))
	         ,@body))))

It defines a local macro that refers directly to the vector element corresponding to a slot. If in some segment of code you wanted to refer to the same slot in many instances of the same class, with this macro the slot references would be straight ``svref``\ s.

For example, if the balloon class is defined as follows,

::

	(setf balloon-class (class nil size))

then this function pops (in the old sense) a list of ballons:

::

	(defun popem (ballons)
	  (with-slotref (bsize 'size balloon-class)
	    (dolist (b ballons)
	      (setf (bsize b) 0))))

備註 284-2 (Notes 284-2)
==================================

Gabriel, Richard P. `Lisp Good News, Bad News, How to Win Big <http://www.dreamsongs.com/Files/LispGoodNewsBadNews.pdf>`_ *AI Expert*\ , June 1991, p.35.

早在 1973 年， `Richard Fateman <http://en.wikipedia.org/wiki/Richard_Fateman>`_ 已經能證明在 `PDP-10 <http://en.wikipedia.org/wiki/PDP-10>`_ 主機上， `MacLisp <http://en.wikipedia.org/wiki/Maclisp>`_ 編譯器比製造商的 FORTRAN 編譯器，產生出更快速的程式碼。

**譯註:** `該篇 MacLisp 編譯器在 PDP-10 可產生比 Fortran 快的程式碼的論文在這可以找到 <http://dl.acm.org/citation.cfm?doid=1086803.1086804>`_

備註 399 (Notes 399)
==================================

It's easiest to understand backquote if we suppose that backquote and comma are like quote,	and	that ```,x``	simply expands	into ``(bq (comma x))`` .	If this were so, we could handle backquote by augmenting ``eval`` as in this sketch:

::

	(defun eval2 (expr)
	  (case (and (consp expr) (car expr))
	    (comma (error "unmatched comma"))
	    (bq	(eval-bq (second expr) 1))
	    (t	(eval expr))))

	(defun eval-bq (expr n)
	  (cond ((atom expr)
	         expr)
	        ((eql (car expr) 'comma)
	         (if (= n 1)
	             (eval2 (second expr))
	             (list 'comma (eval-bq (second expr)
	                                   (1- n)))))
	        ((eql (car expr) 'bq)
	         (list 'bq (eval-bq (second expr) (1+ n))))
	        (t
	         (cons (eval-bq (car expr) n)
	               (eval-bq (cdr expr) n)))))

In ``eval-bq`` , the parameter ``n`` is used to determine which commas match the current backquote. Each backquote increments it, and each comma decrements it. A comma encountered when n = 1 is a matching comma. Here is the example from page 400:

::

	> (setf x 'a a 1 y 'b b 2)
	2
	> (eval2 '(bq (bq (w (comma x) (comma (comma y))))))
	(BQ (W (COMMA X) (COMMA B)))
	> (eval2 *)
	(W A 2)

At some point a particularly remarkable molecule was formed by accident. We will call it the Replicator. It may not necessarily have been the biggest or the most complex molecule around, but it had the extraordinary property of being able to create copies of itself.

Richard Dawkins

The Selfish Gene


We shall first define a class of symbolic expressions in terms of ordered pairs and lists. Then we shall define five elementary functions and predicates, and build from them by composition, conditional expressions, and recursive definitions an extensive class of functions of which we shall give a number of examples. We shall then show how these functions themselves can be expressed as symbolic expressions, and we shall define a universal function apply that allows us to compute from the expression for a given function its value for given arguments.


John McCarthy

Recursive Functions of Symbolic Expressions and their Computation by Machine, Part I