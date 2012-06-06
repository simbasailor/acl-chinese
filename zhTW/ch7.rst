.. highlight:: cl
   :linenothreshold: 0

Chapter 7 輸入與輸出 (Input and Output)
***************************************************

Common Lisp 有著威力強大的 I/O 工具。關於輸入以及一些普遍的讀取字元的函數，我們有 ``read`` 並包含了一個完整的解析器 (parser)。關於輸出以及一些普遍的寫出字元的函數，我們有 ``format`` ，它自身幾乎就是一個語言了。本章介紹了所有基本的概念。

在 Common Lisp 有兩種流 (streams)，字元流與二進制流。本章描述了字元流的操作；二進制流的操作涵蓋在 14.2 節。

7.1 流 (Streams)
==================================

流是表示字元來源或終點的 Lisp 物件。要讀取或寫入檔案，你將其作為流打開。但流與檔案是不一樣的。你可以在頂層讀入或印出時使用一個流。你甚至可以創造可以讀取或寫入字串的流。

輸入預設是從 ``*standard-input*`` 流讀取。輸出預設是在 ``*standard-output*`` 。最初它們在相同的地方：一個表示頂層的流。

我們已經在頂層看過 ``read`` 與 ``format`` 是如何讀取及印出的。前者接受一個應是流的選擇性參數，預設為 ``*standard-input*`` 。 ``format`` 的第一個參數可以是一個流，但當它是 ``t`` 時，輸出被送到 ``*standard-output*`` 。所以我們目前為止都只用到預設的流而已。我們可以在任何流上面做同樣的 I/O 操作。

一個路徑名 (pathname) 是一種指定一個檔案的可攜方式 (portable way)。一個路徑名有六個部分：host, device, directory, name, type 及 version 。你可以透過呼叫 ``make-pathname`` 搭配一個或多個對應的關鍵字參數來產生一個路徑。在最簡單的情況下，你可以只指明名字，讓其他的部分設為預設：

::

  > (setf path (make-pathname :name "myfile"))
	#P"myfile"

開啟一個檔案預設的基本函數是 ``open`` 。它接受一個路徑名 [1]_ 以及大量的選擇性關鍵字參數，而要是開啟成功時，回傳一個指向檔案的流。

你可以在創造流時，指定你想要怎麼使用它。 無論你是要寫入流、從流讀取或是兩者皆是， ``:direction`` 參數會發信號表示。三個對應的數值是 ``:input`` , ``output`` , ``:io`` 。如果是用來輸出的流， ``if-exists`` 參數說明了如果檔案已經存在時該怎麼做；通常它應該是 ``:supercede`` 。所以要創造一個可以寫入 "myfile" 檔案的流，你可以：

::

  > (setf str (open path :direction :output
                         :if-exists :supercede))
  #<Stream C017E6>

流的印出表示法 (printed-representation) 因實現而異。

現在我們可以把這個流當成 ``format`` 的第一個參數傳入，它會在流印出，而不是頂層：

::

	> (format str "Something~%")
	NIL

如果我們在此時檢視這個檔案，輸出也許會、也許不會在那裡。某些實現會將輸出儲存成一塊 (chunks)再寫出。它也許不會出現，直到我們將流關閉：

::

	> (close str)
	NIL

當你使用完時，總是記得關閉檔案；在你還沒關閉之前，內容是不保證會出現的。現在如果我們檢視檔案 "myfile" ，應該有一行：

``Something``

如果我們只想從檔案讀取，我們可以開啟一個流搭配 ``:direction :input`` ：

::

	> (setf str (open path :direction :input))
	#<Stream C01C86>

我們可以對檔案使用任何輸入函數。7.2 節會更詳細的描述輸入部分。下面是一個我們將使用 ``read-line`` 來從檔案讀取一行文字的範例：

::

	> (read-line str)
	"Something"
	> (close str)
	NIL

當你讀取完畢時，記得關閉檔案。

大部分時間我們不使用 ``open`` 與 ``close`` 來操作檔案的輸入輸出。 ``with-open-file`` 巨集通常更方便。它的第一個參數應該是一個列表，包含了變數名以及伴隨你想傳給 ``open`` 的參數。在這之後它接受一個程式碼主體，它會與隨後傳給 ``open`` 參數所創造的流一起被求值。然後這個流會被自動關閉。所以整個檔案寫入動作可以表示為：

::

  (with-open-file (str path :direction :output
                            :if-exists :supercede)
    (format str "Something~%"))

``with-open-file`` 巨集將 ``close`` 放在 ``unwind-protect`` 裡 (參見 92 頁)，即使一個錯誤打斷了主體的求值，檔案是保證會被關閉的。

譯註: 92 頁是 5.6 小節。

7.2 輸入 (Input)
===============================

7.3 輸出 (Output)
================================

7.4 範例：字串代換 (Example: String Substitution)
===================================================

7.5 巨集字元 (Macro Characters)
=======================================

Chapter 7 總結 (Summary)
============================

Chapter 7 練習 (Exercises)
==================================


.. rubric:: 腳註

.. [1] 你可以給一個字串取代路徑名，但這樣就不可攜了 (portable)。
