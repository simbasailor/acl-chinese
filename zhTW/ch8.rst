.. highlight:: cl


第八章：符號
***************************************************

我們一直在使用符號。在符號看似簡單的表面之下，又好像沒有那麼簡單。起初最好不要糾結於背後的實現機制。可以把符號當成資料物件與名字那樣使用，而不需要理解兩者是如何關聯起來的。但到了某個時間點，停下來思考背後是究竟是如何工作會是很有用的。本章解釋了背後實現的細節。

8.1 符號名 (Symbol Names)
==================================

第二章描述過，符號是變數的名字，符號本身以物件所存在。但 Lisp 符號的可能性，要比在多數語言僅允許作爲變數名來得廣泛許多。實際上，符號可以用任何字串當作名字。可以通過呼叫 ``symbol-name`` 來獲得符號的名字：

::

	> (symbol-name 'abc)
	"ABC"

注意到這個符號的名字，打印出來都是大寫字母。預設情況下， Common Lisp 在讀入時，會把符號名字所有的英文字母都轉成大寫。代表 Common Lisp 預設是不分大小寫的：

::

	> (eql 'abc 'Abc)
	T
	> (CaR '(a b c))
	A

一個名字包含空白，或其它可能被讀取器認爲是重要的字元的符號，要用特殊的語法來引用。任何存在垂直槓 (vertical bar)之間的字元序列將被視爲符號。可以如下這般在符號的名字中，放入任何字元：

::

	> (list '|Lisp 1.5| '|| '|abc| '|ABC|)
	(|Lisp 1.5| || |abc| ABC)

當這種符號被讀入時，不會有大小寫轉換，而宏字元與其他的字元被視爲一般字元。

那什麼樣的符號不需要使用垂直槓來參照呢？基本上任何不是數字，或不包含讀取器視爲重要的字元的符號。一個快速找出你是否可以不用垂直槓來引用符號的方法，是看看 Lisp 如何印出它的。如果 Lisp 沒有用垂直槓表示一個符號，如上述列表的最後一個，那麼你也可以不用垂直槓。

記得，垂直槓是一種表示符號的特殊語法。它們不是符號的名字之一：

::

	> (symbol-name '|a b c|)
	"a b c"

(如果想要在符號名稱內使用垂直槓，可以放一個反斜線在垂直槓的前面。)

譯註: 反斜線是 ``\`` (backslash)。


8.2 屬性列表 (Property Lists)
===============================

在 Common Lisp 裡，每個符號都有一個屬性列表（property-list）或稱爲 ``plist`` 。函數 ``get`` 接受符號及任何型別的鍵值，然後返回在符號的屬性列表中，與鍵值相關的數值：

::

	> (get 'alizarin 'color)
	NIL

它使用 ``eql`` 來比較各個鍵。若某個特定的屬性沒有找到時， ``get`` 返回 ``nil`` 。

要將值與鍵關聯起來時，你可以使用 ``setf`` 及 ``get`` :

::

	> (setf (get 'alizarin 'color) 'red)
	RED
	> (get 'alizarin 'color)
	RED

現在符號 ``alizarin`` 的 ``color`` 屬性是 ``red`` 。

.. figure:: ../images/Figure-8.1.png

**圖 8.1 符號的結構**

::

	> (setf (get 'alizarin 'transparency) 'high)
	HIGH
	> (symbol-plist 'alizarin)
	(TRANSPARENCY HIGH COLOR RED)

注意，屬性列表不以關聯列表（assoc-lists）的形式表示，雖然用起來感覺是一樣的。

在 Common Lisp 裡，屬性列表用得不多。他們大部分被雜湊表取代了（4.8 小節）。

8.3 符號很不簡單 (Symbols Are Big)
=====================================

當我們輸入名字時，符號就被悄悄地創建出來了，而當它們被顯示時，我們只看的到符號的名字。某些情況下，把符號想成是表面所見的東西就好，別想太多。但有時候符號不像看起來那麼簡單。

從我們如何使用以及檢視符號，符號看起來像是整數那樣的小物件。而符號實際上確實是一個物件，差不多像是由 ``defstruct`` 定義的那種結構。符號可以有名字、 主包（home package）、作爲變數的值、作爲函數的值以及帶有一個屬性列表。圖 8.1 示範了符號在內部是如何表示的。

很少有程式會使用很多符號，以致於值得用其它的東西來代替符號以節省空間。但需要記住的是，符號是實際的物件，不僅是名字而已。當兩個變數設成相同的符號時，與兩個變數設成相同列表一樣：兩個變數的指標都指向同樣的物件。

8.4 創建符號 (Creating Symbols)
===================================================

8.1 節示範了如何取得符號的名字。另一方面，用字串生成符號也是有可能的。但比較複雜一點，因爲我們需要先介紹包（package）。

概念上來說，包是將名字映射到符號的符號表（symbol-tables）。每個普通的符號都屬於一個特定的包。符號屬於某個包，我們稱爲符號被包扣押（intern）了。函數與變數用符號作爲名稱。包藉由限制哪個符號可以存取來實現模組性（modularity），也是因爲這樣，我們才可以引用到函數與變數。

大多數的符號在讀取時就被扣押了。在第一次輸入一個新符號的名字時，Lisp 會產生一個新的符號物件，並將它扣押到當下的包裡（預設是 ``common-lisp-user`` 包)。但也可以通過給入字串與選擇性包參數給 ``intern`` 函數，來扣押一個名稱爲字串名的符號:

::

	> (intern "RANDOM-SYMBOL")
	RANDOM-SYMBOL
	NIL

選擇性包參數預設是當前的包，所以前述的表達式，返回當前包裡的一個符號，此符號的名字是 “RANDOM-SYMBOL”，若此符號尚未存在時，會創建一個這樣的符號出來。第二個返回值告訴我們符號是否存在；在這個情況，它不存在。

不是所有的符號都會被扣押。有時候有一個自由的（uninterned）符號是有用的，這和公用電話本是一樣的原因。自由的符號叫做 *gensyms* 。我們將會在第 10 章討論宏（Macro）時，理解 ``gensym`` 的作用。

8.5 多重包 (Multiple Packages)
=======================================

大的程式通常切分爲多個包。如果程式的每個部分都是一個包，那麼開發程式另一個部分的某個人，將可以使用符號來作爲函數名或變數名，而不必擔心名字在別的地方已經被用過了。

在沒有提供定義多個命名空間的語言裡，工作於大項目的程式設計師，通常需要想出某些規範（convention），來確保他們不會使用同樣的名稱。舉例來說，程式設計師寫顯示相關的程式（display code）可能用 ``disp_`` 開頭的名字，而寫數學相關的程式（math code）的程式設計師僅使用由 ``math_`` 開始的程式。所以若是數學相關的程式裡，包含一個做快速傅立葉轉換的函數時，可能會叫做 ``math_fft`` 。

包不過是提供了一種方便的方式來自動辦到此事。如果你將函數定義在單獨的包裡，可以隨意使用你喜歡的名字。只有你明確導出（ ``export`` ）的符號會被別的包看到，而通常前面會有包的名字(或修飾符)。

舉例來說，假設一個程式分爲兩個包， ``math`` 與 ``disp`` 。如果符號 ``fft`` 被 ``math`` 包導出，則 ``disp`` 包裡可以用 ``math:fft`` 來參照它。在 ``math`` 包裡，可以只用 ``fft`` 來參照。

下面是你可能會放在檔案最上方，包含獨立包的程式：

::

	(defpackage "MY-APPLICATION"
	            (:use "COMMON-LISP" "MY-UTILITIES")
	            (:nicknames "APP")
	            (:export "WIN" "LOSE" "DRAW"))

	(in-package my-application)

``defpackage`` 定義一個新的包叫做 ``my-application`` [1]_ 它使用了其他兩個包， ``common-lisp`` 與 ``my-utilities`` ，這代表著可以不需要用包修飾符（package qualifiers）來存取這些包所導出的符號。許多包都使用了 ``common-lisp`` 包 ── 因爲你不會想給 Lisp 自帶的運算子與變數再加上修飾符。

``my-application`` 包本身只輸出三個符號: ``WIN`` 、 ``LOSE`` 以及 ``DRAW`` 。由於呼叫 ``defpackage`` 給了 ``my-application`` 一個匿稱 ``app`` ，則別的包可以這樣引用到這些符號，比如 ``app:win`` 。

``defpackage`` 伴隨著一個 ``in-package`` ，確保當前包是 ``my-application`` 。所有其它未修飾的符號會被扣押至 ``my-application`` ── 除非之後有別的 ``in-package`` 出現。當一個檔案被載入時，當前的包總是被重置成載入之前的值。

8.6 關鍵字 (Keywords)
=======================================

在 ``keyword`` 包的符號 (稱爲關鍵字)有兩個獨特的性質：它們總是對自己求值，以及可以在任何地方引用它們，如 ``:x`` 而不是 ``keyword:x`` 。我們首次在 44 頁 (譯註: 3.10 小節）介紹關鍵字參數時， ``(member '(a) '((a) (z)) test: #'equal)`` 比 ``(member '(a) '((a) (z)) :test #'equal)`` 讀起來更自然。現在我們知道爲什麼第二個較彆扭的形式才是對的。 ``test`` 前的冒號字首，是關鍵字的識別符。

爲什麼使用關鍵字而不用一般的符號？因爲關鍵字在哪都可以存取。一個函數接受符號作爲實參，應該要寫成預期關鍵字的函數。舉例來說，這個函數可以安全地在任何包裡呼叫:

::

	(defun noise (animal)
	  (case animal
	    (:dog :woof)
	    (:cat :meow)
	    (:pig :oink)))

但如果是用一般符號寫成的話，它只在被定義的包內正常工作，除非關鍵字也被導出了。

8.7 符號與變數 (Symbols and Variables)
=======================================

Lisp 有一件可能會使你困惑的事情是，符號與變數的從兩個非常不同的層面互相關聯。當符號是特別變數（special variable）的名字時，變數的值存在符號的 value 欄位（圖 8.1）。 ``symbol-value`` 函數引用到那個欄位，所以在符號與特殊變數的值之間，有直接的連接關係。

而對於詞法變數（lexical variables）來說，事情就完全不一樣了。一個作爲詞法變數的符號只不過是個佔位符（placeholder）。編譯器會將其轉爲一個寄存器（register）或記憶體位置的引用位址。在最後編譯出來的

在程式裡，我們無法追蹤這個符號 (除非它被保存在除錯器「debugger」的某個地方)。因此符號與詞法變數的值之間是沒有連接的；只要一有值，符號就消失了。

8.8 範例：隨機文字 (Example: Random Text)
==============================================

如果你要寫一個處理單詞的程式，通常使用符號會比字串來得好，因爲符號概念上是原子性的（atomic）。符號可以用 ``eql`` 一步比較完成，而字串需要使用 ``string=`` 或 ``string-equal`` 逐一字元做比較。作爲一個範例，本節將示範如何寫一個程式來產生隨機文字。程式的第一部分會讀入一個範例檔案（越大越好），用來累積之後所給入的相關單詞的可能性（likeilhood）的資訊。第二部分在每一個單詞都根據原本的範例，產生一個隨機的權重（weight）之後，隨機走訪根據第一部分所產生的網路。

產生的文字將會是部分可信的（locally plausible），因爲任兩個出現的單詞也是輸入檔案裡，兩個同時出現的單詞。令人驚訝的是，獲得看起來是 ── 有意義的整句 ── 甚至整個段落是的頻率相當高。

圖 8.2 包含了程式的上半部，用來讀取範例檔案的程式。

::

	(defparameter *words* (make-hash-table :size 10000))

	(defconstant maxword 100)

	(defun read-text (pathname)
	  (with-open-file (s pathname :direction :input)
	    (let ((buffer (make-string maxword))
	          (pos 0))
	      (do ((c (read-char s nil :eof)
	              (read-char s nil :eof)))
	          ((eql c :eof))
	        (if (or (alpha-char-p c) (char= c #\'))
	            (progn
	              (setf (aref buffer pos) c)
	              (incf pos))
	            (progn
	              (unless (zerop pos)
	                (see (intern (string-downcase
	                               (subseq buffer 0 pos))))
	                (setf pos 0))
	              (let ((p (punc c)))
	                (if p (see p)))))))))

	(defun punc (c)
	  (case c
	    (#\. '|.|) (#\, '|,|) (#\; '|;|)
	    (#\! '|!|) (#\? '|?|) ))

	(let ((prev `|.|))
	  (defun see (symb)
	    (let ((pair (assoc symb (gethash prev *words*))))
	      (if (null pair)
	          (push (cons symb 1) (gethash prev *words*))
	          (incf (cdr pair))))
	    (setf prev symb)))

**圖 8.2 讀取範例檔案**

從圖 8.2 所導出的資料，會被存在雜湊表 ``*words*`` 裡。這個雜湊表的鍵是代表單詞的符號，而值會像是下列的關聯列表（assoc-lists）:

::

	((|sin| . 1) (|wide| . 2) (|sights| . 1))

使用\ `彌爾頓的失樂園 <http://zh.wikipedia.org/wiki/%E5%A4%B1%E6%A8%82%E5%9C%92>`_\ 作爲範例檔案時，這是與鍵 ``|discover|`` 有關的值。它指出了 “discover” 這個單詞，在詩裡面用了四次，與 “wide” 用了兩次，而 “sin” 與 ”sights” 各一次。(譯註: 詩可以在這裡找到 http://www.paradiselost.org/ )

函數 ``read-text`` 累積了這個資訊。這個函數接受一個路徑名（pathname），然後替每一個出現在檔案中的單詞，生成一個上面所展示的關聯列表。它的工作方式是，逐字讀取檔案的每個字元，將累積的單詞存在字串 ``buffer`` 。 ``maxword`` 設成 ``100`` ，程式可以讀取至多 100 個單詞，對英語來說足夠了。

只要下個字元是一個字（由 ``alpha-char-p`` 決定）或是一撇(
apostrophe)
，就持續累積字元。任何使單詞停止累積的字元會送給 ``see`` 。數種標點符號（punctuation）也被視爲是單詞；函數 ``punc`` 返回標點字元的僞單詞（pseudo-word）。

函數 ``see`` 註冊每一個我們看過的單詞。它需要知道前一個單詞，以及我們剛確認過的單詞 ── 這也是爲什麼要有變數 ``prev`` 存在。起初這個變數設爲僞單詞裡的句點；在 ``see`` 函數被呼叫後， ``prev`` 變數包含了我們最後見過的單詞。

在 ``read-text`` 返回之後， ``*words*`` 會包含輸入檔案的每一個單詞的條目（entry）。通過呼叫 ``hash-table-count`` 你可以了解有多少個不同的單詞存在。鮮少有英文檔案會超過 10000 個單詞。

現在來到了有趣的部份。圖 8.3 包含了從圖 8.2 所累積的資料來產生文字的程式。 ``generate-text`` 函數導出整個過程。它接受一個要產生幾個單詞的數字，以及選擇性傳入前一個單詞。使用預設值，會讓產生出來的檔案從句子的開頭開始。

::

	(defun generate-text (n &optional (prev '|.|))
	  (if (zerop n)
	      (terpri)
	      (let ((next (random-next prev)))
	        (format t "~A " next)
	        (generate-text (1- n) next))))

	(defun random-next (prev)
	  (let* ((choices (gethash prev *words*))
	         (i (random (reduce #'+ choices
	                            :key #'cdr))))
	    (dolist (pair choices)
	      (if (minusp (decf i (cdr pair)))
	          (return (car pair))))))

**圖 8.3 產生文字**

要取得一個新的單詞， ``generate-text`` 使用前一個單詞，接著呼叫 ``random-next`` 。 ``random-next`` 函數根據每個單詞出現的機率加上權重，隨機選擇伴隨輸入文字中 ``prev`` 之後的單詞。

現在會是測試運行下程式的好時機。但其實你早看過一個它所產生的範例： 就是本書開頭的那首詩，是使用彌爾頓的失樂園作爲輸入檔案所產生的。

(譯註: 詩可在這裡看，或是瀏覽書的第 vi 頁)

Half lost on my firmness gains more glad heart,

Or violent and from forage drives

A glimmering of all sun new begun

Both harp thy discourse they match'd,

Forth my early, is not without delay;

For their soft with whirlwind; and balm.

Undoubtedly he scornful turn'd round ninefold,

Though doubled now what redounds,

And chains these a lower world devote, yet inflicted?

Till body or rare, and best things else enjoy'd in heav'n

To stand divided light at ev'n and poise their eyes,

Or nourish, lik'ning spiritual, I have thou appear.

── Henley

Chapter 8 總結 (Summary)
============================

1. 符號的名字可以是任何字串，但由 ``read`` 創建的符號預設會被轉成大寫。

2. 符號帶有相關聯的屬性列表，雖然他們不需要是相同的形式，但行爲像是 assoc-lists 。

3. 符號是實質的物件，比較像結構，而不是名字。

4. 包將字串映射至符號。要在包裡給符號創造一個條目的方法是扣留它。符號不需要被扣留。

5. 包通過限制可以引用的名稱增加模組性。預設的包會是 user 包，但爲了提高模組性，大的程式通常分成數個包。

6. 可以讓符號在別的包被存取。關鍵字是自身求值並在所有的包裡都可以存取。

7. 當一個程式用來操作單詞時，用符號來表示單詞是很方便的。

Chapter 8 練習 (Exercises)
==================================

1. 可能有兩個同名符號，但卻不 ``eql`` 嗎？

2. 估計一下用字串表示 "FOO" 與符號表示 foo 所使用記憶體空間的差異。

3. 只使用字串作爲實參 來呼叫 137 頁的 ``defpackage`` 。應該使用符號比較好。爲什麼使用字串可能比較危險呢？

4. 加入需要的程式碼，使圖 7.1 的程式可以放在一個叫做 ``"RING"`` 的包裡，而圖 7.2 的程式放在一個叫做 ``"FILE"`` 包裡。不需要更動現有的程式。

5. 寫一個確認引用的句子是否是由 Henley 生成的程式 (8.8 節)。

6. 寫一版 Henley，接受一個單詞，併產生一個句子，該單詞在句子的中間。


.. rubric:: 腳註

.. [1] 呼叫 ``defpackage`` 裡的名字全部大寫的緣故在 8.1 節提到過，符號的名字預設被轉成大寫。