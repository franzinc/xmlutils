;; $Id: phtml-test.cl,v 1.13 2000/08/10 22:16:26 sdj Exp $

(eval-when (compile load eval)
  (require :tester))

(defpackage :user (:use :util.test :net.html.parser))  ;; assumes phtml.cl loaded
(in-package :user)

(defvar *test-string*)
(defvar *test-string2*)
(defvar *test-string3*)
(defvar *expected-result*)
(defvar *expected-result2*)
(defvar *expected-result3*)


;; it uses a fake pp tag to test nesting for callbacks...
(setf *test-string*
    "<html>
       <!-- this should be <h1>one</h1> string -->
       <head>
        <style> this should be <h1>one</h1> string </STYLE>
        <title> this is some title text </title> 
       <body> this is some body text
        <a name=\"this is an anchor\">with some text </a>
        <!-- testing allowing looser attribute parsing -->
        <a href= mailto:lmcelroy@performigence.com>lmcelroy@performigence.com
           </a>
        <br>
        this is some more text
        <bogus> tests parser 'looseness'</bogus>
        <select>
         <option>1
         <option>2 </select>
        <ul>
         <li>item 1
         <li>item 2 </ul>
        <dl>
         <dt>a term
         <dd>its definition
         <dt>another term
         <dd>another definition</dl>
        <table>
         <colgroup>
          <col align=\"right\">
          <col align=\"center\">
         <thead>
         <tr>
          <th> this cell is aligned right
          <th> this cell is centered
         <tfoot>
         <tr>
          <th> this cell is aligned right
          <th> this cell is centered
         <tbody>
         <tr>
          <td> this cell is aligned right
          <td> this cell is centered
         <tbody>
         <tr>
          <td> this cell is aligned right
          <td> this cell is centered </table>
        <pp>
         <object>
          <pp>Navigate the site:
           <map name=\"mainmap\">
            <area shape=rect coords=\"0,100,100,200\">
            <area shape=rect coords=\"100,100,100,200\"> </map> </object> </pp>
        <abbr>WWW</abbr> is an abbreviation
        <b>force</b>
        <pp>whitespace only")

(setf *expected-result*
    '((:html
       (:comment "this should be <h1>one</h1> string")
       (:head
	(:style "this should be <h1>one</h1> string")
	(:title "this is some title text"))
       (:body 
	"this is some body text"
        ((:a :name "this is an anchor") "with some text")
	(:comment "testing allowing looser attribute parsing")
	((:a :href "mailto:lmcelroy@performigence.com")
	 "lmcelroy@performigence.com")
	:br
	"this is some more text"
	(:bogus "tests parser 'looseness'")
	(:select
	 (:option "1")
	 (:option "2"))
	(:ul
	 (:li "item 1") 
	 (:li "item 2"))
	(:dl
	 (:dt "a term")
	 (:dd "its definition")
	 (:dt "another term")
	 (:dd "another definition"))
	(:table
	 (:colgroup
	  ((:col :align "right"))
	  ((:col :align "center")))
	 (:thead
	  (:tr
	   (:th "this cell is aligned right")
	   (:th "this cell is centered")))
	 (:tfoot
	  (:tr
	   (:th "this cell is aligned right")
	   (:th "this cell is centered")))
	 (:tbody
	  (:tr
	   (:td "this cell is aligned right")
	   (:td "this cell is centered")))
	 (:tbody
	  (:tr
	   (:td "this cell is aligned right")
	   (:td "this cell is centered"))))
	(:pp
	 (:object
	  (:pp "Navigate the site:"
	      ((:map :name "mainmap")
	       ((:area :shape "rect" :coords "0,100,100,200"))
	       ((:area :shape "rect" :coords "100,100,100,200"))))))
	(:abbr "WWW")
	"is an abbreviation"
	(:b "force")
	(:pp "whitespace only")
	))))

(setf *test-string2*
  "<i><b id=1>text</i> more text</b>
   <!doctype this is some text>
   <![if xxx]>
   <i><b>text</i></b> more text
   <b>text<p>more text</b> yet more text</p>
   <ul><li><b>text<li>more text</ul></b>
   prev<b><a href=foo>bar</a>baz</b>
   <b>foo<a>bar</a>baz</b>
   <b>foo<a>bar</b>baz</a>
   <b>foo<i>bar</i>baz</b>
   <script a=b> some text if (year < 1000) year += 1900; more text </script>
   <script a=b></script>
   <frameset><frame foo><frame bar></frameset>"
  )

(setf *expected-result2*
  '((:i ((:b :id "1") "text")) ((:b :id "1") " more text")
    (:!doctype "this is some text")
    (:! "[if xxx]")
    (:i (:b "text")) (:b) " more text"
    (:b "text") (:p (:b "more text") " yet more text")
    (:ul (:li (:b "text")) (:li (:b "more text"))) (:b)
    "prev" (:b ((:a :href "foo") "bar") "baz")
    (:b "foo" (:a "bar") "baz")
    (:b "foo") (:a (:b "bar") "baz")
    (:b "foo" (:i "bar") "baz")
    ((:script :a "b") " some text if (year < 1000) year += 1900; more text ")
    ((:script :a "b"))
    (:frameset ((:frame :foo "foo")) ((:frame :bar "bar")))
    ))

(setf *test-string3*
  "<ICMETA URL='nytimes.html'>
<NYT_HEADER version='1.0' type='homepage'>
<body bgcolor='#ffffff' background='back5.gif' 
vlink='4' link='6'>
<NYT_BANNER version='1.0' type='homepage'>
<table border=0 cellspacing=0 cellpadding=0>
<tr>
<td bgcolor=0 rowspan=4 width=126 align=left valign=center>
<NYT_AD version='1.0' location=''>
<A HREF='ads.gif' target='top'>
<IMG SRC='http://ads2.gif' BORDER=0  WIDTH=120 HEIGHT=90 ALT='E-Mail Updates from NYTimes.com' ></A>
</NYT_AD>")

(setf *expected-result3*
  '(((:icmeta :url "nytimes.html")) ((:nyt_header :version "1.0" :type "homepage"))
    ((:body :bgcolor "#ffffff" :background "back5.gif" :vlink "4" :link "6")
     ((:nyt_banner :version "1.0" :type "homepage"))
     ((:table :border "0" :cellspacing "0" :cellpadding "0")
      (:tr
       ((:td :bgcolor "0" :rowspan "4" :width "126" :align "left" :valign "center")
	((:nyt_ad :version "1.0" :location "")
	 ((:a :href "ads.gif" :target "top")
	  ((:img :src "http://ads2.gif" :border "0" :width "120" :height "90" :alt
		 "E-Mail Updates from NYTimes.com"))))))))))


(defmethod lhtml-equal ((a t) (b t))
  (equal a b))

(defmethod lhtml-equal ((a list) (b list))
  (let ((i 0) (j 0))
    (loop
      (if* (and (= i (length a)) (= j (length b))) then (return t)
       elseif (and (< i (length a)) (white-space-p (nth i a))) then
	      (incf i)
       elseif (white-space-p (nth j b)) then
	      (incf j)
       elseif (and (= i (length a)) (/= j (length b))) then
	      (return
		(loop
		  (when (= j (length b)) (return t))
		  (when (not (white-space-p (nth j b))) (return nil))
		  (incf j)))
       elseif (and (/= i (length a)) (= j (length b))) then
	      (return
		(loop
		  (when (= i (length a)) (return t))
		  (when (not (white-space-p (nth i a))) (return nil))
		  (incf i)))
       elseif (not (lhtml-equal (nth i a) (nth j b))) then
	      (return nil)
	 else
	      (incf i)
	      (incf j)))))

(defmethod lhtml-equal ((a string) (b string))
  (let ((i 0) (j 0))
    ;; skip white space in beginning
    (loop
      (let ((char (elt a i)))
	(when (and (not (eq char #\space))
		   (not (eq char #\tab))
		   (not (eq char #\return))
		   (not (eq char #\linefeed)))
	  (return)))
      (incf i))
    (loop
      (let ((char (elt b j)))
	(when (and (not (eq char #\space))
		   (not (eq char #\tab))
		   (not (eq char #\return))
		   (not (eq char #\linefeed)))
	  (return)))
      (incf j))
    (loop
      (when (and (= i (length a)) (= j (length b))) (return t))
      (when (and (= i (length a)) (/= j (length b)))
	(return
	  (loop
	    (when (= j (length b)) (return t))
	    (let ((char (elt b j)))
	      (when (and (not (eq char #\space))
			 (not (eq char #\tab))
			 (not (eq char #\return))
			 (not (eq char #\linefeed)))
		(return t)))
	    (incf j))))
      (when (and (/= i (length a)) (= j (length b)))
	(return
	  (loop
	    (when (= i (length a)) (return t))
	    (let ((char (elt a i)))
	      (when (and (not (eq char #\space))
			 (not (eq char #\tab))
			 (not (eq char #\return))
			 (not (eq char #\linefeed)))
		(return t)))
	    (incf i))))
      (when (not (eq (elt a i) (elt b j))) (return nil))
      (incf i)
      (incf j))))

(defmethod white-space-p ((a t))
  nil)

(defmethod white-space-p ((a string))
  (let ((i 0)
	(length (length a)))
    (loop
      (when (= i length) (return t))
      (let ((char (elt a i)))
	(when (and (not (eq char #\space))
		   (not (eq char #\tab))
		   (not (eq char #\return))
		   (not (eq char #\linefeed)))
	  (return nil)))
      (incf i))))

;;------------------------------------------------

(defvar *callback-called* 0)

(let ((*pass* 0))
  (defun callback-test-func (arg)
    ;; incf *callback-called* so we know exactly how many times this is
    ;; called
    (incf *callback-called*)
    (if* (= *pass* 0)
       then
	    (incf *pass*)
	    (test t (lhtml-equal arg
				 '((:a :name "this is an anchor") 
				   "with some text")))
       else
	    (setf *pass* 0)
	    (test t (lhtml-equal arg
				 '((:a :href 
				       "mailto:lmcelroy@performigence.com")
				   "lmcelroy@performigence.com"))))))

(let ((*pass* 0))
  (defun nested-callback (arg)
    ;; incf *callback-called* so we know exactly how many times this is
    ;; called
    (incf *callback-called*)
    (if* (= *pass* 0)
       then
	    (incf *pass*)
	    (test t (lhtml-equal arg
				 '(:pp "Navigate the site:"
				   ((:map :name "mainmap")
				    ((:area :shape "rect" :coords "0,100,100,200"))
				    ((:area :shape "rect" :coords "100,100,100,200"))))))
     elseif (= *pass* 1)
       then
	    (incf *pass*)
	    (test t (lhtml-equal arg
				 '(:pp
				   (:object
				    (:pp "Navigate the site:"
				     ((:map :name "mainmap")
				      ((:area :shape "rect" :coords "0,100,100,200"))
				      ((:area :shape "rect" 
					      :coords "100,100,100,200"))))))))
       else
	    (setf *pass* 0)
	    (test t (lhtml-equal arg
				 '(:pp "whitespace only"))))))

(defun testit ()
  (let ((util.test:*test-errors* 0)
	(util.test:*test-successes* 0))
    (test t (lhtml-equal (parse-html *test-string2*) *expected-result2*))
    (setf *callback-called* 0)
    (test t (lhtml-equal (parse-html *test-string*) *expected-result*))
    (test 0 *callback-called*)
    ;;(setf (element-callback :a) 'callback-test-func)
    (setf *callback-called* 0)
    (test t (lhtml-equal (parse-html *test-string* 
				     :callbacks (acons :a 'callback-test-func nil)) 
			 *expected-result*))
    (test 2 *callback-called*)
    (setf *callback-called* 0)
    (test t (lhtml-equal (parse-html *test-string*) *expected-result*))
    (test 0 *callback-called*)
    (setf *callback-called* 0)
    ;; make sure function is OK arg
    ;;(setf (element-callback :a) (symbol-function 'callback-test-func))
    (test t (lhtml-equal 
	     (parse-html *test-string*
			 :callbacks (acons :a (symbol-function 'callback-test-func) nil)) 
			 *expected-result*))
    (test 2 *callback-called*)
    ;; try with :callback-only t
    (setf *callback-called* 0)
    ;;(setf (element-callback :a) 'callback-test-func)
    (parse-html *test-string* :callback-only t
		:callbacks (acons :a 'callback-test-func nil)) ;; won't return parse output
    (test 2 *callback-called*)
    ;; try nested callback
    (setf *callback-called* 0)
    ;;(setf (element-callback :p) 'nested-callback)
    (test t (lhtml-equal (parse-html *test-string*
				     :callbacks (acons :pp 'nested-callback nil))
			 *expected-result*))
    (test 3 *callback-called*)
    (setf *callback-called* 0)
    (parse-html *test-string* :callback-only t
		:callbacks (acons :pp 'nested-callback nil))
    (test 3 *callback-called*)
    (test-error (parse-html "b<a"))
    (test t (lhtml-equal
	     (multiple-value-bind (res rogues)
		 (parse-html *test-string3* :collect-rogue-tags t)
	       (declare (ignorable res))
	       (parse-html *test-string3* :no-body-tags rogues))
	     *expected-result3*))
    (format t "End test: ~s,   ~d errors, ~d successes~%"
	    "parse-html" util.test:*test-errors* util.test:*test-successes*)
    ))
