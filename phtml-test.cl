(eval-when (compile load eval)
  (require :tester))

(defpackage :user (:use :util.test :net.html.parser))  ;; assumes phtml.cl loaded
(in-package :user)

(defvar *test-string*)
(defvar *expected-result*)

(setf *test-string*
    "<html>
       <!-- this should be <h1>one</h1> string -->
       <head>
        <comment> this should be <h1>one</h1> string </COMMENT>
        <title> this is some title text </title> 
       <body> this is some body text
        <a name=\"this is an anchor\">with some text </a>
        <!-- testing allowing looser attribute parsing -->
        <a href=mailto:lmcelroy@performigence.com>lmcelroy@performigence.com
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
        <p>
         <object>
          <p>Navigate the site:
           <map name=\"mainmap\">
            <area shape=rect coords=\"0,100,100,200\">
            <area shape=rect coords=\"100,100,100,200\"> </map> </object> </p>
        <abbr>WWW</abbr> is an abbreviation
        <b>force</b>
        <p>whitespace only")

(setf *expected-result*
    '((:html
       (:comment "this should be <h1>one</h1> string")
       (:head
	(:comment "this should be <h1>one</h1> string")
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
	(:p
	 (:object
	  (:p "Navigate the site:"
	      ((:map :name "mainmap")
	       ((:area :shape "rect" :coords "0,100,100,200"))
	       ((:area :shape "rect" :coords "100,100,100,200"))))))
	(:abbr "WWW")
	"is an abbreviation"
	(:b "force")
	(:p "whitespace only")
	))))

(defmethod lhtml-equal ((a t) (b t))
  (equal a b))

(defmethod lhtml-equal ((a list) (b list))
  (let ((i 0) (j 0))
    (loop
      (when (and (= i (length a)) (= j (length b))) (return t))
      (when (white-space-p (nth i a))
	(incf i)
	(continue))
      (when (white-space-p (nth j b))
	(incf j)
	(continue))
      (when (and (= i (length a)) (/= j (length b)))
	(return
	  (loop
	    (when (= j (length b)) (return t))
	    (when (not (white-space-p (nth j b))) (return nil))
	    (incf j))))
      (when (and (/= i (length a)) (= j (length b)))
	(return
	  (loop
	    (when (= i (length a)) (return t))
	    (when (not (white-space-p (nth i a))) (return nil))
	    (incf i))))
      (when (not (lhtml-equal (nth i a) (nth j b)))
	(return nil))
      (incf i)
      (incf j))))

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
	    (test-t (lhtml-equal arg
				 '((:a :name "this is an anchor") 
				   "with some text")))
       else
	    (setf *pass* 0)
	    (test-t (lhtml-equal arg
				 '((:a :href 
				       "mailto:lmcelroy@performigence.com")
				   "lmcelroy@performigence.com"))))))

(defun testit ()
  (let ((util.test:*test-errors* 0)
	(util.test:*test-successes* 0))
    (setf (element-callback :a) nil)
    (setf *callback-called* 0)
    (test-t (lhtml-equal (parse-html *test-string*) *expected-result*))
    (test-eq 0 *callback-called*)
    (setf (element-callback :a) 'callback-test-func)
    (setf *callback-called* 0)
    (test-t (lhtml-equal (parse-html *test-string*) *expected-result*))
    (test-eq 2 *callback-called*)
    (setf (element-callback :a) nil)
    (setf *callback-called* 0)
    (test-t (lhtml-equal (parse-html *test-string*) *expected-result*))
    (test-eq 0 *callback-called*)
    (setf *callback-called* 0)
    ;; make sure function is OK arg
    (setf (element-callback :a) (symbol-function 'callback-test-func))
    (test-t (lhtml-equal (parse-html *test-string*) *expected-result*))
    (test-eq 2 *callback-called*)
    (setf (element-callback :a) nil)
    ;; try some bad ones
    (test-err (setf (element-callback :a) 1))
    (test-err (setf (element-callback 1) 'callback-test-func))
    (test-err (setf (element-callback 1) 2))
    (format t "End test: ~s,   ~d errors, ~d successes~%"
	    "parse-html" util.test:*test-errors* util.test:*test-successes*)
    ))