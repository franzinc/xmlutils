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
         <tr>
          <th> this cell is aligned right
          <th> this cell is centered
         <tr>
          <th> this cell is aligned right
          <th> this cell is centered
         <tr>
          <td> this cell is aligned right
          <td> this cell is centered
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
	 (:tr
	  (:th "this cell is aligned right")
	  (:th "this cell is centered"))
	 (:tr
	  (:th "this cell is aligned right")
	  (:th "this cell is centered"))
	 (:tr
	  (:td "this cell is aligned right")
	  (:td "this cell is centered"))
	 (:tr
	  (:td "this cell is aligned right")
	  (:td "this cell is centered")))
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
      
      