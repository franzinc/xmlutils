;; $Id: pxml3.cl,v 1.2 2000/07/17 20:03:07 layer Exp $

(in-package :net.xml.parser)

(defun parse-dtd (stream ignore-strings read-sequence-func
		  parameter-entities general-entities tokenbuf entity-buf external)
  (declare (optimize (speed 3) (safety 1)))
  (let ((guts))
    (loop
      (multiple-value-bind (val kind p-ents g-ents e-buf)
	  (next-dtd-token stream ignore-strings read-sequence-func
			  parameter-entities general-entities tokenbuf entity-buf
			  external)
	(setf parameter-entities p-ents)
	(setf general-entities g-ents)
	(setf entity-buf e-buf)
	(if* (eq kind :end-dtd) then 
		(return (values (nreverse guts) parameter-entities general-entities))
	   else (push val guts))))))

(eval-when (compile load eval)
  (defconstant state-dtdstart 0)
  (defconstant state-tokenstart 1)
  (defconstant state-dtd-? 2)
  (defconstant state-dtd-! 3)
  (defconstant state-dtd-comment 4)
  (defconstant state-dtd-!-token 5)
  (defconstant state-dtd-!-element 6)
  (defconstant state-dtd-!-element-name 7)
  (defconstant state-dtd-!-element-content 8)
  (defconstant state-dtd-!-element-type 9)
  (defconstant state-dtd-!-element-type-paren 10)
  (defconstant state-dtd-!-element-type-token 11)
  (defconstant state-dtd-!-element-type-end 12)
  (defconstant state-dtd-!-element-type-paren-name 13)
  (defconstant state-dtd-!-element-type-paren-pcd 14)
  (defconstant state-dtd-!-element-type-paren-pcd2 15)
  (defconstant state-dtd-!-element-type-paren-pcd3 16)
  (defconstant state-dtd-!-element-type-paren-pcd4 17)
  (defconstant state-dtd-!-element-type-paren-pcd5 18)
  (defconstant state-dtd-!-element-type-paren-pcd6 19)
  (defconstant state-dtd-!-element-type-paren-pcd7 20)
  (defconstant state-dtd-!-element-type-paren-pcd8 21)
  (defconstant state-dtd-!-element-type-paren-pcd9 22)
  (defconstant state-dtd-!-element-type-paren-name2 23)
  ;;(defconstant state-dtd-!-element-type-paren-seq 24) folded into choice
  (defconstant state-dtd-!-element-type-paren-choice 25)
  (defconstant state-dtd-!-element-type-paren2 26)
  (defconstant state-dtd-!-element-type-paren-choice-name 27)
  (defconstant state-dtd-!-element-type-paren-choice-paren 28)
  (defconstant state-dtd-!-element-type-paren-choice-name2 29)
  (defconstant state-dtd-!-element-type-paren3 30)
  (defconstant state-dtd-!-element-type-paren-choice-name3 31)
  (defconstant state-dtd-!-attlist 32)
  (defconstant state-dtd-!-attlist-name 33)
  (defconstant state-dtd-!-attdef 34)
  (defconstant state-dtd-!-attdef-name 35)
  (defconstant state-dtd-!-attdef-type 36)
  ;;(defconstant state-dtd-!-attdef-enumeration 37)
  (defconstant state-dtd-!-attdef-decl 38)
  (defconstant state-dtd-!-attdef-decl-type 39)
  (defconstant state-dtd-!-attdef-decl-value 40)
  (defconstant state-dtd-!-attdef-decl-value2 41)
  (defconstant state-dtd-!-attdef-decl-value3 42)
  (defconstant state-dtd-!-attdef-decl-value4 43)
  (defconstant state-dtd-!-attdef-decl-value5 44)
  (defconstant state-dtd-!-attdef-decl-value6 45)
  (defconstant state-dtd-!-attdef-decl-value7 46)
  (defconstant state-dtd-!-attdef-notation 47)
  (defconstant state-dtd-!-attdef-notation2 48)
  (defconstant state-dtd-!-attdef-notation3 49)
  (defconstant state-dtd-!-attdef-notation4 50)
  (defconstant state-dtd-!-attdef-type2 51)
  (defconstant state-dtd-!-entity 52)
  (defconstant state-dtd-!-entity2 53)
  (defconstant state-dtd-!-entity3 54)
  (defconstant state-dtd-!-entity4 55)
  (defconstant state-dtd-!-entity-value 56)
  (defconstant state-dtd-!-entity5 57)
  (defconstant state-dtd-!-entity6 58)
  (defconstant state-!-dtd-system 59)
  (defconstant state-!-dtd-public 60)
  (defconstant state-!-dtd-system2 61)
  (defconstant state-!-dtd-system3 62)
  (defconstant state-!-dtd-system4 63)
  (defconstant state-!-dtd-system5 64)
  (defconstant state-!-dtd-system6 65)
  (defconstant state-!-dtd-system7 66)
  (defconstant state-!-dtd-public2 67)
  (defconstant state-dtd-!-notation 68)
  (defconstant state-dtd-!-notation2 69)
  (defconstant state-dtd-!-notation3 70)
  (defconstant state-dtd-?-2 71)
  (defconstant state-dtd-?-3 72)
  (defconstant state-dtd-?-4 73)
  (defconstant state-dtd-comment2 74)
  (defconstant state-dtd-comment3 75)
  (defconstant state-dtd-comment4 76)
  (defconstant state-dtd-!-entity7 77)
  (defconstant state-dtd-pref 78)
  (defconstant state-dtd-pref2 79)
  )

(defun next-dtd-token (stream ignore-strings read-sequence-func
		       parameter-entities general-entities tokenbuf entity-buf
		       external)
  (declare (optimize (speed 3) (safety 1)))
  (macrolet ((next-char (stream)
	       `(let ((cur (tokenbuf-cur tokenbuf))
		      (tb (tokenbuf-data tokenbuf)))
		  (if* (>= cur (tokenbuf-max tokenbuf))
		     then ; fill buffer
			  (if* (zerop (setf (tokenbuf-max tokenbuf)
					(if* read-sequence-func
					   then (funcall read-sequence-func tb stream)
					   else (read-sequence tb stream))))
			     then (setq cur nil) ; eof
			     else (setq cur 0)))
		  (if* cur
		     then (prog1 (schar tb cur)
			    (setf (tokenbuf-cur tokenbuf) (1+ cur))))))
	     
	     (get-next-char (stream)
	       `(if* entity-buf then
			(let* ((buf (first entity-buf))
			       (cur (tokenbuf-cur buf)))
			  #+ignore (format t "max: ~s index: ~s from entity-buf: ~s~%" 
				  (tokenbuf-max buf) cur (schar (tokenbuf-data buf) cur))
			  (prog1 (schar (tokenbuf-data buf) cur)
			    (setf (tokenbuf-cur buf) (1+ cur))
			    (when (= (tokenbuf-cur buf)
				     (tokenbuf-max buf))
			      (put-back-tokenbuf buf)
			      (setf entity-buf (rest entity-buf)))))
		   else (next-char stream)))
	     
	     (un-next-char (stream ch)  ;; XML syntax rules should prevent edge condition
	                                ;; where last char was the last char in an entity buf
	       `(if* entity-buf then
			(decf (tokenbuf-cur (first entity-buf)))
		   else (decf (tokenbuf-cur tokenbuf))))
	     
	     (clear-coll (coll)
	       `(setf (collector-next ,coll) 0))
		     
	     (add-to-coll (coll ch)
	       `(let ((.next. (collector-next ,coll)))
		  (if* (>= .next. (collector-max ,coll))
		     then (grow-and-add ,coll ,ch)
		     else (setf (schar (collector-data ,coll) .next.)
			    ,ch)
			  (setf (collector-next ,coll) (1+ .next.)))))
	     
	     (to-preferred-case (ch)
	       ;; should check the case mode
	       `(char-downcase ,ch))
	       
	     )
    (let ((state state-dtdstart)
	  (coll  (get-collector))
	  (entity  (get-collector))
	  (tag-to-return)
	  (contents-to-return)
	  (pending (list nil))
	  (pending-type)
	  (value-delim)
	  (char-code 0)
	  (reference-save-state)
	  (prefp)
	  (entityp)
	  (pentityp)
	  (ch))
      (loop
	(setq ch (get-next-char stream))
	#+ignore (format t "dtd char: ~s state: ~s contents: ~s pending: ~s~%" 
		ch state contents-to-return pending)
	(if* (null ch)
	   then (setf state :eof)
		(return)		;; eof -- exit loop
		)
	(case state
	  (#.state-dtdstart
	   (if* (eq #\] ch) then
		   (when external (error "need to distinguish between error and start of INCLUDE end"))
		   (return)
	    elseif (eq #\< ch) then (setf state state-tokenstart)
	    elseif (xml-space-p ch) then nil
	    elseif (eq #\% ch) then (setf state state-dtd-pref)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD characters, starting at: '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-pref
	   (if* (xml-name-start-char-p ch) then
		   (add-to-coll coll ch)
		   (setf state state-dtd-pref2)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD parameter reference name, starting at: '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-pref2
	   (if* (xml-name-char-p ch)
	      then (add-to-coll coll ch)
	    elseif (eq #\; ch)
	      then (let* ((entity-symbol (compute-tag coll))
			  (p-value (assoc entity-symbol parameter-entities)))
		     (clear-coll coll)
		     (if* p-value then
			     (setf p-value (rest p-value))
			     (let* ((buf (get-tokenbuf))
				    (tb (tokenbuf-data buf)))
			       (setf (tokenbuf-max buf) (+ 2 (length p-value)))
			       (dotimes (i (length p-value))
				 (setf (schar tb (+ 1 i)) (char p-value i)))
			       (setf (schar tb 0) #\ )
			       (setf (schar tb (+ 1 (length p-value))) #\ )
			       (push buf entity-buf))
			     (setf state state-dtdstart)
			else
			     (push :parameter-reference contents-to-return)
			     (push (string entity-symbol) contents-to-return)
			     (return)))
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD parameter reference name, starting at: '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-tokenstart
	   (if* (eq #\? ch) then (setf state state-dtd-?)
	    elseif (eq #\! ch) then (setf state state-dtd-!)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD characters, starting at: '<"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-?
	   (if* (xml-name-char-p ch)
	      then
		   (add-to-coll coll ch)
	      else
		   (when (not (xml-space-p ch))
		     (xml-error (concatenate 'string 
				  "expecting name following: '<?"
				  (compute-coll-string coll)
				  "' ; got: '" (string ch) "'"))
		     )
		   (when (= (collector-next coll) 0)
		     (xml-error "null <? token"))
		   (if* (and (= (collector-next coll) 3)
			     (or (eq (elt (collector-data coll) 0) #\X) 
				 (eq (elt (collector-data coll) 0) #\x))
			     (or (eq (elt (collector-data coll) 1) #\M) 
				 (eq (elt (collector-data coll) 1) #\m))
			     (or (eq (elt (collector-data coll) 2) #\L) 
				 (eq (elt (collector-data coll) 2) #\l)))
		      then
			   (xml-error "<?xml not allowed in dtd")
		      else
			   (setq tag-to-return (compute-tag coll))
			   (setf state state-dtd-?-2))
		   (clear-coll coll)))
	  (#.state-dtd-?-2
	   (if* (xml-space-p ch)
	      then nil
	    elseif (not (xml-char-p ch))
	      then (xml-error "XML is not well formed") ;; no test
	      else (add-to-coll coll ch)
		   (setf state state-dtd-?-3)))
	  (#.state-dtd-?-3
	   (if* (eq #\? ch)
	      then (setf state state-dtd-?-4)
	    elseif (not (xml-char-p ch))
	      then (xml-error "XML is not well formed") ;; no test
	      else (add-to-coll coll ch)))
	  (#.state-dtd-?-4
	   (if* (eq #\> ch)
	      then 
		   (push (compute-coll-string coll) contents-to-return)
		   (clear-coll coll)
		   (return)
	      else (setf state state-dtd-?-3)
		   (add-to-coll coll #\?)
		   (add-to-coll coll ch)))
	  (#.state-dtd-!
	   (if* (eq #\- ch) then (setf state state-dtd-comment)
	    elseif (xml-name-start-char-p ch) then (setf state state-dtd-!-token)
		   (un-next-char stream ch)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD characters, starting at: '<!"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-comment
	   (if* (eq #\- ch)
	      then (setf state state-dtd-comment2)
		   (setf tag-to-return :comment)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal token following '<![-', starting at '<!-"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-comment2
	   (if* (eq #\- ch)
	      then (setf state state-dtd-comment3)
	      else (add-to-coll coll ch)))
	  (#.state-dtd-comment3
	   (if* (eq #\- ch)
	      then (setf state state-dtd-comment4)
	      else (setf state state-dtd-comment2)
		   (add-to-coll coll #\-) (add-to-coll coll ch)))
	  (#.state-dtd-comment4
	   (if* (eq #\> ch)
	      then (push (compute-coll-string coll) contents-to-return)
		   (clear-coll coll)
		   (return)
	      else  (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal token following '--' comment terminator, starting at '--"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-token
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (xml-space-p ch) then
		   (setf tag-to-return (compute-tag coll))
		   (clear-coll coll)
		   (if* (eq tag-to-return :ELEMENT) then (setf state state-dtd-!-element)
		    elseif (eq tag-to-return :ATTLIST) then 
			   (setf state state-dtd-!-attlist)
		    elseif (eq tag-to-return :ENTITY) then 
			   (setf entityp t)
			   (setf state state-dtd-!-entity)
		    elseif (eq tag-to-return :NOTATION) then 
			   (setf state state-dtd-!-notation)
		      else
			   (xml-error (concatenate 'string
					"illegal DTD characters, starting at: '<!"
					(string tag-to-return)
					"'")))
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD characters, starting at: '<!"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-notation
	      (if* (xml-space-p ch) then nil
	       elseif (xml-name-start-char-p ch) then
		      (add-to-coll coll ch)
		      (setf state state-dtd-!-notation2)
		 else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD characters, starting at: '<!NOTATION "
				(compute-coll-string coll)
				"'"))
		      ))
	  (#.state-dtd-!-notation2
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (xml-space-p ch) then
		   (push (compute-tag coll) contents-to-return)
		   (clear-coll coll)
		   (setf state state-dtd-!-notation3)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!NOTATION name: "
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-notation3
	   (if* (xml-space-p ch) then nil
	    elseif (xml-name-char-p ch) then
		   (add-to-coll coll ch)
		   (setf state state-dtd-!-entity6)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!NOTATION spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-entity
	   (if* (eq #\% ch) then (push :param contents-to-return)
		   (setf pentityp t)
		   (setf state state-dtd-!-entity2)
	    elseif (xml-name-start-char-p ch) then
		   (add-to-coll coll ch)
		   (setf pending nil)
		   (setf state state-dtd-!-entity3)
	    elseif (xml-space-p ch) then nil
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD characters, starting at: '<!ENTITY "
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-entity2
	   (if* (xml-space-p ch) then (setf state state-dtd-!-entity7)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ENTITY spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-entity3
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (xml-space-p ch) then 
		   (push (compute-tag coll) contents-to-return)
		   (setf contents-to-return
		     (nreverse contents-to-return))
		   (clear-coll coll)
		   (setf state state-dtd-!-entity4)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ENTITY name: "
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-entity4
	   (if* (xml-space-p ch) then nil
	    elseif (or (eq #\' ch) (eq #\" ch)) then
		   (setf value-delim ch)
		   (setf state state-dtd-!-entity-value)
	    elseif (xml-name-start-char-p ch) then
		   (add-to-coll coll ch)
		   (setf state state-dtd-!-entity6)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ENTITY spec: '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-entity6
	   (if* (xml-name-char-p ch) ;; starting char already passed more restrictive test
	      then
		   (add-to-coll coll ch)
	      else
		   (when (not (xml-space-p ch))
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char stream))
		       (if* (null ch)
			  then (return)))
		     (xml-error 
		      (concatenate 'string
			"illegal character in '"
			(compute-coll-string coll)
			"' in <! tag: " (string tag-to-return) " " 
			(string (first contents-to-return))   
		      ))
		     )
		   (let ((token (compute-tag coll)))
		     (push token contents-to-return)
		     (clear-coll coll)
		     (if* (eq :SYSTEM token) then (setf state state-!-dtd-system)
		      elseif (eq :PUBLIC token) then (setf state state-!-dtd-public)
			else (xml-error 
			      (concatenate 'string
				"expected 'SYSTEM' or 'PUBLIC' got '"
				(string (first contents-to-return))
				"' in <! tag: " (string tag-to-return) " "
				(string (second contents-to-return))))
			     )
		     )))
	  (#.state-dtd-!-entity7
	   (if* (xml-space-p ch) then nil
	    elseif (xml-name-start-char-p ch) then
		   (add-to-coll coll ch)
		   (setf state state-dtd-!-entity3)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ENTITY % name: "
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-!-dtd-public
	   (if* (xml-space-p ch) then nil
		   elseif (or (eq #\" ch) (eq #\' ch)) then 
		   (setf state state-!-dtd-public2)
		   (setf value-delim ch)
	      else (xml-error 
		    (concatenate 'string
		      "expected quote or double-quote got: '"
		      (string ch)
		      "' in <! tag: " (string tag-to-return) " "
		      (string (second contents-to-return)) " "
		      (string (first contents-to-return))
		      ))))
	  (#.state-!-dtd-public2
	   (if* (eq value-delim ch) then 
		   (push (compute-coll-string coll) contents-to-return)
		   (clear-coll coll)
		   (setf state state-!-dtd-system)
	    elseif (pub-id-char-p ch) then (add-to-coll coll ch)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error
		    (concatenate 'string
		      "illegal character in string: '"
		      (compute-coll-string coll) "'"))
		   ))
	  (#.state-!-dtd-system
	   (if* (xml-space-p ch) then nil
	    elseif (or (eq #\" ch) (eq #\' ch)) then 
		   (setf state state-!-dtd-system2)
		   (setf value-delim ch)
	    elseif (and (or (not entityp) pentityp)
			(eq #\> ch)) then (return)
	      else (xml-error 
		    (concatenate 'string
		      "expected quote or double-quote got: '"
		      (string ch)
		      "' in <! tag: " (string tag-to-return) " "
		      (string (second contents-to-return)) " "
		      (string (first contents-to-return))
		      ))))
	  (#.state-!-dtd-system2
	   (when (not (xml-char-p ch))
	     (xml-error "XML is not well formed")) ;; not tested
	   (if* (eq value-delim ch) then 
		   (push (compute-coll-string coll) contents-to-return)
		   (clear-coll coll)
		   (setf state state-!-dtd-system3)
	      else (add-to-coll coll ch)))
	  (#.state-!-dtd-system3
	   (if* (xml-space-p ch) then (setf state state-!-dtd-system4)
	    elseif (eq #\> ch) then (return)
	      else
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ENTITY value for "
				(string (first (nreverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-!-dtd-system4
	   (if* (xml-space-p ch) then nil
	    elseif (and (not pentityp) (xml-name-start-char-p ch)) then
		   (add-to-coll coll ch)
		   (setf state state-!-dtd-system5)
	    elseif (eq #\> ch) then (return)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ENTITY value for "
				(string (first (nreverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-!-dtd-system5
	   (if* (xml-name-char-p ch) then
		   (add-to-coll coll ch)
	    elseif (xml-space-p ch) then
		   (let ((token (compute-tag coll)))
		     (when (not (eq :NDATA token))
		       (dotimes (i 15)
			 (add-to-coll coll ch)
			 (setq ch (get-next-char stream))
			 (if* (null ch)
			    then (return)))
		       (xml-error (concatenate 'string
				    "illegal DTD <!ENTITY value for "
				    (string (first (nreverse contents-to-return)))
				    ": '"
				    (compute-coll-string coll)
				    "'"))
		       )
		     (clear-coll coll)
		     (push token contents-to-return)
		     (setf state state-!-dtd-system6))
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ENTITY value for "
				(string (first (nreverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-!-dtd-system6
	   (if* (xml-space-p ch) then nil
	    elseif (xml-name-start-char-p ch) then
		   (add-to-coll coll ch)
		   (setf state state-!-dtd-system7)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ENTITY value for "
				(string (first (nreverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-!-dtd-system7
	   (if* (xml-name-char-p ch) then
		   (add-to-coll coll ch)
	    elseif (xml-space-p ch) then
		   (push (compute-tag coll) contents-to-return)
		   (clear-coll coll)
		   (setf state state-dtd-!-entity5) ;; just looking for space, >
	    elseif (eq #\> ch) then
		   (push (compute-tag coll) contents-to-return)
		   (clear-coll coll)
		   (return)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ENTITY value for "
				(string (first (nreverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-entity-value
	   (if* (eq ch value-delim) then
		   (push (compute-coll-string coll) pending)
		   (push (first pending) contents-to-return)
		   (setf pending (list nil))
		   (setf state state-dtd-!-entity5)
		   (clear-coll coll)
		   (if* pentityp then
			   (when (not (assoc (third contents-to-return)
					     parameter-entities))
			     (setf parameter-entities
			       (acons (third contents-to-return)
				      (first contents-to-return)
				      parameter-entities)))
		      else
			   (when (not (assoc (second contents-to-return)
					     general-entities))
			     (setf general-entities
			       (acons (second contents-to-return)
				      (first contents-to-return)
				      general-entities))))
	    elseif (eq #\& ch) then
		   (setf reference-save-state state-dtd-!-entity-value)
		   (setf state state-dtd-!-attdef-decl-value3)
	    elseif (eq #\% ch) then
		   (setf prefp t)
		   (setf reference-save-state state-dtd-!-entity-value)
		   (setf state state-dtd-!-attdef-decl-value3)
	    elseif (xml-char-p ch)
	      then (add-to-coll coll ch)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ENTITY value for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-entity5
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\> ch) then (return)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD contents following <!ENTITY spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-attlist
	   (if* (xml-name-start-char-p ch) then (setf state state-dtd-!-attlist-name)
		   (un-next-char stream ch)
	    elseif (xml-space-p ch) then nil
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD characters, starting at: '<!ATTLIST "
				(compute-coll-string coll)
				"'"))))
	  (#.state-dtd-!-attlist-name
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (xml-space-p ch) then
		   (push (compute-tag coll) 
			 contents-to-return)
		   (clear-coll coll)
		   (setf state state-dtd-!-attdef)
	    elseif (eq #\> ch) then
		   (push (compute-tag coll) 
			 contents-to-return)
		   (clear-coll coll)
		   (return)
	      else (push (compute-tag coll) 
			 contents-to-return)
		   (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ATTLIST content spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-attdef
	   (if* (xml-space-p ch) then nil
	    elseif (xml-name-start-char-p ch) then
		   (un-next-char stream ch)
		   (setf state state-dtd-!-attdef-name)
	    elseif (eq #\> ch) then (return)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ATTLIST content spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-attdef-name
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (xml-space-p ch) then
		   (setf (first pending) (compute-tag coll))
		   (clear-coll coll)
		   (setf state state-dtd-!-attdef-type)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ATTLIST type spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-attdef-type
	   (if* (xml-space-p ch) then nil
	      else (un-next-char stream ch)
		   ;; let next state do all other checking
		   (setf state state-dtd-!-attdef-type2)))
	  (#.state-dtd-!-attdef-type2
	   ;; can only be one of a few tokens, but wait until token built to check
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (eq #\( ch) then
		   (push (list :enumeration) pending)
		   (setf state state-dtd-!-attdef-notation2)
	    elseif (xml-space-p ch) then
		   (let ((token (compute-tag coll)))
		     (when (and (not (eq :CDATA token))
				(not (eq :ID token))
				(not (eq :IDREF token))
				(not (eq :IDREFS token))
				(not (eq :ENTITY token))
				(not (eq :ENTITIES token))
				(not (eq :NMTOKEN token))
				(not (eq :NMTOKENS token))
				(not (eq :NOTATION token)))
		       (dotimes (i 15)
			 (add-to-coll coll ch)
			 (setq ch (get-next-char stream))
			 (if* (null ch)
			    then (return)))
		       (xml-error (concatenate 'string
				    "illegal DTD <!ATTLIST type spec for "
				    (string (first contents-to-return))
				    ": '"
				    (compute-coll-string coll)
				    "'")))
		     (if* (eq token :NOTATION) then
			     (push (list token) pending)
			     (setf state state-dtd-!-attdef-notation)
			else
			     (push token pending)
			     (setf state state-dtd-!-attdef-decl))
		     )
		   (clear-coll coll)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ATTLIST type spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-attdef-notation
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\( ch) then (setf state state-dtd-!-attdef-notation2)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ATTLIST type spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-attdef-notation2
	   (if* (xml-space-p ch) then nil
	    elseif (xml-name-start-char-p ch) then
		   (setf state state-dtd-!-attdef-notation3)
		   (add-to-coll coll ch)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ATTLIST type spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-attdef-notation3
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (xml-space-p ch) then
		   (push (compute-tag coll) (first pending))
		   (clear-coll coll)
		   (setf state state-dtd-!-attdef-notation4)
	    elseif (eq #\| ch) then
		   (push (compute-tag coll) (first pending))
		   (clear-coll coll)
		   (setf state state-dtd-!-attdef-notation2)
	    elseif (eq #\) ch) then
		   (push (compute-tag coll) (first pending))
		   (clear-coll coll)
		   (setf (first pending) (nreverse (first pending)))
		   (setf state state-dtd-!-attdef-decl)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ATTLIST type spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-attdef-notation4
	   (if* (xml-space-p ch) then nil
	    elseif (xml-name-char-p ch) then (add-to-coll coll ch)
		   (setf state state-dtd-!-attdef-notation3)
	    elseif (eq #\| ch) then (setf state state-dtd-!-attdef-notation2)
	    elseif (eq #\) ch) then (setf state state-dtd-!-attdef-decl)
		   (setf (first pending) (nreverse (first pending)))
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ATTLIST type spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-attdef-decl
	   (if* (eq #\# ch) then
		   (setf state state-dtd-!-attdef-decl-type)
	    elseif (or (eq #\' ch) (eq #\" ch)) then
		   (setf value-delim ch)
		   (setf state state-dtd-!-attdef-decl-value)
	    elseif (xml-space-p ch) then nil
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ATTLIST type spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-attdef-decl-value
	   (if* (eq ch value-delim) then
		   (push (compute-coll-string coll) pending)
		   (setf contents-to-return
		     (append contents-to-return
			     (if* entityp then
				    (nreverse pending) 
				else (list (nreverse pending)))))
		   (setf pending (list nil))
		   (setf state state-dtd-!-attdef)
		   (clear-coll coll)
	    elseif (eq #\& ch) then (setf state state-dtd-!-attdef-decl-value3)
		   (setf reference-save-state state-dtd-!-attdef-decl-value)
	    elseif (and (xml-char-p ch) (not (eq #\< ch)))
	      then (add-to-coll coll ch)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ATTLIST type spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-attdef-decl-value3
	   (if* (eq #\# ch)
	      then (setf state state-dtd-!-attdef-decl-value4)
	    elseif (xml-name-start-char-p ch)
	      then (setf state state-dtd-!-attdef-decl-value5)
		   (un-next-char stream ch)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal reference name, starting at: '&"
				(compute-coll-string coll)
				"'"))))
	  (#.state-dtd-!-attdef-decl-value4
	   (if* (eq #\x ch)
	      then (setf state state-dtd-!-attdef-decl-value6)
	    elseif (<= (char-code #\0) (char-code ch) (char-code #\9))
	      then (setf state state-dtd-!-attdef-decl-value7)
		   (un-next-char stream ch)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal character reference code, starting at: '&#"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-attdef-decl-value5
	   (if* (xml-name-char-p ch)
	      then (add-to-coll entity ch)
	    elseif (eq #\; ch)
	      then (let ((entity-symbol (compute-tag entity)))
		     (clear-coll entity)
		     (if* (eq entity-symbol :amp) then (add-to-coll coll #\&)
		      elseif (eq entity-symbol :lt) then (add-to-coll coll #\<)
		      elseif (eq entity-symbol :gt) then (add-to-coll coll #\>)
		      elseif (eq entity-symbol :apos) then (add-to-coll coll #\')
		      elseif (eq entity-symbol :quot) then (add-to-coll coll #\")
			else (let ((p-value (assoc entity-symbol 
						   (if prefp parameter-entities
						     general-entities))))
			       (if* p-value then
				       (setf p-value (rest p-value))
				       (let* ((buf (get-tokenbuf))
					      (tb (tokenbuf-data buf)))
					 (setf (tokenbuf-max buf) (+ (if prefp 2 0)
								     (length p-value)))
					 (dotimes (i (length p-value))
					   (setf (schar tb (+ (if prefp 1 0)
							      i)) (char p-value i)))
					 (when prefp
					   (setf prefp nil)
					   (setf (schar tb 0) #\ )
					   (setf (schar tb (+ 1 (length p-value))) #\ ))
					 (push buf entity-buf))
				       else
				       (let ((entity-string (format nil "~s" entity-symbol)))
					 (dotimes (i (length entity-string))
					   (if* (= i 0)
					      then (if* prefp then
							   (add-to-coll coll #\%)
						      else (add-to-coll coll #\&))
					      else (add-to-coll coll (elt entity-string i))))
					 (add-to-coll coll #\;)
					 (when prefp (setf prefp nil)))
				       ))))
		   (setf state reference-save-state)
	      else (let ((tmp (compute-coll-string entity)))
		     (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char stream))
		       (if* (null ch)
			  then (return)))
		     (xml-error (concatenate 'string
				  "reference not terminated by ';', starting at: '&"
				  tmp
				  (compute-coll-string coll)
				  "'")))
		   ))
	  (#.state-dtd-!-attdef-decl-value6
	   (let ((code (char-code ch)))
	     (if* (eq #\; ch)
		then (add-to-coll coll (code-char char-code))
		     (setf char-code 0)
		     (setq state state-dtd-!-attdef-decl-value)
	      elseif (<= (char-code #\0) code (char-code #\9))
		then (setf char-code (+ (* char-code 16) (- code (char-code #\0))))
	      elseif (<= (char-code #\A) code (char-code #\F))
		then (setf char-code (+ 10 (* char-code 16) (- code (char-code #\A))))
	      elseif (<= (char-code #\a) code (char-code #\f))
		then (setf char-code (+ 10 (* char-code 16) (- code (char-code #\a))))
		else (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char stream))
		       (if* (null ch)
			  then (return)))
		     (xml-error (concatenate 'string
				  "illegal hexidecimal character reference code, starting at: '"
				  (compute-coll-string coll)
				  "', calculated char code: "
				  (format nil "~s" char-code)))
		     )))
	  (#.state-dtd-!-attdef-decl-value7
	   (let ((code (char-code ch)))
	     (if* (eq #\; ch)
		then (add-to-coll coll (code-char char-code))
		     (setf char-code 0)
		     (setq state state-dtd-!-attdef-decl-value)
	      elseif (<= (char-code #\0) code (char-code #\9))
		then (setf char-code (+ (* char-code 10) (- code (char-code #\0))))
		else (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char stream))
		       (if* (null ch)
			  then (return)))
		     (xml-error (concatenate 'string
				  "illegal decimal character reference code, starting at: '"
				  (compute-coll-string coll)
				  "', calculated char code: "
				  (format nil "~s" char-code)))
		     )))
	  (#.state-dtd-!-attdef-decl-type
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (or (xml-space-p ch) (eq #\> ch)) then
		   (let ((token (compute-tag coll)))
		     (when (and (not (eq :REQUIRED token))
				(not (eq :IMPLIED token))
				(not (eq :FIXED token)))
		       (dotimes (i 15)
			 (add-to-coll coll ch)
			 (setq ch (get-next-char stream))
			 (if* (null ch)
			    then (return)))
		       (xml-error (concatenate 'string
				    "illegal DTD <!ATTLIST type spec for "
				    (string (first contents-to-return))
				    ": '"
				    (compute-coll-string coll)
				    "'")))
		     (push token pending)
		     (if* (eq :FIXED token) then
			     (when (eq #\> ch)
			       (dotimes (i 15)
				 (add-to-coll coll ch)
				 (setq ch (get-next-char stream))
				 (if* (null ch)
				    then (return)))
			       (xml-error (concatenate 'string
					    "illegal DTD <!ATTLIST type spec for "
					    (string (first contents-to-return))
					    ": '"
					    (compute-coll-string coll)
					    "'")))
			     (setf state state-dtd-!-attdef-decl-value2)
		      elseif (eq #\> ch) then (return)
			else (setf contents-to-return 
			       (append contents-to-return (list (nreverse pending))))
			     (setf pending (list nil))
			     (setf state state-dtd-!-attdef)))
		   (clear-coll coll)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ATTLIST type spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#. state-dtd-!-attdef-decl-value2
	      (if* (xml-space-p ch) then nil
	       elseif (or (eq #\' ch) (eq #\" ch)) then
		      (setf value-delim ch)
		      (setf state state-dtd-!-attdef-decl-value)
		 else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ATTLIST type spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		      ))
	  (#.state-dtd-!-element
	   (if* (xml-space-p ch) then nil
	    elseif (xml-name-start-char-p ch) then (setf state state-dtd-!-element-name)
		   (un-next-char stream ch)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD characters, starting at: '<!ELEMENT "
				(compute-coll-string coll)
				"'"))))
	  (#.state-dtd-!-element-name
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (xml-space-p ch) then
		   (push (compute-tag coll) 
			 contents-to-return)
		   (clear-coll coll)
		   (setf state state-dtd-!-element-type)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT name: '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type
	   (if* (eq #\( ch) then (setf state state-dtd-!-element-type-paren)
	    elseif (xml-space-p ch) then nil
	    elseif (xml-name-start-char-p ch) then
		   (un-next-char stream ch)
		   (setf state state-dtd-!-element-type-token)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT content spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren
	   (if* (xml-space-p ch) then nil
	    elseif (xml-name-start-char-p ch) then
		   (un-next-char stream ch)
		   (setf state state-dtd-!-element-type-paren-name)
	    elseif (eq #\# ch) then
		   (setf state state-dtd-!-element-type-paren-pcd)
	    elseif (eq #\( ch) then
		   (push nil pending)
		   (setf state state-dtd-!-element-type-paren-choice-paren)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT content spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))))
	  (#.state-dtd-!-element-type-paren2
	   (if* (eq #\> ch) then 
		   ;; there only one name...
		   (setf (first contents-to-return) (first (first contents-to-return)))
		   (return)
	    elseif (eq #\* ch) then
		   (setf state state-dtd-!-element-type-paren-pcd5)
		   (setf (first contents-to-return) (nreverse (first contents-to-return)))
		   (if* (> (length (first contents-to-return)) 1) then
			   (setf (first contents-to-return)
			     (list (append (list :choice) 
					   (first contents-to-return))))
		    elseif (listp (first (first contents-to-return))) then
			   (setf (first contents-to-return) 
			     (first (first contents-to-return))))
		   (push :* (first contents-to-return))
	    elseif (eq #\? ch) then
		   (setf state state-dtd-!-element-type-paren-pcd5)
		   (setf (first contents-to-return) (nreverse (first contents-to-return)))
		   (if* (> (length (first contents-to-return)) 1) then
			   (setf (first contents-to-return)
			     (list (append (list :choice) 
					   (first contents-to-return))))
		    elseif (listp (first (first contents-to-return))) then
			   (setf (first contents-to-return) 
			     (first (first contents-to-return))))
		   (push :? (first contents-to-return))
	    elseif (eq #\+ ch) then
		   (setf state state-dtd-!-element-type-paren-pcd5)
		   (setf (first contents-to-return) (nreverse (first contents-to-return)))
		   (if* (> (length (first contents-to-return)) 1) then
			   (setf (first contents-to-return)
			     (list (append (list :choice) 
					   (first contents-to-return))))
		    elseif (listp (first (first contents-to-return))) then
			   (setf (first contents-to-return) 
			     (first (first contents-to-return))))
		   (push :+ (first contents-to-return))
	    elseif (xml-space-p ch) then
		   (setf state state-dtd-!-element-type-paren-pcd5)
		   (setf (first contents-to-return) (nreverse (first contents-to-return)))
		   (when (> (length (first contents-to-return)) 1)
		     (setf (first contents-to-return)
		       (list (append (list :\choice) 
				     (first contents-to-return)))))
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT content spec for "
				(string (first (reverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-name
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (xml-space-p ch) then
		   (push (compute-tag coll) (first pending))
		   (clear-coll coll)
		   (setf state state-dtd-!-element-type-paren-name2)
	    elseif (eq #\? ch) then
		   (push (compute-tag coll) (first pending))
		   (setf (first pending)
		     (list (push :? (first pending))))
		   (clear-coll coll)
		   (setf state state-dtd-!-element-type-paren-name2)
	    elseif (eq #\* ch) then
		   (push (compute-tag coll) (first pending))
		   (setf (first pending)
		     (list (push :* (first pending))))
		   (clear-coll coll)
		   (setf state state-dtd-!-element-type-paren-name2)
	    elseif (eq #\+ ch) then
		   (push (compute-tag coll) (first pending))
		   (setf (first pending)
		     (list (push :+ (first pending))))
		   (clear-coll coll)
		   (setf state state-dtd-!-element-type-paren-name2)
	    elseif (eq #\) ch) then
		   (push (compute-tag coll) (first pending))
		   (clear-coll coll)
		   (if* (= (length pending) 1) then
			   (push (first pending) contents-to-return)
			   (setf state state-dtd-!-element-type-paren2)
		      else ;; this is (xxx)
			   (if* (second pending) then
				   (push (first pending) (second pending))
			      else (setf (second pending) (first pending)))
			   (setf pending (rest pending))
			   (setf state state-dtd-!-element-type-paren-choice-name3)
			   )
	    elseif (eq #\, ch) then
		   (push (compute-tag coll) (first pending))
		   (push :seq pending-type)
		   (clear-coll coll)
		   (setf state state-dtd-!-element-type-paren-choice)
	    elseif (eq #\| ch) then
		   (push (compute-tag coll) (first pending))
		   (push :choice pending-type)
		   (clear-coll coll)
		   (setf state state-dtd-!-element-type-paren-choice)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT content spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-name2
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\| ch) then 
		   (push :choice pending-type)
		   (setf state state-dtd-!-element-type-paren-choice)
	    elseif (eq #\, ch) then
		   (push :seq pending-type)
		   (setf state state-dtd-!-element-type-paren-choice)
	    elseif (eq #\) ch) then
		   (if* (= (length pending) 1) then
			   (push (list (first pending)) contents-to-return)
			   (setf state state-dtd-!-element-type-paren2)
		      else (error "have to deal with nested parens (2)")
			   )
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT content spec for "
				(string (first (reverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-choice
	   (if* (xml-name-start-char-p ch) then
		   (un-next-char stream ch)
		   (setf state state-dtd-!-element-type-paren-choice-name)
	    elseif (xml-space-p ch) then nil
	    elseif (eq #\( ch) then 
		   (push nil pending)
		   (setf state state-dtd-!-element-type-paren-choice-paren)
	    elseif (eq #\) ch) then
		   (if* (= (length pending) 1) then
			   (setf (first pending) (nreverse (first pending)))
			   (if* (> (length (first pending)) 1) then
				   (push (first pending-type) (first pending))
				   (setf pending-type (rest pending-type))
			      else (setf (first pending) (first (first pending))))
			   (push (first pending) contents-to-return)
			   (setf state state-dtd-!-element-type-paren3)
		      else (setf (first pending) (nreverse (first pending)))
			   (if* (> (length (first pending)) 1) then
				   (push (first pending-type) (first pending))
				   (setf pending-type (rest pending-type))
			      else (setf (first pending) (first (first pending))))
			   (if* (second pending) then
				   (push (first pending) (second pending))
			      else (setf (second pending) (list (first pending))))
			   (setf pending (rest pending))
			   (setf state state-dtd-!-element-type-paren-choice-name3)
			   )
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT content spec for "
				(string (first (reverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-choice-paren
	   (if* (xml-name-start-char-p ch) then
		   (setf state state-dtd-!-element-type-paren-name)
		   (un-next-char stream ch)
	    elseif (eq #\( ch) then (push nil pending)
	    elseif (xml-space-p ch) then nil
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT content spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-choice-name
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (xml-space-p ch) then
		   (push (compute-tag coll) (first pending))
		   (clear-coll coll)
		   (setf state state-dtd-!-element-type-paren-choice-name2)
	    elseif (eq #\? ch) then
		   (push (list :? (compute-tag coll)) (first pending))
		   (clear-coll coll)
		   (setf state state-dtd-!-element-type-paren-choice-name2)
	    elseif (eq #\* ch) then
		   (push (list :* (compute-tag coll)) (first pending))
		   (clear-coll coll)
		   (setf state state-dtd-!-element-type-paren-choice-name2)
	    elseif (eq #\+ ch) then
		   (push (list :+ (compute-tag coll)) (first pending))
		   (clear-coll coll)
		   (setf state state-dtd-!-element-type-paren-choice-name2)
	    elseif (eq #\) ch) then
		   (push (compute-tag coll) (first pending))
		   (clear-coll coll)
		   (if* (= (length pending) 1) then
			   (setf (first pending) (nreverse (first pending)))
			   (if* (> (length (first pending)) 1) then
				   (push (first pending-type) (first pending))
				   (setf pending-type (rest pending-type))
			      else (setf (first pending) (first (first pending))))
			   (push (first pending) contents-to-return)
			   (setf state state-dtd-!-element-type-paren3)
		      else (setf (first pending) (nreverse (first pending)))
			   (push (first pending-type) (first pending))
			   (setf pending-type (rest pending-type))
			   (if* (second pending) then
				   (push (first pending) (second pending))
			      else (setf (second pending) (list (first pending))))
			   (setf pending (rest pending))
			   (setf state state-dtd-!-element-type-paren-choice-name3)
			   )
	    elseif (eq #\, ch) then
		   (push (compute-tag coll) (first pending))
		   (clear-coll coll)
		   (push :seq pending-type)
		   (setf state state-dtd-!-element-type-paren-choice)
	    elseif (eq #\| ch) then
		   (push (compute-tag coll) (first pending))
		   (clear-coll coll)
		   (push :choice pending-type)
		   (setf state state-dtd-!-element-type-paren-choice)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT content spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-choice-name2
	   (if* (eq #\| ch) then 
		   (setf state state-dtd-!-element-type-paren-choice)
	    elseif (xml-space-p ch) then nil
	    elseif (eq #\) ch) then
		   (if* (= (length pending) 1) then
			   (setf (first pending) (nreverse (first pending)))
			   (if* (> (length (first pending)) 1) then
				   (push (first pending-type) (first pending))
				   (setf pending-type (rest pending-type))
			      else (setf (first pending) (first (first pending))))
			   (push (first pending) contents-to-return)
			   (setf state state-dtd-!-element-type-paren3)
		      else (error "have to deal with nested parens")
			   )
		   (setf pending (rest pending))
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT content spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-choice-name3
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\? ch) then
		   (setf (first pending) (list :? (first pending)))
		   (setf state state-dtd-!-element-type-paren-choice-name2)
	    elseif (eq #\* ch) then
		   (setf (first pending) (list :* (first pending)))
		   (setf state state-dtd-!-element-type-paren-choice-name2)
	    elseif (eq #\+ ch) then
		   (setf (first pending) (list :+ (first pending)))
		   (setf state state-dtd-!-element-type-paren-choice-name2)
	    elseif (eq #\) ch) then
		   (if* (= (length pending) 1) then
			   (setf (first pending) (nreverse (first pending)))
			   (if* (> (length (first pending)) 1) then
				   (push (first pending-type) (first pending))
				   (setf pending-type (rest pending-type))
			      else (setf (first pending) (first (first pending))))
			   (push (first pending) contents-to-return)
			   (setf pending (rest pending))
			   (setf state state-dtd-!-element-type-paren3)
		      else (setf (first pending) (nreverse (first pending)))
			   (if* (> (length (first pending)) 1) then
				   (push (first pending-type) (first pending))
				   (setf pending-type (rest pending-type))
			      else (setf (first pending) (first (first pending))))
			   (if* (second pending) then
				   (push (first pending) (second pending))
			      else (setf (second pending) (list (first pending))))
			   (setf pending (rest pending))
			   (setf state state-dtd-!-element-type-paren-choice)
			   )
	    elseif (eq #\, ch) then
		   (push :seq pending-type)
		   (setf state state-dtd-!-element-type-paren-choice)
	    elseif (eq #\| ch) then
		   (push :choice pending-type)
		   (setf state state-dtd-!-element-type-paren-choice)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT content spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren3
	   (if* (eq #\+ ch) then
		   (setf (first contents-to-return)
		     (append (list :+) (list (first contents-to-return))))
		   (setf state state-dtd-!-element-type-paren-pcd5)
	    elseif (eq #\? ch) then
		   (setf (first contents-to-return)
		     (append (list :?) (list (first contents-to-return))))
		   (setf state state-dtd-!-element-type-paren-pcd5)
	    elseif (eq  #\* ch) then
		   (setf (first contents-to-return)
		     (append (list :*) (list (first contents-to-return))))
		   (setf state state-dtd-!-element-type-paren-pcd5)
	    elseif (xml-space-p ch) then
		   (setf state state-dtd-!-element-type-paren-pcd5)
	    elseif (eq #\> ch) then (return)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT content spec for "
				(string (first (reverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-pcd
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (xml-space-p ch) then 
		    (let ((token (compute-tag coll)))
		     (when (not (eq token :PCDATA))
		       (xml-error (concatenate 'string
				    "illegal DTD <!ELEMENT content spec for "
				    (string (first contents-to-return))
				    ": '"
				    (compute-coll-string coll)
				    "'")))
		     (clear-coll coll)
		     (push token contents-to-return))
		   (setf state state-dtd-!-element-type-paren-pcd2)
	    elseif (eq #\| ch) then 
		   (let ((token (compute-tag coll)))
		     (when (not (eq token :PCDATA))
		       (xml-error (concatenate 'string
				    "illegal DTD <!ELEMENT content spec for "
				    (string (first contents-to-return))
				    ": '"
				    (compute-coll-string coll)
				    "'")))
		     (push token contents-to-return))
		   (clear-coll coll)
		   (setf state state-dtd-!-element-type-paren-pcd3)
	    elseif (eq #\) ch) then 
		   (let ((token (compute-tag coll)))
		     (when (not (eq token :PCDATA))
		       (xml-error (concatenate 'string
				    "illegal DTD <!ELEMENT content spec for "
				    (string (first contents-to-return))
				    ": '"
				    (compute-coll-string coll)
				    "'")))
		     (push token contents-to-return))
		   (setf state state-dtd-!-element-type-paren-pcd4)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT content spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-pcd2
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\) ch) then
		   (setf state state-dtd-!-element-type-paren-pcd4)
	    elseif (eq #\| ch) then (setf state state-dtd-!-element-type-paren-pcd3)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT content spec for "
				(string (first (reverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-pcd3
	   (if* (xml-space-p ch) then nil
	    elseif (xml-name-start-char-p ch) then
		   (un-next-char stream ch)
		   (setf state state-dtd-!-element-type-paren-pcd7)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT content spec for "
				(string (first (reverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-pcd4
	   (if* (xml-space-p ch) then 
		   (setf state state-dtd-!-element-type-paren-pcd6)
	    elseif (eq #\* ch) then
		   (setf (first contents-to-return) '(:* :PCDATA))
		   (setf state state-dtd-!-element-type-paren-pcd5)
	    elseif (eq #\> ch) then (return)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD contents following <!ELEMENT content spec for "
				(string (first (reverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-pcd5
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\> ch) then (return)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD contents following <!ELEMENT content spec for "
				(string (first (reverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-pcd6
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\> ch) then (return)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD contents following <!ELEMENT content spec for "
				(string (first (reverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-pcd7
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (xml-space-p ch) then
		   (setf state state-dtd-!-element-type-paren-pcd8)
		   (let ((token (compute-tag coll)))
		     (clear-coll coll)
		     (if* (listp (first contents-to-return)) then
			     (push token (first contents-to-return))
			else (setf (first contents-to-return) 
			       (list token (first contents-to-return)))))
	    elseif (eq #\) ch) then
		   (setf state  state-dtd-!-element-type-paren-pcd9)
		   (let ((token (compute-tag coll)))
		     (clear-coll coll)
		     (if* (listp (first contents-to-return)) then
			     (push token (first contents-to-return))
			else (setf (first contents-to-return) 
			       (list token (first contents-to-return)))))
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD contents in <!ELEMENT content spec for "
				(string (first (reverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-pcd8
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\| ch) then (setf state state-dtd-!-element-type-paren-pcd3)
	    elseif (eq #\) ch) then (setf state state-dtd-!-element-type-paren-pcd9)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD contents in <!ELEMENT content spec for "
				(string (first (reverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-paren-pcd9
	   (if* (eq #\* ch) then (setf state state-dtd-!-element-type-paren-pcd5)
		   (setf (first contents-to-return) (nreverse (first contents-to-return)))
		   (when (> (length (first contents-to-return)) 1)
		     (setf (first contents-to-return)
		       (list (append (list :choice) 
				     (first contents-to-return)))))
		   (push :* (first contents-to-return))
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD contents in <!ELEMENT content spec for "
				(string (first (reverse contents-to-return)))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-element-type-token
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (xml-space-p ch) then
		   (let ((token (compute-tag coll)))
		     (when (not (or (eq token :EMPTY) (eq token :ANY)))
		       (xml-error (concatenate 'string
				    "illegal DTD <!ELEMENT content spec for "
				    (string (first contents-to-return))
				    ": '"
				    (compute-coll-string coll)
				    "'")))
		     (push token contents-to-return)
		     (setf state state-dtd-!-element-type-end))
	    elseif (eq #\> ch) then
		   (let ((token (compute-tag coll)))
		     (when (not (or (eq token :EMPTY) (eq token :ANY)))
		       (xml-error (concatenate 'string
				    "illegal DTD <!ELEMENT content spec for "
				    (string (first contents-to-return))
				    ": '"
				    (compute-coll-string coll)
				    "'")))
		     (push token contents-to-return)
		     (return))
	      else (add-to-coll coll ch)
		   (xml-error (concatenate 'string
				"illegal DTD <!ELEMENT content spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   )
	   )
	  (#.state-dtd-!-element-type-end
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\> ch) then (return)
	      else (xml-error (concatenate 'string
				"expected '>', got '"
				(string ch)
				"' in DTD <! ELEMENT "
				(string (first contents-to-return))
				" for "
				(string (second contents-to-return))))
		   ))
	  (t
	   (error "need to support dtd state:~s" state))))
      (put-back-collector entity)
      (put-back-collector coll)
      (case state
	(#.state-dtdstart
	 (when (and (null ch) (not external))
	   (xml-error "unexpected end of input while parsing DTD"))
	 (if* (null tag-to-return) then (values nil :end-dtd
						parameter-entities general-entities
						entity-buf)
	    else (error "process other return state")))
	((#.state-dtd-!-element-type-end #.state-dtd-!-element-type-token
	  #.state-dtd-!-element-type-paren-pcd4 #.state-dtd-!-element-type-paren-pcd6
	  #.state-dtd-!-element-type-paren-pcd5 #.state-dtd-!-element-type-paren2
	  #.state-dtd-!-element-type-paren3)
	 (values (append (list tag-to-return) (nreverse contents-to-return))
		 nil parameter-entities general-entities entity-buf))
	((#.state-dtd-!-attdef-decl-type #.state-dtd-!-attlist-name
	  #.state-dtd-!-attdef)
	 (values (append (list tag-to-return) contents-to-return)
		 nil parameter-entities general-entities entity-buf))
	((#.state-dtd-!-entity5 #.state-!-dtd-system3
	  #.state-!-dtd-system7 #.state-!-dtd-system4
	  #.state-!-dtd-system ;; this is actually a !NOTATION
	  #.state-dtd-?-4 ;; PI
	  #.state-dtd-comment4 ;; comment
	  )
	 (values (append (list tag-to-return) (nreverse contents-to-return))
		 nil parameter-entities general-entities entity-buf))
	(#.state-dtd-pref2
	 (values (nreverse contents-to-return) nil
	  parameter-entities general-entities entity-buf))
	(:eof
	 (when (not external)
	   (xml-error "unexpected end of input while processing DTD"))
	 (values nil :end-dtd
	  parameter-entities general-entities entity-buf))
	(t
	 (print (list tag-to-return contents-to-return))
	 (error "need to support dtd <post> state:~s" state)))
      )
    ))

(defun xml-error (text)
  (declare (optimize (speed 3) (safety 1)))
  (funcall 'error (concatenate 'string "XML not well-formed - " text)))
