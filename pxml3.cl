;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by
;; the Free Software Foundation, as clarified by the AllegroServe
;; prequel found in license-allegroserve.txt.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License is in the file
;; license-lgpl.txt that was distributed with this file.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA  02111-1307  USA
;;
;; $Id: pxml3.cl,v 1.2.2.2 2001/06/11 20:26:32 layer Exp $

(in-package :net.xml.parser)

(pxml-dribble-bug-hook "$Id: pxml3.cl,v 1.2.2.2 2001/06/11 20:26:32 layer Exp $")

(defvar *debug-dtd* nil)

(defun parse-dtd (tokenbuf
		  external external-callback)
  (declare (optimize (speed 3) (safety 1)))
  (let ((guts)
	(include-count 0))
    (loop
      (multiple-value-bind (val kind)
	  (next-dtd-token tokenbuf
			  external include-count external-callback)
	(if* (eq kind :end-dtd) then
		(return (nreverse guts))
	 elseif (eq kind :include) then
		(incf include-count)
	 elseif (eq kind :ignore) then nil
	 elseif (eq kind :include-end) then
		(if* (> include-count 0) then (decf include-count)
		   else (xml-error "unexpected ']]>' token"))
	   else (when (iostruct-do-entity tokenbuf) (push val guts)))))))

(defparameter dtd-parser-states ())

(macrolet ((def-dtd-parser-state (var val)
	       `(progn (eval-when (compile load eval) (defconstant ,var ,val))
		       (pushnew '(,val . ,var) dtd-parser-states :key #'car))))
  (def-dtd-parser-state state-dtdstart 0)
  (def-dtd-parser-state state-tokenstart 1)
  (def-dtd-parser-state state-dtd-? 2)
  (def-dtd-parser-state state-dtd-! 3)
  (def-dtd-parser-state state-dtd-comment 4)
  (def-dtd-parser-state state-dtd-!-token 5)
  (def-dtd-parser-state state-dtd-!-element 6)
  (def-dtd-parser-state state-dtd-!-element-name 7)
  (def-dtd-parser-state state-dtd-!-element-content 8)
  (def-dtd-parser-state state-dtd-!-element-type 9)
  (def-dtd-parser-state state-dtd-!-element-type-paren 10)
  (def-dtd-parser-state state-dtd-!-element-type-token 11)
  (def-dtd-parser-state state-dtd-!-element-type-end 12)
  (def-dtd-parser-state state-dtd-!-element-type-paren-name 13)
  (def-dtd-parser-state state-dtd-!-element-type-paren-pcd 14)
  (def-dtd-parser-state state-dtd-!-element-type-paren-pcd2 15)
  (def-dtd-parser-state state-dtd-!-element-type-paren-pcd3 16)
  (def-dtd-parser-state state-dtd-!-element-type-paren-pcd4 17)
  (def-dtd-parser-state state-dtd-!-element-type-paren-pcd5 18)
  (def-dtd-parser-state state-dtd-!-element-type-paren-pcd6 19)
  (def-dtd-parser-state state-dtd-!-element-type-paren-pcd7 20)
  (def-dtd-parser-state state-dtd-!-element-type-paren-pcd8 21)
  (def-dtd-parser-state state-dtd-!-element-type-paren-pcd9 22)
  (def-dtd-parser-state state-dtd-!-element-type-paren-name2 23)
  ;;(def-dtd-parser-state state-dtd-!-element-type-paren-seq 24) folded into choice
  (def-dtd-parser-state state-dtd-!-element-type-paren-choice 25)
  (def-dtd-parser-state state-dtd-!-element-type-paren2 26)
  (def-dtd-parser-state state-dtd-!-element-type-paren-choice-name 27)
  (def-dtd-parser-state state-dtd-!-element-type-paren-choice-paren 28)
  (def-dtd-parser-state state-dtd-!-element-type-paren-choice-name2 29)
  (def-dtd-parser-state state-dtd-!-element-type-paren3 30)
  (def-dtd-parser-state state-dtd-!-element-type-paren-choice-name3 31)
  (def-dtd-parser-state state-dtd-!-attlist 32)
  (def-dtd-parser-state state-dtd-!-attlist-name 33)
  (def-dtd-parser-state state-dtd-!-attdef 34)
  (def-dtd-parser-state state-dtd-!-attdef-name 35)
  (def-dtd-parser-state state-dtd-!-attdef-type 36)
  ;;(def-dtd-parser-state state-dtd-!-attdef-enumeration 37)
  (def-dtd-parser-state state-dtd-!-attdef-decl 38)
  (def-dtd-parser-state state-dtd-!-attdef-decl-type 39)
  (def-dtd-parser-state state-dtd-!-attdef-decl-value 40)
  (def-dtd-parser-state state-dtd-!-attdef-decl-value2 41)
  (def-dtd-parser-state state-dtd-!-attdef-decl-value3 42)
  (def-dtd-parser-state state-dtd-!-attdef-decl-value4 43)
  (def-dtd-parser-state state-dtd-!-attdef-decl-value5 44)
  (def-dtd-parser-state state-dtd-!-attdef-decl-value6 45)
  (def-dtd-parser-state state-dtd-!-attdef-decl-value7 46)
  (def-dtd-parser-state state-dtd-!-attdef-notation 47)
  (def-dtd-parser-state state-dtd-!-attdef-notation2 48)
  (def-dtd-parser-state state-dtd-!-attdef-notation3 49)
  (def-dtd-parser-state state-dtd-!-attdef-notation4 50)
  (def-dtd-parser-state state-dtd-!-attdef-type2 51)
  (def-dtd-parser-state state-dtd-!-entity 52)
  (def-dtd-parser-state state-dtd-!-entity2 53)
  (def-dtd-parser-state state-dtd-!-entity3 54)
  (def-dtd-parser-state state-dtd-!-entity4 55)
  (def-dtd-parser-state state-dtd-!-entity-value 56)
  (def-dtd-parser-state state-dtd-!-entity5 57)
  (def-dtd-parser-state state-dtd-!-entity6 58)
  (def-dtd-parser-state state-!-dtd-system 59)
  (def-dtd-parser-state state-!-dtd-public 60)
  (def-dtd-parser-state state-!-dtd-system2 61)
  (def-dtd-parser-state state-!-dtd-system3 62)
  (def-dtd-parser-state state-!-dtd-system4 63)
  (def-dtd-parser-state state-!-dtd-system5 64)
  (def-dtd-parser-state state-!-dtd-system6 65)
  (def-dtd-parser-state state-!-dtd-system7 66)
  (def-dtd-parser-state state-!-dtd-public2 67)
  (def-dtd-parser-state state-dtd-!-notation 68)
  (def-dtd-parser-state state-dtd-!-notation2 69)
  (def-dtd-parser-state state-dtd-!-notation3 70)
  (def-dtd-parser-state state-dtd-?-2 71)
  (def-dtd-parser-state state-dtd-?-3 72)
  (def-dtd-parser-state state-dtd-?-4 73)
  (def-dtd-parser-state state-dtd-comment2 74)
  (def-dtd-parser-state state-dtd-comment3 75)
  (def-dtd-parser-state state-dtd-comment4 76)
  (def-dtd-parser-state state-dtd-!-entity7 77)
  (def-dtd-parser-state state-dtd-!-attdef-notation5 78)
  (def-dtd-parser-state state-!-dtd-public3 79)
  (def-dtd-parser-state state-dtd-!-cond 80)
  (def-dtd-parser-state state-dtd-!-cond2 81)
  (def-dtd-parser-state state-dtd-!-include 82)
  (def-dtd-parser-state state-dtd-!-ignore 83)
  (def-dtd-parser-state state-dtd-!-include2 84)
  (def-dtd-parser-state state-dtd-!-include3 85)
  (def-dtd-parser-state state-dtd-!-include4 86)
  (def-dtd-parser-state state-dtd-!-ignore2 87)
  (def-dtd-parser-state state-dtd-!-ignore3 88)
  (def-dtd-parser-state state-dtd-!-ignore4 89)
  (def-dtd-parser-state state-dtd-!-ignore5 90)
  (def-dtd-parser-state state-dtd-!-ignore6 91)
  (def-dtd-parser-state state-dtd-!-ignore7 92)
  )

(defun next-dtd-token (tokenbuf
		       external include-count external-callback)
  (declare (:fbound parse-default-value) (optimize (speed 3) (safety 1)))
  (macrolet ((add-to-entity-buf (entity-symbol p-value)
	       `(progn
		  (push (make-tokenbuf :cur 0 :max (length ,p-value) :data ,p-value)
			(iostruct-entity-bufs tokenbuf))))

	     (un-next-char (ch)
	       `(push ,ch (iostruct-unget-char tokenbuf)))

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
	  (public-string)
	  (char-code 0)
	  (check-count 0)
	  (ignore-count 0)
	  (reference-save-state)
	  (prefp)
	  (entityp)
	  (pentityp)
	  (prev-state)
	  (ch))
      (loop
	(setq ch (get-next-char tokenbuf))
	(when *debug-dtd*
	  (format t "~@<dtd ~:Ichar: ~s ~:_state: ~s ~:_contents: ~s ~:_pending: ~s ~:_pending-type: ~s ~:_entity-names ~s~:>~%"
		  ch (or (cdr (assoc state dtd-parser-states)) state)
		  contents-to-return pending pending-type
		  (iostruct-entity-names tokenbuf)))
	(if* (null ch)
	   then (setf prev-state state)
		(setf state :eof)
		(return)		;; eof -- exit loop
		)

	(case state
	  (#.state-dtdstart
	   (if* (and (eq #\] ch)
		     external (> include-count 0)) then
		   (setf state state-dtd-!-include3)
	    elseif (and (eq #\] ch) (not external)) then (return)
	    elseif (eq #\< ch) then (setf state state-tokenstart)
	    elseif (xml-space-p ch) then nil
	    elseif (eq #\% ch) then (external-param-reference tokenbuf coll external-callback)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD characters, starting at: '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-include3
	   (if* (eq #\] ch) then (setf state state-dtd-!-include4)
	      else
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD token, starting at: ']"
				(compute-coll-string coll)
				"'"))))
	  (#.state-dtd-!-include4
	   (if* (eq #\> ch) then (return)
		else
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD token, starting at: ']]"
				(compute-coll-string coll)
				"'"))))
	  #+ignore
	  (#.state-dtd-pref
	   (if* (xml-name-start-char-p ch) then
		   (add-to-coll coll ch)
		   (setf state state-dtd-pref2)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
		   (un-next-char ch)
	    elseif (and (eq #\[ ch) external) then
		   (setf state state-dtd-!-cond)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD characters, starting at: '<!"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-cond
	   (if* (xml-space-p ch) then nil
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (eq #\I ch) then (setf state state-dtd-!-cond2)
	      else (error "this should not happen")
		   ))
	  (#.state-dtd-!-cond2
	   (if* (eq #\N ch) then (setf state state-dtd-!-include)
		   (setf check-count 2)
	    elseif (eq #\G ch) then (setf state state-dtd-!-ignore)
		   (setf check-count 2)
	      else (xml-error "<![ external DTD token not INCLUDE or IGNORE")
		   ))
	  (#.state-dtd-!-ignore
	   (if* (and (eq check-count 5) (eq ch #\E)) then
		   (setf state state-dtd-!-ignore2)
	    elseif (eq ch (elt "IGNORE" check-count)) then
		   (incf check-count)
	      else (xml-error "<![ external DTD token not INCLUDE or IGNORE")
		   ))
	  (#.state-dtd-!-ignore2
	   (if* (xml-space-p ch) then nil
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (eq #\[ ch) then (setf state state-dtd-!-ignore3)
		   (incf ignore-count)
	      else (xml-error "'[' missing after '<![Ignore'")))
	  (#.state-dtd-!-ignore3
	   (if* (eq #\< ch) then (setf state state-dtd-!-ignore4)
	    elseif (eq #\] ch) then (setf state state-dtd-!-ignore5)))
	  (#.state-dtd-!-ignore4
	   (if* (eq #\! ch) then (setf state state-dtd-!-ignore6)
	      else (un-next-char ch)
		   (setf state state-dtd-!-ignore3)))
	  (#.state-dtd-!-ignore5
	   (if* (eq #\] ch) then (setf state state-dtd-!-ignore7)
	      else (un-next-char ch)
		   (setf state state-dtd-!-ignore3)))
	  (#.state-dtd-!-ignore6
	   (if* (eq #\[ ch) then (incf ignore-count)
		   (setf state state-dtd-!-ignore3)
	      else (un-next-char ch)
		   (setf state state-dtd-!-ignore3)))
	  (#.state-dtd-!-ignore7
	   (if* (eq #\> ch) then (decf ignore-count)
		   (when (= ignore-count 0) (return))
	      else (un-next-char ch)
		   (setf state state-dtd-!-ignore3)))
	  (#.state-dtd-!-include
	   (if* (and (eq check-count 6) (eq ch #\E)) then
		   (setf state state-dtd-!-include2)
	    elseif (eq ch (elt "INCLUD" check-count)) then
		   (incf check-count)
	      else (xml-error "<![ external DTD token not INCLUDE or IGNORE")
		   ))
	  (#.state-dtd-!-include2
	   (if* (xml-space-p ch) then nil
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (eq #\[ ch) then (return)
	      else (xml-error "'[' missing after '<![INCLUDE'")))
	  (#.state-dtd-comment
	   (if* (eq #\- ch)
	      then (setf state state-dtd-comment2)
		   (setf tag-to-return :comment)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal token following '--' comment terminator, starting at '--"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-token
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD characters, starting at: '<!"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-notation
	   (if* (xml-space-p ch) then nil
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-name-start-char-p ch) then
		   (add-to-coll coll ch)
		   (setf state state-dtd-!-notation2)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD characters, starting at: '<!NOTATION "
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-notation2
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-space-p ch) then
		   (push (compute-tag coll) contents-to-return)
		   (clear-coll coll)
		   (setf state state-dtd-!-notation3)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!NOTATION name: "
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-notation3
	   (if* (xml-space-p ch) then nil
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-name-char-p ch) then
		   (add-to-coll coll ch)
		   (setf state state-dtd-!-entity6)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD characters, starting at: '<!ENTITY "
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-entity2
	   (if* (xml-space-p ch) then (setf state state-dtd-!-entity7)
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-space-p ch) then
		   (push (compute-tag coll) contents-to-return)
		   (setf contents-to-return
		     (nreverse contents-to-return))
		   (clear-coll coll)
		   (setf state state-dtd-!-entity4)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ENTITY name: "
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-entity4
	   (if* (xml-space-p ch) then nil
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (or (eq #\' ch) (eq #\" ch)) then
		   (setf value-delim ch)
		   (setf state state-dtd-!-entity-value)
	    elseif (xml-name-start-char-p ch) then
		   (add-to-coll coll ch)
		   (setf state state-dtd-!-entity6)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	      else
		   (when (not (xml-space-p ch))
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-name-start-char-p ch) then
		   (add-to-coll coll ch)
		   (setf state state-dtd-!-entity3)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ENTITY % name: "
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-!-dtd-public
	   (if* (xml-space-p ch) then nil
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
		   (push (setf public-string
			   (normalize-public-value
			    (compute-coll-string coll))) contents-to-return)
		   (clear-coll coll)
		   (setf state state-!-dtd-public3)
	    elseif (pub-id-char-p ch) then (add-to-coll coll ch)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error
		    (concatenate 'string
		      "illegal character in string: '"
		      (compute-coll-string coll) "'"))
		   ))
	  (#.state-!-dtd-public3
	   (if* (xml-space-p ch) then (setf state state-!-dtd-system)
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (and (not entityp)
			(eq #\> ch)) then
		   (setf state state-!-dtd-system)
		   (return)
	      else
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error
		    (concatenate 'string
		      "Expected space before: '"
		      (compute-coll-string coll) "'"))
		   ))
	  (#.state-!-dtd-system
	   (if* (xml-space-p ch) then nil
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (or (eq #\" ch) (eq #\' ch)) then
		   (setf state state-!-dtd-system2)
		   (setf value-delim ch)
	    elseif (and (not entityp)
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
		   (let ((entity-symbol (first (last contents-to-return)))
			 (system-string (compute-coll-string coll)))
		     (if* pentityp then
			     (when (not (assoc entity-symbol (iostruct-parameter-entities tokenbuf)))
			       (setf (iostruct-parameter-entities tokenbuf)
				 (acons entity-symbol (list (parse-uri system-string)
							    tag-to-return
							    public-string)
					(iostruct-parameter-entities tokenbuf)))
			       )
			else
			    (when (not (assoc entity-symbol (iostruct-general-entities tokenbuf)))
			       (setf (iostruct-general-entities tokenbuf)
				 (acons entity-symbol (list (parse-uri system-string)
							    tag-to-return
							    public-string
							    )
					(iostruct-general-entities tokenbuf)))
			       (when (not (assoc entity-symbol (iostruct-general-entities tokenbuf)))
			       (setf (iostruct-general-entities tokenbuf)
				 (acons entity-symbol (list (parse-uri system-string)
							    tag-to-return
							    public-string
							    )
					(iostruct-general-entities tokenbuf))))
			       )
			     )
		     (push system-string contents-to-return))
		   (clear-coll coll)
		   (setf state state-!-dtd-system3)
	      else (add-to-coll coll ch)))
	  (#.state-!-dtd-system3
	   (if* (xml-space-p ch) then (setf state state-!-dtd-system4)
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (eq #\> ch) then (return)
	      else
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (and (not pentityp) (xml-name-start-char-p ch)) then
		   (add-to-coll coll ch)
		   (setf state state-!-dtd-system5)
	    elseif (eq #\> ch) then (return)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-space-p ch) then
		   (let ((token (compute-tag coll)))
		     (when (not (eq :NDATA token))
		       (dotimes (i 15)
			 (add-to-coll coll ch)
			 (setq ch (get-next-char tokenbuf))
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
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-name-start-char-p ch) then
		   (add-to-coll coll ch)
		   (setf state state-!-dtd-system7)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
		     (setq ch (get-next-char tokenbuf))
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
		   (let ((tmp (compute-coll-string coll)))
		     (when (> (length tmp) 0)
		       (when (null (first pending)) (setf pending (rest pending)))
		       (push tmp pending)))
		   (if* (> (length pending) 1) then
			   (push (nreverse pending) contents-to-return)
		      else (push (first pending) contents-to-return))
		   (setf pending (list nil))
		   (setf state state-dtd-!-entity5)
		   (clear-coll coll)
		   (if* pentityp then
			   (when (not (assoc (third contents-to-return)
					     (iostruct-parameter-entities tokenbuf)))
			     (setf (iostruct-parameter-entities tokenbuf)
			       (acons (third contents-to-return)
				      (first contents-to-return)
				      (iostruct-parameter-entities tokenbuf))))
		      else
			   (when (not (assoc (second contents-to-return)
					     (iostruct-general-entities tokenbuf)))
			     (setf (iostruct-general-entities tokenbuf)
			       (acons (second contents-to-return)
				      (first contents-to-return)
				      (iostruct-general-entities tokenbuf)))))
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
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (eq #\> ch) then (return)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
		   (un-next-char ch)
	    elseif (xml-space-p ch) then nil
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD characters, starting at: '<!ATTLIST "
				(compute-coll-string coll)
				"'"))))
	  (#.state-dtd-!-attlist-name
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-space-p ch) then
		   (push (compute-tag coll *package*)
			 contents-to-return)
		   (clear-coll coll)
		   (setf state state-dtd-!-attdef)
	    elseif (eq #\> ch) then
		   (push (compute-tag coll *package*)
			 contents-to-return)
		   (clear-coll coll)
		   (return)
	      else (push (compute-tag coll)
			 contents-to-return)
		   (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-name-start-char-p ch) then
		   (un-next-char ch)
		   (setf state state-dtd-!-attdef-name)
	    elseif (eq #\> ch) then (return)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-space-p ch) then
		   (setf (first pending) (compute-tag coll *package*))
		   (clear-coll coll)
		   (setf state state-dtd-!-attdef-type)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	      else (un-next-char ch)
		   ;; let next state do all other checking
		   (setf state state-dtd-!-attdef-type2)))
	  (#.state-dtd-!-attdef-type2
	   ;; can only be one of a few tokens, but wait until token built to check
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (and (eq #\( ch) (= 0 (length (compute-coll-string coll)))) then
		   (push (list :enumeration) pending)
		   (setf state state-dtd-!-attdef-notation2)
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
			 (setq ch (get-next-char tokenbuf))
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
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (eq #\( ch) then (setf state state-dtd-!-attdef-notation2)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-name-start-char-p ch) then
		   (setf state state-dtd-!-attdef-notation3)
		   (add-to-coll coll ch)
	    elseif (and (xml-name-char-p ch) (listp (first pending))
			(eq :enumeration (first (reverse (first pending))))) then
		   (setf state state-dtd-!-attdef-notation3)
		   (add-to-coll coll ch)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
		   ;;(setf state state-dtd-!-attdef-decl)
		   (setf state state-dtd-!-attdef-notation5)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD <!ATTLIST type spec for "
				(string (first contents-to-return))
				": '"
				(compute-coll-string coll)
				"'"))
		   ))
	  (#.state-dtd-!-attdef-notation5
	   (if* (xml-space-p ch) then (setf state state-dtd-!-attdef-decl)
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	      else
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error
		    (concatenate 'string
		      "Expected space before: '"
		      (compute-coll-string coll) "'"))))
	  (#.state-dtd-!-attdef-notation4
	   (if* (xml-space-p ch) then nil
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-name-char-p ch) then (add-to-coll coll ch)
		   (setf state state-dtd-!-attdef-notation3)
	    elseif (eq #\| ch) then (setf state state-dtd-!-attdef-notation2)
	    elseif (eq #\) ch) then (setf state state-dtd-!-attdef-decl)
		   (setf (first pending) (nreverse (first pending)))
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
		   #-ignore
		   (push (first (parse-default-value (list (compute-coll-string coll))
					      tokenbuf external-callback))
			 pending)
		   #+ignore
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
		     (setq ch (get-next-char tokenbuf))
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
	   (if* (and (not prefp) (eq #\# ch))
	      then (setf state state-dtd-!-attdef-decl-value4)
	    elseif (xml-name-start-char-p ch)
	      then (setf state state-dtd-!-attdef-decl-value5)
		   (when (not prefp) (add-to-coll coll #\&))
		   (un-next-char ch)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
		   (un-next-char ch)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
		   (when (not prefp) (add-to-coll coll ch))
	    elseif (eq #\; ch)
	      then
		   (if* (not prefp) then (add-to-coll coll ch)
		    elseif (not external) then
			   (xml-error
			    (concatenate 'string
			      "internal dtd subset cannot reference parameter entity within a token; entity: "
			      (compute-coll-string entity)))
		      else
			   (let* ((entity-symbol (compute-tag entity))
				  (p-value
				   (assoc entity-symbol (iostruct-parameter-entities tokenbuf))))
			     (clear-coll entity)
			     (if* (and (iostruct-do-entity tokenbuf)
				       (setf p-value
					 (assoc entity-symbol
						(iostruct-parameter-entities tokenbuf)))) then
				     (setf p-value (rest p-value))
				     (when (member entity-symbol (iostruct-entity-names tokenbuf))
					 (xml-error (concatenate 'string
						      "entity:"
						      (string entity-symbol)
						      " in recursive reference")))
				     (push entity-symbol (iostruct-entity-names tokenbuf))
				     (if* (stringp p-value) then
					     (dotimes (i (length p-value))
					       (add-to-coll coll (schar p-value i)))
				      elseif p-value then
					     (if* (null external-callback) then
						     (setf (iostruct-do-entity tokenbuf) nil)
						else
						     (let ((count 0) (string "<?xml ") last-ch
							   save-ch save-unget
							   (tmp-count 0)
							   (entity-stream
							    (apply external-callback p-value)))
						       (when entity-stream
							 (let ((tmp-buf (get-tokenbuf)))
							   (setf (tokenbuf-stream tmp-buf)
							     entity-stream)
							   (setf save-unget
							     (iostruct-unget-char tokenbuf))
							   (setf (iostruct-unget-char tokenbuf) nil)
							   (unicode-check entity-stream tokenbuf)
							   (when (iostruct-unget-char tokenbuf)
							     (setf save-ch (first (iostruct-unget-char tokenbuf))))
							   (setf (iostruct-unget-char tokenbuf) save-unget)
							   (loop
							     (let ((cch
								    (if* save-ch
								       then
									    (let ((s2 save-ch))
									      (setf save-ch nil)
									      s2)
								       else
									    (next-char
									     tmp-buf
									     (iostruct-read-sequence-func
									      tokenbuf)))))
							       (when (null cch) (return))
							       (when *debug-dtd*
								 (format t "dtd-char: ~s~%" cch))
							       (if* (< count 0) then
								       (if* (and (eq last-ch #\?)
										 (eq cch #\>)) then
									       (setf count 6)
									  else (setf last-ch cch))
								elseif (< count 6) then
								       (when (and (= count 5)
									       (xml-space-p cch))
									 (setf cch #\space))
								       (if* (not (eq cch
										    (schar string count)
										    )) then
									       (loop
										 (when (= tmp-count count)
										   (return))
										 (add-to-coll coll
											      (schar string
												     tmp-count))
										 (incf tmp-count))
									       (add-to-coll coll cch)
									       (setf count 10)
									  else (incf count))
								elseif (= count 6) then
								       (dotimes (i 6)
									 (add-to-coll coll (schar string i)))
								       (setf count 10)
								  else (add-to-coll coll cch))))
							   (setf (iostruct-entity-names tokenbuf)
							     (rest (iostruct-entity-names tokenbuf)))
							   (close entity-stream)
							   (put-back-tokenbuf tmp-buf)))))
					     )
				     (setf state state-dtdstart)
				else nil
				     )))
		   (setf state reference-save-state)
	      else (let ((tmp (compute-coll-string entity)))
		     (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
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
		       (setq ch (get-next-char tokenbuf))
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
		     (setq state reference-save-state)
	      elseif (<= (char-code #\0) code (char-code #\9))
		then (setf char-code (+ (* char-code 10) (- code (char-code #\0))))
		else (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (or (xml-space-p ch) (eq #\> ch)) then
		   (let ((token (compute-tag coll)))
		     (when (and (not (eq :REQUIRED token))
				(not (eq :IMPLIED token))
				(not (eq :FIXED token)))
		       (dotimes (i 15)
			 (add-to-coll coll ch)
			 (setq ch (get-next-char tokenbuf))
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
				 (setq ch (get-next-char tokenbuf))
				 (if* (null ch)
				    then (return)))
			       (xml-error (concatenate 'string
					    "illegal DTD <!ATTLIST type spec for "
					    (string (first contents-to-return))
					    ": '"
					    (compute-coll-string coll)
					    "'")))
			     (setf state state-dtd-!-attdef-decl-value2)
		      elseif (eq #\> ch) then
			     (setf contents-to-return
			       (append contents-to-return (list (nreverse pending))))
			     (return)
			else (setf contents-to-return
			       (append contents-to-return (list (nreverse pending))))
			     (setf pending (list nil))
			     (setf state state-dtd-!-attdef)))
		   (clear-coll coll)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	       elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	       elseif (or (eq #\' ch) (eq #\" ch)) then
		      (setf value-delim ch)
		      (setf state state-dtd-!-attdef-decl-value)
		 else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-name-start-char-p ch) then (setf state state-dtd-!-element-name)
		   (un-next-char ch)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal DTD characters, starting at: '<!ELEMENT "
				(compute-coll-string coll)
				"'"))))
	  (#.state-dtd-!-element-name
	   (if* (xml-name-char-p ch) then (add-to-coll coll ch)
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-space-p ch) then
		   (push (compute-tag coll)
			 contents-to-return)
		   (clear-coll coll)
		   (setf state state-dtd-!-element-type)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-name-start-char-p ch) then
		   (un-next-char ch)
		   (setf state state-dtd-!-element-type-token)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-name-start-char-p ch) then
		   (un-next-char ch)
		   (setf state state-dtd-!-element-type-paren-name)
	    elseif (eq #\# ch) then
		   (setf state state-dtd-!-element-type-paren-pcd)
	    elseif (eq #\( ch) then
		   (push nil pending)
		   (setf state state-dtd-!-element-type-paren-choice-paren)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-space-p ch) then
		   (setf state state-dtd-!-element-type-paren-pcd5)
		   (setf (first contents-to-return) (nreverse (first contents-to-return)))
		   (when (> (length (first contents-to-return)) 1)
		     (setf (first contents-to-return)
		       (list (append (list :\choice)
				     (first contents-to-return)))))
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
		   (when (and (first pending) (not (eq :seq (first pending-type))))
		     (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error (concatenate 'string
				"illegal '|' and ',' mix starting at '"
				(compute-coll-string coll)
				"'")))
		   (push (compute-tag coll) (first pending))
		   (push :seq pending-type)
		   (clear-coll coll)
		   (setf state state-dtd-!-element-type-paren-choice)
	    elseif (eq #\| ch) then
		   (when (and (first pending) (not (eq :choice (first pending-type))))
		     (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error (concatenate 'string
				"illegal '|' and ',' mix starting at '"
				(compute-coll-string coll)
				"'")))
		   (push (compute-tag coll) (first pending))
		   (push :choice pending-type)
		   (clear-coll coll)
		   (setf state state-dtd-!-element-type-paren-choice)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (eq #\| ch) then
		   (when (and (rest (first pending)) (not (eq :choice (first pending-type))))
		     (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error (concatenate 'string
				"illegal '|' and ',' mix starting at '"
				(compute-coll-string coll)
				"'")))
		   (push :choice pending-type)
		   (setf state state-dtd-!-element-type-paren-choice)
	    elseif (eq #\, ch) then
		   (when (and (rest (first pending)) (not (eq :seq (first pending-type))))
		     (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error (concatenate 'string
				"illegal '|' and ',' mix starting at '"
				(compute-coll-string coll)
				"'")))
		   (push :seq pending-type)
		   (setf state state-dtd-!-element-type-paren-choice)
	    elseif (eq #\) ch) then
		   (if* (= (length pending) 1) then
			   (push (list (first pending)) contents-to-return)
			   (setf state state-dtd-!-element-type-paren2)
		      else (setf pending (reverse (rest (reverse pending))))
			   )
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
		   (un-next-char ch)
		   (setf state state-dtd-!-element-type-paren-choice-name)
	    elseif (xml-space-p ch) then nil
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
		     (setq ch (get-next-char tokenbuf))
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
		   (un-next-char ch)
	    elseif (eq #\( ch) then (push nil pending)
	    elseif (xml-space-p ch) then nil
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
			      else (setf (second pending)
				     ;; (list (first pending)) ;2001-03-22
				     (first pending) ;2001-03-22
				     ))
			   (setf pending (rest pending))
			   (setf state state-dtd-!-element-type-paren-choice-name3)
			   )
	    elseif (eq #\, ch) then
		   (when (and (first pending) (not (eq :seq (first pending-type))))
		     (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error (concatenate 'string
				"illegal '|' and ',' mix starting at '"
				(compute-coll-string coll)
				"'")))
		   (push (compute-tag coll) (first pending))
		   (clear-coll coll)
		   (push :seq pending-type)
		   (setf state state-dtd-!-element-type-paren-choice)
	    elseif (eq #\| ch) then
		   (when (and (first pending) (not (eq :choice (first pending-type))))
		     (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error (concatenate 'string
				"illegal '|' and ',' mix starting at '"
				(compute-coll-string coll)
				"'")))
		   (push (compute-tag coll) (first pending))
		   (clear-coll coll)
		   (push :choice pending-type)
		   (setf state state-dtd-!-element-type-paren-choice)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	   (if* (eq #\| ch)
		   ;; begin changes 2001-03-22
	      then (setf state state-dtd-!-element-type-paren-choice)
		   (push :choice pending-type)
	    elseif (eq #\, ch)
	      then (setf state state-dtd-!-element-type-paren-choice)
		   (push :seq pending-type)
		   ;; end changes 2001-03-22
	    elseif (xml-space-p ch) then nil
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
			   (push (first pending-type) (first pending))
			   (setf pending-type (rest pending-type))
			   (if* (second pending) then
				   (push (first pending) (second pending))
			      else (setf (second pending) (list (first pending))))
			   (setf state state-dtd-!-element-type-paren-choice-name3)
			   )
		   (setf pending (rest pending))
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
		   (when (and (rest (first pending)) (not (eq :seq (first pending-type))))
		     (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error (concatenate 'string
				"illegal '|' and ',' mix starting at '"
				(compute-coll-string coll)
				"'")))
		   (push :seq pending-type)
		   (setf state state-dtd-!-element-type-paren-choice)
	    elseif (eq #\| ch) then
		   (when (and (rest (first pending)) (not (eq :choice (first pending-type))))
		     (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error (concatenate 'string
				"illegal '|' and ',' mix starting at '"
				(compute-coll-string coll)
				"'")))
		   (push :choice pending-type)
		   (setf state state-dtd-!-element-type-paren-choice)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-space-p ch) then
		   (setf state state-dtd-!-element-type-paren-pcd5)
	    elseif (eq #\> ch) then (return)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (eq #\) ch) then
		   (setf state state-dtd-!-element-type-paren-pcd4)
	    elseif (eq #\| ch) then (setf state state-dtd-!-element-type-paren-pcd3)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (xml-name-start-char-p ch) then
		   (un-next-char ch)
		   (setf state state-dtd-!-element-type-paren-pcd7)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (eq #\* ch) then
		   (setf (first contents-to-return) '(:* :PCDATA))
		   (setf state state-dtd-!-element-type-paren-pcd5)
	    elseif (eq #\> ch) then (return)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (eq #\> ch) then (return)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (eq #\> ch) then (return)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
	    elseif (eq #\| ch) then (setf state state-dtd-!-element-type-paren-pcd3)
	    elseif (eq #\) ch) then (setf state state-dtd-!-element-type-paren-pcd9)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
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
		     (setq ch (get-next-char tokenbuf))
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
	    elseif (and external (eq #\% ch)) then
		   (external-param-reference tokenbuf coll external-callback)
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
	 (if* (null tag-to-return) then (values nil :end-dtd)
	    else (error "process other return state")))
	((#.state-dtd-!-element-type-end #.state-dtd-!-element-type-token
	  #.state-dtd-!-element-type-paren-pcd4 #.state-dtd-!-element-type-paren-pcd6
	  #.state-dtd-!-element-type-paren-pcd5 #.state-dtd-!-element-type-paren2
	  #.state-dtd-!-element-type-paren3)
	 (values (append (list tag-to-return) (nreverse contents-to-return))
		 nil))
	((#.state-dtd-!-attdef-decl-type #.state-dtd-!-attlist-name
	  #.state-dtd-!-attdef)
	 (values (append (list tag-to-return) contents-to-return)
		 nil))
	((#.state-dtd-!-entity5 #.state-!-dtd-system3
	  #.state-!-dtd-system7 #.state-!-dtd-system4
	  #.state-!-dtd-system ;; this is actually a !NOTATION
	  #.state-dtd-?-4 ;; PI
	  #.state-dtd-comment4 ;; comment
	  )
	 (let ((ret (append (list tag-to-return) (nreverse contents-to-return))))
	   (values ret
		   nil)))
	#+ignore
	(#.state-dtd-pref2
	 (values (nreverse contents-to-return) nil))
	(#.state-dtd-!-include2
	 (values nil :include))
	(#.state-dtd-!-include4
	 (values nil :include-end))
	(#.state-dtd-!-ignore7
	 (values nil :ignore))
	(:eof
	 (if* (not external) then
		 (xml-error "unexpected end of input while processing DTD internal subset")
	  elseif (or (> include-count 0) (not (eq prev-state state-dtdstart))) then
		 (xml-error "unexpected end of input while processing external DTD"))
	 (values nil :end-dtd))
	(t
	 (print (list tag-to-return contents-to-return))
	 (error "need to support dtd <post> state:~s" state)))
      )
    ))

(defun external-param-reference (tokenbuf old-coll external-callback)
  (declare (:fbound next-token) (ignorable old-coll) (optimize (speed 3) (safety 1)))
  (setf (iostruct-seen-parameter-reference tokenbuf) t)
  (macrolet ((add-to-entity-buf (entity-symbol p-value)
	       `(progn
		  (push (make-tokenbuf :cur 0 :max (length ,p-value) :data ,p-value)
			(iostruct-entity-bufs tokenbuf))))
	     (clear-coll (coll)
	       `(setf (collector-next ,coll) 0))
	     (un-next-char (ch)
	       `(push ,ch (iostruct-unget-char tokenbuf)))
	     (add-to-coll (coll ch)
	       `(let ((.next. (collector-next ,coll)))
		  (if* (>= .next. (collector-max ,coll))
		     then (grow-and-add ,coll ,ch)
		     else (setf (schar (collector-data ,coll) .next.)
			    ,ch)
			  (setf (collector-next ,coll) (1+ .next.))))))
    (let ((ch (get-next-char tokenbuf))
	  (coll (get-collector))
	  p-value entity-symbol)
      (add-to-coll coll ch)
      (when (not (xml-name-start-char-p ch))
	(dotimes (i 15)
	  (add-to-coll coll ch)
	  (setq ch (get-next-char tokenbuf))
	  (if* (null ch)
	     then (return)))
	(xml-error (concatenate 'string
		     "Illegal DTD parameter entity name starting at: "
		     (compute-coll-string coll))))
      (loop
	(setf ch (get-next-char tokenbuf))
	(if* (eq #\; ch) then
		(setf entity-symbol (compute-tag coll))
		(clear-coll coll)
		#+ignore (format t "entity symbol: ~s entities: ~s match: ~s~%"
				 entity-symbol (iostruct-parameter-entities tokenbuf)
				 (assoc entity-symbol
					(iostruct-parameter-entities tokenbuf)))
		(if* (and (iostruct-do-entity tokenbuf)
			  (setf p-value
			    (assoc entity-symbol
				   (iostruct-parameter-entities tokenbuf)))) then
			(setf p-value (rest p-value))
			(when (member entity-symbol (iostruct-entity-names tokenbuf))
			  (xml-error (concatenate 'string
				       "entity:"
				       (string entity-symbol)
				       " in recursive reference")))
			(push entity-symbol (iostruct-entity-names tokenbuf))
			(if* (stringp p-value) then
				(setf p-value (concatenate 'string " " p-value " "))
				(add-to-entity-buf entity-symbol p-value)
			 elseif (null external-callback) then
				(setf (iostruct-do-entity tokenbuf) nil)
			 elseif p-value then
				(let ((entity-stream (apply external-callback p-value)))
				  (when entity-stream
				    (let ((entity-buf (get-tokenbuf)))
				      (setf (tokenbuf-stream entity-buf) entity-stream)
				      (unicode-check entity-stream tokenbuf)
				      (add-to-entity-buf entity-symbol " ")
				      (push entity-buf
					    (iostruct-entity-bufs tokenbuf))
				      (let ((count 0) cch
					    (string "<?xml "))
					(if* (dotimes (i (length string) t)
					       (setf cch (get-next-char tokenbuf))
					       (when (and (= i 5)
							  (xml-space-p cch))
						 (setf cch #\space))
					       (when (not (eq cch
							      (schar string count)))
						 (return nil))
					       (incf count)) then
						(setf count 5)
						(loop
						  (when (< count 0) (return))
						  (un-next-char (schar string count))
						  (decf count))
						;; swallow <?xml token
						(next-token tokenbuf external-callback nil)
					   else
						(un-next-char cch)
						(decf count)
						(loop
						  (when (< count 0) (return))
						  (un-next-char (schar string count))
						  (decf count))))
				      (push #\space (iostruct-unget-char tokenbuf))
				      )
				    )))
		   else (xml-error
			 (concatenate 'string
			   (string entity-symbol)
			   " parameter entity referenced but not declared"))
			)
		(put-back-collector coll)
		(return)
	 elseif (xml-name-char-p ch) then (add-to-coll coll ch)
	   else
		(dotimes (i 15)
		  (add-to-coll coll ch)
		  (setq ch (get-next-char tokenbuf))
		  (if* (null ch)
		     then (return)))
		(xml-error (concatenate 'string
			     "Illegal DTD parameter entity name stating at: "
			     (compute-coll-string coll))))))))


