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

;; $Id: pxml2.cl,v 1.7 2000/12/05 21:26:50 sdj Exp $

;; Change Log 
;;
;; 10/14/00 add namespace support

(in-package :net.xml.parser)

;; state titles can be better chosen and explained

(defvar *debug-xml* nil)

(defmethod parse-xml ((str string) &key external-callback general-entities parameter-entities
					content-only uri-to-package)
  (declare (optimize (speed 3) (safety 1)))
  (parse-xml (make-string-input-stream str) :external-callback external-callback
	     :general-entities general-entities
	     :parameter-entities parameter-entities :content-only content-only
	     :uri-to-package uri-to-package))

(defmethod parse-xml ((p stream) &key external-callback general-entities 
				      parameter-entities content-only uri-to-package)
  (declare (optimize (speed 3) (safety 1)))
  (pxml-internal0 p nil external-callback general-entities parameter-entities content-only
		  uri-to-package))

(eval-when (compile load eval)
  (defconstant state-docstart 0) ;; looking for XMLdecl, Misc, doctypedecl, 1st element
  (defconstant state-docstart-misc 1) ;; looking for Misc, doctypedecl, 1st element
  (defconstant state-docstart-misc2 2) ;; looking for Misc, 1st element
  (defconstant state-element-done 3) ;; looking for Misc
  (defconstant state-element-contents 4) ;; looking for element content
  )

(defun all-xml-whitespace-p (val)
  (dotimes (i (length val) t)
    (when (not (xml-space-p (elt val i))) (return nil))))

(defun pxml-internal0 (p read-sequence-func external-callback
		      general-entities parameter-entities content-only uri-to-package)
  (declare (optimize (speed 3) (safety 1)))
  (let ((tokenbuf (make-iostruct :tokenbuf (get-tokenbuf)
				 :do-entity t
				 :read-sequence-func read-sequence-func)))
    ;; set up stream right
    (setf (tokenbuf-stream (iostruct-tokenbuf tokenbuf)) p)
    ;; set up user specified entities
    (setf (iostruct-parameter-entities tokenbuf) parameter-entities)
    (setf (iostruct-general-entities tokenbuf) general-entities)
    (setf (iostruct-uri-to-package tokenbuf) uri-to-package)
    ;; look for Unicode file
    (unicode-check p tokenbuf)
    (unwind-protect
	(values (pxml-internal tokenbuf external-callback content-only)
		(iostruct-uri-to-package tokenbuf))
      (dolist (entity-buf (iostruct-entity-bufs tokenbuf))
	(when (streamp (tokenbuf-stream entity-buf))
	  (close (tokenbuf-stream entity-buf))
	  (put-back-tokenbuf entity-buf))))
    ))

(defun pxml-internal (tokenbuf external-callback content-only)
  (declare (optimize (speed 3) (safety 1)))
  (let ((state state-docstart)
	(guts)
	(pending)
	(attlist-data)
	(public-string)
	(system-string)
	(entity-open-tags)
	)
    
    (loop
      (multiple-value-bind (val kind kind2) 
	  (next-token tokenbuf external-callback attlist-data)
	(when *debug-xml*
	  (format t "val: ~s kind: ~s kind2: ~s state: ~s~%" 
		  val kind kind2 state))
	(case state
	  (#.state-docstart
	   (if* (and (listp val) (eq :xml (first val)) (eq kind :xml) (eq kind2 :end-tag))
	      then
		   (check-xmldecl val tokenbuf)
		   (when (not content-only) (push val guts))
		   (setf state state-docstart-misc)
	    elseif (eq kind :comment)
	      then
		   (when (not content-only) (push val guts))
		   (setf state state-docstart-misc)
	    elseif (and (listp val) (eq :DOCTYPE (first val)))
	      then
		   (if* (eq (third val) :SYSTEM) then
			   (setf system-string (fourth val))
			   (setf val (remove (third val) val))
			   (setf val (remove (third val) val))
		    elseif (eq (third val) :PUBLIC) then
			   (setf public-string (normalize-public-value (fourth val)))
			   (setf system-string (fifth val))
			   (setf val (remove (third val) val))
			   (setf val (remove (third val) val))
			   (setf val (remove (third val) val)))
		   (when system-string
		     (if* external-callback then
			     (let ((ext-stream (apply external-callback
						      (list (parse-uri system-string)
							    :DOCTYPE
							    public-string
							    ))))
			       (when ext-stream
				 (let (ext-io (entity-buf (get-tokenbuf)))
				   (setf (tokenbuf-stream entity-buf) ext-stream)
				   (setf ext-io (make-iostruct :tokenbuf entity-buf
							       :do-entity 
							       (iostruct-do-entity tokenbuf)
							       :read-sequence-func 
							       (iostruct-read-sequence-func tokenbuf)))
				   (unicode-check ext-stream ext-io)
				   (setf (iostruct-parameter-entities ext-io) 
				     (iostruct-parameter-entities tokenbuf))
				   (setf (iostruct-general-entities ext-io)
				     (iostruct-general-entities tokenbuf))
				   (unwind-protect
				       (setf val (append val 
							 (list (append 
								(list :external)
								(parse-dtd
								 ext-io	    
								 t external-callback)))))
				     (setf (iostruct-seen-any-dtd tokenbuf) t)
				     (setf (iostruct-seen-external-dtd tokenbuf) t)
				     (setf (iostruct-seen-parameter-reference tokenbuf)
				       (iostruct-seen-parameter-reference ext-io))
				     (setf (iostruct-general-entities tokenbuf)
				       (iostruct-general-entities ext-io))
				     (setf (iostruct-parameter-entities tokenbuf)
				       (iostruct-parameter-entities ext-io))
				     (setf (iostruct-do-entity tokenbuf)
				       (iostruct-do-entity ext-io))
				     (dolist (entity-buf2 (iostruct-entity-bufs ext-io))
				       (when (streamp (tokenbuf-stream entity-buf2))
					 (close (tokenbuf-stream entity-buf2))
					 (put-back-tokenbuf entity-buf2)))
				     (close (tokenbuf-stream entity-buf))
				     (put-back-tokenbuf entity-buf))
				   )))
			else
			     (setf (iostruct-do-entity tokenbuf) nil)))
		   (setf attlist-data
		     (process-attlist (rest (rest val)) attlist-data))
		   (when (not content-only) (push val guts))
		   (setf state state-docstart-misc2)
	    elseif (eq kind :pi)
	      then
		   (push val guts)
		   (setf state state-docstart-misc)
	    elseif (eq kind :pcdata)
	      then
		   (when (or (not kind2) (not (all-xml-whitespace-p val)))
		     (if* (not kind2) then
			     (xml-error "An entity reference occured where only whitespace or the first element may occur")
			else
			     (xml-error (concatenate 'string
					  "unrecognized content '"
					  (subseq val 0 (min (length val) 40)) "'"))))
		   (setf state state-docstart-misc)
	    elseif (or (symbolp val)
		       (and (listp val) (symbolp (first val))))
	      then
		   (when (eq kind :start-tag)
		     (setf val (add-default-values val attlist-data)))
		   (if* (and (eq kind :start-tag) (eq kind2 :end-tag))
		      then (push (list val) guts)
			   (setf state state-element-done)
		    elseif (eq kind :start-tag)
		      then (push (list val) pending)
			   ;;(format t "pending: ~s guts: ~s <1>~%" pending guts)
			   (when (iostruct-entity-bufs tokenbuf)
			     (push (if (symbolp val) val (first val)) entity-open-tags))
			   (setf state state-element-contents)
		      else (xml-error (concatenate 'string
						   "encountered token at illegal syntax position: '"
						   (string kind) "'"
						   (if* (null guts) then
							   " at start of contents"
						      else 
							   (concatenate 'string
							     " following: '"
							     (format nil "~s" (first guts))
							     "'")))))
	      else
		   (print (list val kind kind2))
		   (break "need to check for other allowable docstarts")))
	  (#.state-docstart-misc2
	   (if* (eq kind :pcdata)
	      then
		   (when (or (not kind2) (not (all-xml-whitespace-p val)))
		     (if* (not kind2) then
			     (xml-error "An entity reference occured where only whitespace or the first element may occur")
			else
			     (xml-error (concatenate 'string
					  "unrecognized content '"
					  (subseq val 0 (min (length val) 40)) "'"))))
	    elseif (and (listp val) (eq :comment (first val)))
	      then
		   (when (not content-only) (push val guts))
	    elseif (eq kind :pi)
	      then
		   (push val guts)
	    elseif (eq kind :eof)
	      then
		   (xml-error "unexpected end of file encountered")
	    elseif (or (symbolp val)
		       (and (listp val) (symbolp (first val))))
	      then
		   (when (eq kind :start-tag)
		     (setf val (add-default-values val attlist-data)))
		   (if* (and (eq kind :start-tag) (eq kind2 :end-tag))
		      then (push (list val) guts)
			   (setf state state-element-done)
		    elseif (eq kind :start-tag)
		      then (push (list val) pending)
			   ;;(format t "pending: ~s guts: ~s <2>~%" pending guts)
			   (when (iostruct-entity-bufs tokenbuf)
			     (push (if (symbolp val) val (first val)) entity-open-tags))
			   (setf state state-element-contents)
		      else (xml-error (concatenate 'string
					"encountered token at illegal syntax position: '"
					(string kind) "'"
					(if* (null guts) then
						" at start of contents"
					   else 
						(concatenate 'string
						  " following: '"
						  (format nil "~s" (first guts))
						  "'")))))
	      else
		   (error "this branch unexpected <1>")))
	  (#.state-docstart-misc
	   (if* (eq kind :pcdata)
	      then
		   (when (or (not kind2) (not (all-xml-whitespace-p val)))
		     (if* (not kind2) then
			     (xml-error "An entity reference occured where only whitespace or the first element may occur")
			else
			     (xml-error (concatenate 'string
					  "unrecognized content '"
					  (subseq val 0 (min (length val) 40)) "'"))))
	    elseif (and (listp val) (eq :DOCTYPE (first val)))
	      then
		   (if* (eq (third val) :SYSTEM) then
			   (setf system-string (fourth val))
			   (setf val (remove (third val) val))
			   (setf val (remove (third val) val))
		    elseif (eq (third val) :PUBLIC) then
			   (setf public-string (normalize-public-value (fourth val)))
			   (setf system-string (fifth val))
			   (setf val (remove (third val) val))
			   (setf val (remove (third val) val))
			   (setf val (remove (third val) val)))
		   (when system-string
		     (if* external-callback then
			     (let ((ext-stream (apply external-callback
						      (list (parse-uri system-string)
							    :DOCTYPE
							    public-string
							    ))))
			       (when ext-stream
				 (let (ext-io (entity-buf (get-tokenbuf)))
				   (setf (tokenbuf-stream entity-buf) ext-stream)
				   (setf ext-io (make-iostruct :tokenbuf entity-buf
							       :do-entity 
							       (iostruct-do-entity tokenbuf)
							       :read-sequence-func 
							       (iostruct-read-sequence-func tokenbuf)))
				   (unicode-check ext-stream ext-io)
				   (setf (iostruct-parameter-entities ext-io) 
				     (iostruct-parameter-entities tokenbuf))
				   (setf (iostruct-general-entities ext-io)
				     (iostruct-general-entities tokenbuf))
				   (unwind-protect
				       (setf val (append val 
							 (list (append 
								(list :external)
								(parse-dtd
								 ext-io	    
								 t external-callback)))))
				     (setf (iostruct-seen-any-dtd tokenbuf) t)
				     (setf (iostruct-seen-external-dtd tokenbuf) t)
				     (setf (iostruct-seen-parameter-reference tokenbuf)
				       (iostruct-seen-parameter-reference ext-io))
				     (setf (iostruct-general-entities tokenbuf)
				       (iostruct-general-entities ext-io))
				     (setf (iostruct-parameter-entities tokenbuf)
				       (iostruct-parameter-entities ext-io))
				     (setf (iostruct-do-entity tokenbuf)
				       (iostruct-do-entity ext-io))
				     (dolist (entity-buf2 (iostruct-entity-bufs ext-io))
				       (when (streamp (tokenbuf-stream entity-buf2))
					 (close (tokenbuf-stream entity-buf2))
					 (put-back-tokenbuf entity-buf2)))
				     (close (tokenbuf-stream entity-buf))
				     (put-back-tokenbuf entity-buf))
				   )))
			else
			     (setf (iostruct-do-entity tokenbuf) nil)))
		   (setf attlist-data
		     (process-attlist (rest (rest val)) attlist-data))
		   (when (not content-only) (push val guts))
		   (setf state state-docstart-misc2)
	    elseif (and (listp val) (eq :comment (first val)))
	      then
		   (when (not content-only) (push val guts))
	    elseif (eq kind :pi)
	      then
		   (push val guts)
	    elseif (or (symbolp val)
		       (and (listp val) (symbolp (first val))))
	      then
		   (when (eq kind :start-tag)
		     (setf val (add-default-values val attlist-data)))
		   (if* (and (eq kind :start-tag) (eq kind2 :end-tag))
		      then (push (list val) guts)
			   (setf state state-element-done)
		    elseif (eq kind :start-tag)
		      then (push (list val) pending)
			   ;;(format t "pending: ~s guts: ~s <3>~%" pending guts)
			   (when (iostruct-entity-bufs tokenbuf)
			     (push (if (symbolp val) val (first val)) entity-open-tags))
			   (setf state state-element-contents)
		      else (xml-error (concatenate 'string
					"encountered token at illegal syntax position: '"
					(string kind) "'"
					(concatenate 'string
					  " following: '"
					  (format nil "~s" (first guts))
					  "'"))))
	      else
		   (print (list val kind kind2))
		   (break "check for other docstart-misc states")))
	  (#.state-element-contents
	   (if* (or (symbolp val)
		    (and (listp val) (symbolp (first val))))
	      then 
		   (when (eq kind :start-tag)
		     (setf val (add-default-values val attlist-data)))
		   (if* (eq kind :end-tag)
		      then (let ((candidate (first (first pending))))
			     (when (listp candidate) (setf candidate (first candidate)))
			     (if* (eq candidate val)
				then 
				     (if* (iostruct-entity-bufs tokenbuf) then
					     (when (not (eq (first entity-open-tags) val))
					       (xml-error
						(concatenate 'string
						  (string val)
						  " element closed in entity that did not open it")))
					     (setf entity-open-tags (rest entity-open-tags))
					else
					     (when (eq (first entity-open-tags) val)
					       (xml-error
						(concatenate 'string
						  (string val)
						  " element closed outside of entity that did not open it")))
					     )
				     (if* (= (length pending) 1)
					then
					     (push (first pending) guts)
					     (setf state state-element-done)
					else
					     (setf (second pending)
					       (append (second pending) (list (first pending)))))
				     (setf pending (rest pending))
				     ;;(format t "pending: ~s guts: ~s <4>~%" pending guts)
				else (xml-error (format nil 
							"encountered end tag: ~s expected: ~s"
							val candidate))))
		    elseif (and (eq kind :start-tag) (eq kind2 :end-tag))
		      then
			   (setf (first pending)
			     (append (first pending) (list (list val))))
			   ;;(format t "pending: ~s guts: ~s <5>~%" pending guts)
		    elseif (eq kind :start-tag)
		      then 
			   (push (list val) pending)
			   ;;(format t "pending: ~s guts: ~s <6>~%" pending guts)
			   (when (iostruct-entity-bufs tokenbuf)
			     (push (if (symbolp val) val (first val)) entity-open-tags))
		    elseif (eq kind :cdata) then
			   (setf (first pending)
			     (append (first pending) (rest val)))
			   (let ((old (first pending))
				 (new))
			     (dolist (item old)
			       (if* (and (stringp (first new)) (stringp item)) then
				       (setf (first new)
					 (concatenate 'string (first new) item))
				  else (push item new)))
			     (setf (first pending) (reverse new)))
		    elseif (eq kind :comment) then
			  (when (not content-only) (push val guts)) 
		    elseif (eq kind :pi)
		      then
			   (setf (first pending)
			     (append (first pending) (list val)))
		    elseif (eq kind :eof)
		      then
			   (xml-error "unexpected end of file encountered")
		      else (xml-error (format nil "unexpected token: ~s" val)))
	    elseif (eq kind :pcdata)
	      then 
		   (setf (first pending)
		     (append (first pending) (list val)))
		   (let ((old (first pending))
			 (new))
		     (dolist (item old)
		       (if* (and (stringp (first new)) (stringp item)) then
			       (setf (first new)
				 (concatenate 'string (first new) item))
			  else (push item new)))
		     (setf (first pending) (reverse new)))
	      else (xml-error (format nil "unexpected token: ~s" val))))
	  (#.state-element-done
	   (if* (eq kind :pcdata)
	      then
		   (when (or (not kind2) (not (all-xml-whitespace-p val)))
		     (if* (not kind2) then
			     (xml-error "An entity reference occured where only whitespace or the first element may occur")
			else
			     (xml-error (concatenate 'string
					  "unrecognized content '"
					  (subseq val 0 (min (length val) 40)) "'"))))
	    elseif (eq kind :eof) then 
		   (put-back-tokenbuf (iostruct-tokenbuf tokenbuf))
		   (return (nreverse guts))
	    elseif (eq kind :comment) then
		   (when (not content-only) (push val guts))
	    elseif (eq kind :pi)
	      then (push val guts)
	      else
		   (xml-error (concatenate 'string
				"encountered token at illegal syntax position: '"
				(string kind) "'"
				(concatenate 'string
				  " following: '"
				  (format nil "~s" (first guts))
				  "'")))
		   ))
	  (t 
	   (error "need to support state:~s token:~s  kind:~s kind2:~s <parse>" state val kind kind2)))
	))))

(eval-when (compile load eval)
  (defconstant state-pcdata 0) ;;looking for < (tag start), & (reference); all else is string data
  (defconstant state-readtagfirst 1) ;; seen < looking for /,?,!,name start
  (defconstant state-readtag-? 2) ;; seen <? looking for space,char
  (defconstant state-readtag-! 3) ;; seen <! looking for name,[,-
  (defconstant state-readtag-end 4) ;; found </ looking for tag name
  (defconstant state-readtag 5) ;; found < name start looking for more name, /, >
  (defconstant state-findattributename 6) ;; found <?xml space looking for ?,>,space,name start
  (defconstant state-readpi 7) 
  (defconstant state-noattributename 8)
  (defconstant state-attribname 9) ;; found <?xml space name start looking for more name,=
  (defconstant state-attribstartvalue 10) ;; found <?xml space name= looking for ',"
  (defconstant state-attribvaluedelim 11)
  (defconstant state-readtag-!-name 12) ;; seen <!name(start) looking for more name chars or space
  (defconstant state-readtag-!-conditional 13) ;; found <![ looking for CDATA, INCLUDE, IGNORE
  (defconstant state-readtag-!-comment 14)
  (defconstant state-readtag-!-readcomment 15)
  (defconstant state-readtag-!-readcomment2 16)
  (defconstant state-readtag-end-bracket 17)
  (defconstant state-readpi2 18) ;; found <?name space char looking for char,?
  (defconstant state-prereadpi 19);; found <?name space looking for space,character
  (defconstant state-pre-!-contents 20) ;; found <!name space looking for > or contents
  (defconstant state-!-contents 21) ;; found <!name space name start looking for more name,>,[,space
  (defconstant state-!-doctype 22) ;; found <!DOCTYPE space looking for space,>,[,name
  (defconstant state-begin-dtd 23)
  (defconstant state-!-doctype-ext 24) ;; found <!DOCTYPE space name space name start looking for name,space
  (defconstant state-!-doctype-system 25) ;; found <!DOCTYPE name SYSTEM looking for ',"
  (defconstant state-!-doctype-public 26) ;; found <!DOCTYPE name PUBLIC looking for ',"
  (defconstant state-!-doctype-system2 27) ;; found <!DOCTYPE name SYSTEM " looking for chars,"
  (defconstant state-!-doctype-system3 28) ;; found <!DOCTYPE name SYSTEM ' looking for chars,'
  (defconstant state-!-doctype-ext2 29) ;; found <!DOCTYPE name SYSTEM/PUBLIC etc. looking for space,>,[
  (defconstant state-!-doctype-ext3 30) ;; processed DTD looking for space,>
  (defconstant state-!-doctype-public2 31) ;;  found <!DOCTYPE name PUBLIC " looking for text or "
  (defconstant state-!-doctype-public3 32) ;;  found <!DOCTYPE name PUBLIC ' looking for text or '
  (defconstant state-readtag2 33) ;; found <name looking for space,/,>,attrib name
  (defconstant state-readtag3 34) ;; found <name/ or <name / looking for >
  (defconstant state-readtag4 35) ;; found <name attrib-name start looking for more name,=
  (defconstant state-readtag5 36) ;; found attrib= looking for ',"
  (defconstant state-readtag6 37) ;; found attrib=['"] looking for end delimiter,value,reference
  (defconstant state-readtag7 38) ;; found & inside attribute value, looking for # or name start
  (defconstant state-readtag8 39) ;; found &# in attribute value looking for char code
  (defconstant state-readtag9 40) ;; found &name start looking for more name,;
  (defconstant state-readtag10 41) ;; found &#x in attribute value looking for hex code
  (defconstant state-readtag11 42) ;; found &#[0-9] looking for more digits,;
  (defconstant state-readtag-end2 43) ;; found </ & tag name start looking for more tag, space, >
  (defconstant state-readtag-end3 44) ;; found </ end tag name space looking for >
  (defconstant state-pcdata2 45) ;; seen & looking for name start
  (defconstant state-pcdata3 46) ;; seen &# looking for character reference code
  (defconstant state-pcdata4 47) ;; working on entity reference name looking for ;
  (defconstant state-pcdata5 48) ;; working on hex character code reference
  (defconstant state-pcdata6 49) ;; working on decimal character code reference
  (defconstant state-findattributename0 50)
  (defconstant state-readtag6a 51)
  (defconstant state-readtag-!-conditional4 52)
  (defconstant state-readtag-!-conditional5 53)
  (defconstant state-readtag-!-conditional6 54)
  (defconstant state-readtag-!-conditional7 55)
  ;;(defconstant state-pcdata-parsed 56)
  (defconstant state-pcdata7 57)
  (defconstant state-pcdata8 58)
  (defconstant state-readtag12 59)
  (defconstant state-attribname2 60)
  )

(defun next-token (tokenbuf external-callback attlist-data)
  (declare (optimize (speed 3) (safety 1)))
  ;; return two values: 
  ;;    the next token from the stream.
  ;; 	the kind of token
  ;;
  ;; if read-sequence-func is non-nil,
  ;; read-sequence-func is called to fetch the next character
  (macrolet ((add-to-entity-buf (entity-symbol p-value)
	       `(progn
		  (push (make-tokenbuf :cur 0 :max (length p-value) :data p-value)
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
    
    (let ((state state-pcdata)
	  (coll  (get-collector))
	  (entity  (get-collector))
	  (tag-to-return)
	  (tag-to-return-string)
	  (attrib-name)
	  (empty-delim)
	  (value-delim)
	  (attrib-value)
	  (attribs-to-return)
	  (contents-to-return)
	  (char-code 0)
	  (special-tag-count 0)
	  (attrib-value-tokenbuf)
	  (last-ch)
	  (cdatap t)
	  (pcdatap t)
	  (entity-source)
	  (ns-token)
	  (ch))
      
      (loop

	(setq ch (get-next-char tokenbuf))
	(when *debug-xml* (format t "ch: ~s code: ~x state:~s entity-names:~s~%" 
				  ch (char-code ch) state (iostruct-entity-names tokenbuf)))
	(if* (null ch)
	   then (return) ; eof -- exit loop
		)
      
      
	(case state
	  (#.state-pcdata
	  (if* (eq ch #\<)
	     then
		  (setf entity-source (first (iostruct-entity-bufs tokenbuf)))
		  (if* (> (collector-next coll) 0)
		     then		; have collected something, return this string
			  (un-next-char ch) ; push back the <
			  (return)
		      else ; collect a tag
			  (setq state state-readtagfirst))
	   elseif (eq #\& ch)
	     then (setf state state-pcdata2)
		  (setf entity-source (first (iostruct-entity-bufs tokenbuf)))
		  (setf pcdatap nil)
	   elseif (eq #\] ch) then (setf state state-pcdata7)
	   elseif (not (xml-char-p ch)) then
		  (xml-error (concatenate 'string
			       "Illegal character: "
			       (string ch)
			       " detected in input"))
	     else
		  (add-to-coll coll ch)
		  #+ignore
		  (if* (not (eq ch #\return))
		     then (add-to-coll coll ch))))
	  
	  (#.state-pcdata7
	   (if* (eq #\] ch) then (setf state state-pcdata8)
	      else (setf state state-pcdata)
		   (add-to-coll coll #\]) (un-next-char ch)))
	  
	  (#.state-pcdata8
	   (if* (eq #\> ch) then
		   (add-to-coll coll #\])
		   (add-to-coll coll #\])
		   (add-to-coll coll #\>)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"content cannot contain ']]>':'"
				(compute-coll-string coll)
				"'"))
	    elseif (eq #\] ch) then
		   (add-to-coll coll #\])
	      else (setf state state-pcdata)
		   (add-to-coll coll #\]) (add-to-coll coll #\]) (un-next-char ch)))
	  
	  (#.state-pcdata2
	   (if* (eq #\# ch)
	      then (setf state state-pcdata3)
	    elseif (xml-name-start-char-p ch)
	      then (setf state state-pcdata4)
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
				"'"))
		   ))
	  
	  (#.state-pcdata3
	   (if* (eq #\x ch)
	      then (setf state state-pcdata5)
	    elseif (<= (char-code #\0) (char-code ch) (char-code #\9))
	      then (setf state state-pcdata6)
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
	  
	  (#.state-pcdata4
	   (if* (xml-name-char-p ch)
	      then (add-to-coll entity ch)
	    elseif (eq #\; ch)
	      then (let ((entity-symbol (compute-tag entity)))
		     (clear-coll entity)
		     (if* (not (eq entity-source (first (iostruct-entity-bufs tokenbuf)))) then
			     (xml-error
			      (concatenate 'string
				(string entity-symbol)
				" reference cannot be constructed from entity reference/character data sequence"))
			else
			     (setf entity-source nil))
		     (if* (string= (symbol-name entity-symbol) "amp") then (add-to-coll coll #\&)
		      elseif (string= (symbol-name entity-symbol) "lt") then (add-to-coll coll #\<)
		      elseif (string= (symbol-name entity-symbol) "gt") then (add-to-coll coll #\>)
		      elseif (string= (symbol-name entity-symbol) "apos") then (add-to-coll coll #\')
		      elseif (string= (symbol-name entity-symbol) "quot") then (add-to-coll coll #\")
			else 
			     (let (p-value)
			       (if* (and (iostruct-do-entity tokenbuf)
					 (setf p-value 
					   (assoc entity-symbol 
						  (iostruct-general-entities tokenbuf)))) then
				       (setf p-value (rest p-value))
				       (when (member entity-symbol (iostruct-entity-names tokenbuf))
					 (xml-error (concatenate 'string
						      "entity:"
						      (string entity-symbol)
						      " in recursive reference")))
				       (push entity-symbol (iostruct-entity-names tokenbuf))
				       (if* (stringp p-value) then
					       (add-to-entity-buf entity-symbol p-value)
					elseif (null external-callback) then
					       (setf (iostruct-do-entity tokenbuf) nil)
					elseif p-value then
					       (let ((entity-stream (apply external-callback p-value)))
						 (if* entity-stream then
							 (let ((entity-buf (get-tokenbuf)))
							   (setf (tokenbuf-stream entity-buf) entity-stream)
							   (unicode-check entity-stream tokenbuf)
							   (push entity-buf 
								 (iostruct-entity-bufs tokenbuf))
							   ;; check for possible external textdecl
							   (let ((count 0) cch
								 (string "<?xml "))
							     (if* (dotimes (i (length string) t)
								    (setf cch (get-next-char tokenbuf))
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
								     (swallow-xml-token 
								      tokenbuf 
								      external-callback)
								else
								     (un-next-char cch)
								     (decf count)
								     (loop
								       (when (< count 0) (return))
								       (un-next-char (schar string count))
								       (decf count)))) 
							   )
						    else
							 (xml-error (concatenate 'string
								      "Reference to unparsed entity "
								      (string entity-symbol)))
							 ))
					       )
				elseif (or (not (iostruct-seen-any-dtd tokenbuf))
					   (iostruct-standalonep tokenbuf)
					   (and (iostruct-seen-any-dtd tokenbuf)
						(not (iostruct-seen-external-dtd tokenbuf))
						(not (iostruct-seen-parameter-reference tokenbuf))))
				  then
				       (xml-error (concatenate 'string
						    (string entity-symbol)
						    " must have entity declaration before being referenced"))
				       ))
			     ))
		   (setq state state-pcdata)
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
	  
	  (#.state-pcdata5
	   (let ((code (char-code ch)))
	     (if* (eq #\; ch)
		then (if* (not (eq entity-source (first (iostruct-entity-bufs tokenbuf)))) then
			     (xml-error
			      (concatenate 'string
				(string (code-char char-code))
				" reference cannot be constructed from entity reference/character data sequence"))
			else
			     (setf entity-source nil))
		     (when (not (xml-char-p (code-char char-code)))
			   (xml-error
			    (concatenate 'string
			      "Character reference: "
			      (format nil "~s" char-code)
			      " (decimal) is not valid XML input character")))
		     (add-to-coll coll (code-char char-code))
		     (setf char-code 0)
		     (setq state state-pcdata)
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
	  
	  (#.state-pcdata6
	   (let ((code (char-code ch)))
	     (if* (eq #\; ch)
		then (if* (not (eq entity-source (first (iostruct-entity-bufs tokenbuf)))) then
			     (xml-error
			      (concatenate 'string
				(string (code-char char-code))
				" reference cannot be constructed from entity reference/character data sequence"))
			else
			     (setf entity-source nil))
		     (when (not (xml-char-p (code-char char-code)))
			   (xml-error
			    (concatenate 'string
			      "Character reference: "
			      (format nil "~s" char-code)
			      " (decimal) is not valid XML input character")))
		     (add-to-coll coll (code-char char-code))
		     (setf char-code 0)
		     (setq state state-pcdata)
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
	  
	  (#.state-readtag-end
	   (if* (xml-name-start-char-p ch)
	      then (setf state state-readtag-end2)
		   (un-next-char ch)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal end tag name, starting at: '</"
				(compute-coll-string coll)
				"'"))
		   ))
	  
	  (#.state-readtag-end2
	   (if* (xml-name-char-p ch)
	      then (add-to-coll coll ch)
	    elseif (eq #\> ch) then 
		   (let ((tag-string (compute-coll-string coll)))
		     (when (and (iostruct-ns-scope tokenbuf)
				(string= tag-string
				    (first (first (iostruct-ns-scope tokenbuf)))))
		       (dolist (item (second (first (iostruct-ns-scope tokenbuf))))
			 (setf (iostruct-ns-to-package tokenbuf)
			   (remove (assoc item (iostruct-ns-to-package tokenbuf))
				   (iostruct-ns-to-package tokenbuf))))
		       (setf (iostruct-ns-scope tokenbuf)
			 (rest (iostruct-ns-scope tokenbuf)))))
		   (setq tag-to-return (compute-tag coll *package* 
						    (iostruct-ns-to-package tokenbuf)))
		   (return)
	    elseif (xml-space-p ch) then (setf state state-readtag-end3)
		   (let ((tag-string (compute-coll-string coll)))
		     (when (and (iostruct-ns-scope tokenbuf)
				(string= tag-string
				    (first (first (iostruct-ns-scope tokenbuf)))))
		       (setf (iostruct-ns-scope tokenbuf)
			 (rest (iostruct-ns-scope tokenbuf)))))
		   (setq tag-to-return (compute-tag coll *package*
						    (iostruct-ns-to-package tokenbuf)))
	      else (let ((tmp (compute-coll-string coll)))
		     (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error (concatenate 'string
				  "illegal end tag name, starting at: '</"
				  tmp
				  (compute-coll-string coll)
				  "'")))
		   ))
	  
	  (#.state-readtag-end3
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\> ch) then (return)
	      else (let ((tmp (compute-coll-string coll)))
		     (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error (concatenate 'string
				  "illegal end tag name, starting at: '"
				  (compute-coll-string coll)
				  "' end tag name: " tmp )))
		   )) 
		     
	  (#.state-readtagfirst
	   ; starting to read a tag name
	   (if* (eq #\/ ch)
	      then (setf state state-readtag-end)
	    elseif (eq #\? ch)
	      then (setf state state-readtag-?)
		   (setf empty-delim #\?)
	    elseif (eq #\! ch)
	      then (setf state state-readtag-!)
		   (setf empty-delim nil)
	    elseif (xml-name-start-char-p ch)
	      then (setf state state-readtag)
		   (setf empty-delim #\/)
		   (un-next-char ch)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal character following '<', starting at '"
				(compute-coll-string coll)
				"'"))
		   ))
	  
	  (#.state-readtag-!
	   (if* (xml-name-start-char-p ch)
	      then
		   (setf state state-readtag-!-name)
		   (un-next-char ch)
	    elseif (eq #\[ ch)
	      then
		   (setf state state-readtag-!-conditional)
	    elseif (eq #\- ch)
	      then
		   (setf state state-readtag-!-comment)
	      else
		   (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal character following '<!', starting at '<!"
				(compute-coll-string coll)
				"'"))
		   ))
	  
	  (#.state-readtag-!-conditional
	   (if* (eq #\C ch) then
		   (setf state state-readtag-!-conditional4)
		   (setf special-tag-count 1)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal character following '<![', starting at '<!["
				(compute-coll-string coll)
				"'"))
		   ))   
	  
	  (#.state-readtag-!-conditional4
	   (if* (not (eq (elt "CDATA[" special-tag-count) ch))
	      then (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal token following '<![', starting at '<!["
				(subseq "CDATA[" 0 special-tag-count)
				(compute-coll-string coll)
				"'"))
	    elseif (eq #\[ ch) then (setf state state-readtag-!-conditional5)   
	      else (incf special-tag-count)))
	  
	  (#.state-readtag-!-conditional5
	   (if* (eq #\] ch)
	      then (setf state state-readtag-!-conditional6)
	    elseif (not (xml-char-p ch)) then
		  (xml-error (concatenate 'string
			       "Illegal character: "
			       (string ch)
			       " detected in CDATA input"))
	      else (add-to-coll coll ch)))
	  
	  (#.state-readtag-!-conditional6
	   (if* (eq #\] ch)
	      then (setf state state-readtag-!-conditional7)
	      else (setf state state-readtag-!-conditional5)
		   (add-to-coll coll #\])
		   (add-to-coll coll ch)))
	  
	  (#.state-readtag-!-conditional7
	   (if* (eq #\> ch)
	      then 
		   (if* (not (eq entity-source (first (iostruct-entity-bufs tokenbuf)))) then
			   (xml-error
			    "CDATA cannot be constructed from entity reference/character data sequence")
		      else
			     (setf entity-source nil))
		   (return)
	    elseif (eq #\] ch) then
		   (add-to-coll coll #\]) ;; come back here to check again
	      else (setf state state-readtag-!-conditional5)
		   (add-to-coll coll #\])
		   (add-to-coll coll #\])
		   (add-to-coll coll ch)))	   
		     
	  (#.state-readtag-!-comment
	   (if* (eq #\- ch)
	      then (setf state state-readtag-!-readcomment)
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

	  (#.state-readtag-!-readcomment
	   (if* (eq #\- ch)
	      then (setf state state-readtag-!-readcomment2)
	    elseif (not (xml-char-p ch)) then
		   (xml-error (concatenate 'string
				"Illegal character: "
				(string ch)
				" detected in input"))
	      else (add-to-coll coll ch)))
	  
	  (#.state-readtag-!-readcomment2
	   (if* (eq #\- ch)
	      then (setf state state-readtag-end-bracket)
	      else (setf state state-readtag-!-readcomment)
		   (add-to-coll coll #\-) (add-to-coll coll ch)))
	  
	  (#.state-readtag-end-bracket
	   (if* (eq #\> ch)
	      then (if* (not (eq entity-source (first (iostruct-entity-bufs tokenbuf)))) then
			   (xml-error
			    (concatenate 'string
			      (string tag-to-return)
			    " tag cannot be constructed from entity reference/character data sequence"))
		      else
			     (setf entity-source nil))
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
	  
	  (#.state-readtag
	   (if* (xml-name-char-p ch) ;; starting char already passed more restrictive test
	      then
		   (add-to-coll coll ch)
	      else
		   (if* (xml-space-p ch) then
			   (setf tag-to-return-string (compute-coll-string coll))
			   (setq tag-to-return 
			     (compute-tag coll *package*
					  (iostruct-ns-to-package tokenbuf)))
			   (clear-coll coll)
			   (setf state state-readtag2)
		    elseif (eq #\> ch) then 
			   (setq tag-to-return 
			     (compute-tag coll *package*
					  (iostruct-ns-to-package tokenbuf)))
			   (clear-coll coll)
			   (return)
		    elseif (eq #\/ ch) then 
			   (setq tag-to-return 
			     (compute-tag coll *package*
					  (iostruct-ns-to-package tokenbuf)))
			   (clear-coll coll)
			   (setf state state-readtag3)
		      else (dotimes (i 15)
			     (add-to-coll coll ch)
			     (setq ch (get-next-char tokenbuf))
			     (if* (null ch)
				then (return)))
			   (xml-error 
			    (concatenate 'string
			      "illegal token name, starting at '"
			      (compute-coll-string coll)
			      "'"))
			   )))
	  
	  (#.state-readtag2
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\> ch) then (return)
	    elseif (eq #\/ ch) then (setf state state-readtag3)
	    elseif (xml-name-start-char-p ch) then 
		   (un-next-char ch)
		   (setf state state-readtag4)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error 
		    (concatenate 'string
		      "illegal token, starting at '"
		      (compute-coll-string coll)
		      "' following element token start: " (string tag-to-return)))
		   ))
	  
	  (#.state-readtag4
	   (if* (xml-name-char-p ch) ;; starting char already passed more restrictive test
	      then
		   (add-to-coll coll ch)
	    elseif (eq #\= ch) then
		   (setq attrib-name (compute-tag coll *package*
						  (iostruct-ns-to-package tokenbuf)))
		   (clear-coll coll)
		   (let ((name (symbol-name attrib-name)))
		     (when (and (>= (length name) 5)
				(string= name "xmlns" :end1 5))
		       (if* (= (length name) 5)
			  then
			       (setf ns-token :none)
			elseif (eq (schar name 5) #\:)
			  then
			       (setf ns-token (subseq name 6)))))
		   (setf state state-readtag5)
	    elseif (xml-space-p ch) then
		   (setq attrib-name (compute-tag coll *package*
						  (iostruct-ns-to-package tokenbuf)))
		   (clear-coll coll)
		   (let ((name (symbol-name attrib-name)))
		     (when (and (>= (length name) 5)
				(string= name "xmlns" :end1 5))
		       (if* (= (length name) 5)
			  then
			       (setf ns-token :none)
			  else
			       (setf ns-token (subseq name 6)))))
		   (setf state state-readtag12)
	      else (let ((tmp (compute-coll-string coll)))
		     (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error 
		      (concatenate 'string
			"looking for attribute '=', found: '"
		      (compute-coll-string coll)
		      "' following attribute name: " tmp)))
		   ))
	  
	  (#.state-readtag12
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\= ch) then (setf state state-readtag5)
	      else
		 (dotimes (i 15)
		   (add-to-coll coll ch)
		   (setq ch (get-next-char tokenbuf))
		   (if* (null ch)
		      then (return)))
		   (xml-error 
		    (concatenate 'string
		      "looking for attribute '=', found: '"
		      (compute-coll-string coll)
		      "' following attribute name: " (string attrib-name)))))
	  
	  (#.state-readtag5
	   ;; begin to collect attribute value
	   (if* (or (eq ch #\")
		    (eq ch #\'))
	      then (setq value-delim ch)
		   (let* ((tag-defaults (assoc tag-to-return attlist-data))
			  (this-attrib (assoc attrib-name tag-defaults)))
		     (when (and (second this-attrib) (not (eq (second this-attrib) :CDATA)))
		       (setf cdatap nil))
		     )
		   (setq state state-readtag6)
	    elseif (xml-space-p ch) then nil
	      else
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error 
		    (concatenate 'string
		      "attribute value not delimited by ' or \" : '"
		      (compute-coll-string coll)
		      "' following attribute: " (string attrib-name)))
		   ))
	  
	  (#.state-readtag6
	   (let ((from-entity (and attrib-value-tokenbuf
				   (eq attrib-value-tokenbuf 
				       (first (iostruct-entity-bufs tokenbuf))))))
	     (when (not from-entity) (setf attrib-value-tokenbuf nil))
	     (if* from-entity then
		     (if* (eq #\newline ch) then (setf ch #\space)
		      elseif (eq #\return ch) then (setf ch #\space)
		      elseif (eq #\tab ch) then (setf ch #\space)
			     ))
	     (if* (and (not from-entity) (eq ch value-delim))
		then (setq attrib-value (compute-coll-string coll))
		     (when (not cdatap)
		       (setf attrib-value (normalize-attrib-value attrib-value)))
		     (clear-coll coll)
		     (push attrib-name attribs-to-return)
		     (push attrib-value attribs-to-return)
		     (when ns-token
		       (let ((package (assoc (parse-uri attrib-value)
					     (iostruct-uri-to-package tokenbuf)
					     :test 'uri=)))
			 (if* package then (setf package (rest package))
			    else
				 (setf package
				   (let ((i 0) new-package)
				     (loop
				       (let* ((candidate (concatenate 'string
							   "net.xml.namespace."
							   (format nil "~s" i)))
					      (exists (find-package candidate)))
					 (if* exists
					    then (incf i)
					    else (setf new-package (make-package candidate))
						 (setf (iostruct-uri-to-package tokenbuf)
						   (acons (parse-uri attrib-value) new-package
							  (iostruct-uri-to-package tokenbuf)))  
						 (return new-package)))))))
			 (setf (iostruct-ns-to-package tokenbuf)
			   (acons ns-token package (iostruct-ns-to-package tokenbuf)))
			 )
		       (if* (and (first (iostruct-ns-scope tokenbuf))
				 (string= (first (first (iostruct-ns-scope tokenbuf)))
				     tag-to-return-string))
			  then
			       (push ns-token (second (first (iostruct-ns-scope tokenbuf))))
			  else
			       (push (list tag-to-return-string (list ns-token))
				     (iostruct-ns-scope tokenbuf)))
		       (setf ns-token nil))
		     (setq state state-readtag6a)
	      elseif (eq #\newline ch) then
		     (when (not (eq #\return last-ch)) (add-to-coll coll #\space))
	      elseif (or (eq #\tab ch) (eq #\return ch)) then
		     (add-to-coll coll #\space)
	      elseif (eq #\& ch)
		 then (setq state state-readtag7)
		      (setf entity-source (first (iostruct-entity-bufs tokenbuf)))
	      elseif (and (xml-char-p ch) (not (eq #\< ch)))
		then (add-to-coll coll ch)
		else 
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error 
		      (concatenate 'string
			"attribute value cannot contain '<': '"
			(compute-coll-string coll)
			"' following attribute: " (string attrib-name)))
		     )
	     (setf last-ch ch)))
	  
	  (#.state-readtag6a
	   (if* (xml-space-p ch) then (setf state state-readtag2)
	    elseif (eq #\> ch) then (setf state state-readtag2)
		   (return)
	    elseif (eq #\/ ch) then (setf state state-readtag3)
	    else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error 
		    (concatenate 'string
		      "illegal token, starting at '"
		      (compute-coll-string coll)
		      "' following element token start: " (string tag-to-return)))
		   ))
	  
	  (#.state-readtag7
	   (if* (eq #\# ch)
	      then (setf state state-readtag8)
	    elseif (xml-name-start-char-p ch)
	      then (setf state state-readtag9)
		   (un-next-char ch)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error 
		    (concatenate 'string
		      "attribute value contains illegal reference name: '&"
		      (compute-coll-string coll)
		      "' in attribute value for: " (string attrib-name)))
		   ))
	  
	  (#.state-readtag8
	   (if* (eq #\x ch)
	      then (setf state state-readtag10)
	    elseif (<= (char-code #\0) (char-code ch) (char-code #\9))
	      then (setf state state-readtag11)
		   (un-next-char ch)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error 
		    (concatenate 'string
		      "attribute value contains illegal character reference code: '"
		      (compute-coll-string coll)
		      "' in attribute value for: " (string attrib-name)))
		   ))
	  
	  (#.state-readtag10
	   (let ((code (char-code ch)))
	     (if* (eq #\; ch)
		then (if* (not (eq entity-source (first (iostruct-entity-bufs tokenbuf)))) then
			     (xml-error
			      (concatenate 'string
				(string (code-char char-code))
				" reference cannot be constructed from entity reference/character data sequence"))
			else
			     (setf entity-source nil))
		     (add-to-coll coll (code-char char-code))
		     (setf char-code 0)
		     (setq state state-readtag6)
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
		     (xml-error 
		      (concatenate 'string
			"attribute value contains illegal hexidecimal character reference code: '"
			(compute-coll-string coll)
			"' in attribute value for: " (string attrib-name)))
		     )))
	  
	  (#.state-readtag11
	   (let ((code (char-code ch)))
	     (if* (eq #\; ch)
		then (if* (not (eq entity-source (first (iostruct-entity-bufs tokenbuf)))) then
			     (xml-error
			      (concatenate 'string
				(string (code-char char-code))
				" reference cannot be constructed from entity reference/character data sequence"))
			else
			     (setf entity-source nil))
		     (add-to-coll coll (code-char char-code))
		     (setf char-code 0)
		     (setq state state-readtag6)
	      elseif (<= (char-code #\0) code (char-code #\9))
		then (setf char-code (+ (* char-code 10) (- code (char-code #\0))))
		else (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error 
		      (concatenate 'string
			"attribute value contains illegal decimal character reference code: '"
			(compute-coll-string coll)
			"' in attribute value for: " (string attrib-name)))
		     )))
	  
	  (#.state-readtag9
	   (if* (xml-name-char-p ch)
	      then (add-to-coll entity ch)
	    elseif (eq #\; ch)
	      then (let ((entity-symbol (compute-tag entity)))
		     (clear-coll entity)
		     (if* (not (eq entity-source (first (iostruct-entity-bufs tokenbuf)))) then
			     (xml-error
			      (concatenate 'string
				(string entity-symbol)
				" reference cannot be constructed from entity reference/character data sequence"))
			else
			     (setf entity-source nil))
		     (if* (string= (symbol-name entity-symbol) "amp") then (add-to-coll coll #\&)
		      elseif (string= (symbol-name entity-symbol) "lt") then (add-to-coll coll #\<)
		      elseif (string= (symbol-name entity-symbol) "gt") then (add-to-coll coll #\>)
		      elseif (string= (symbol-name entity-symbol) "apos") then (add-to-coll coll #\')
		      elseif (string= (symbol-name entity-symbol) "quot") then (add-to-coll coll #\")
			else (let (p-value)
			       (if* (and (iostruct-do-entity tokenbuf)
					 (setf p-value 
					   (assoc entity-symbol 
						  (iostruct-general-entities tokenbuf)))) then
				       (setf p-value (rest p-value))
				       (when (member entity-symbol (iostruct-entity-names tokenbuf))
					 (xml-error (concatenate 'string
						      "entity:"
						      (string entity-symbol)
						      " in recursive reference")))
				       (push entity-symbol (iostruct-entity-names tokenbuf))
				       (if* (stringp p-value) then
					       (add-to-entity-buf entity-symbol p-value)
					       (when (not attrib-value-tokenbuf)
						 (setf attrib-value-tokenbuf
						   (first (iostruct-entity-bufs tokenbuf))))
					elseif (null external-callback) then
					       (setf (iostruct-do-entity tokenbuf) nil)
					elseif p-value then
					       (let ((entity-stream (apply external-callback p-value)))
						 (if* entity-stream then
							 (let ((entity-buf (get-tokenbuf)))
							   (setf (tokenbuf-stream entity-buf) entity-stream)
							   (unicode-check entity-stream tokenbuf)
							   (push entity-buf 
								 (iostruct-entity-bufs tokenbuf))
							   ;; check for possible external textdecl
							   (let ((count 0) cch
								 (string "<?xml "))
							     (if* (dotimes (i (length string) t)
								    (setf cch (get-next-char tokenbuf))
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
								     (swallow-xml-token 
								      tokenbuf 
								      external-callback)
								else
								     (un-next-char cch)
								     (decf count)
								     (loop
								       (when (< count 0) (return))
								       (un-next-char (schar string count))
								       (decf count)))) 
							   )
						    else
							 (xml-error (concatenate 'string
								      "Reference to unparsed entity "
								      (string entity-symbol)))
							 ))
					       )
				elseif (or (not (iostruct-seen-any-dtd tokenbuf))
					   (and (iostruct-seen-any-dtd tokenbuf)
						(not (iostruct-seen-external-dtd tokenbuf))
						(not (iostruct-seen-parameter-reference tokenbuf))))
				  then
				       (xml-error (concatenate 'string
						    (string entity-symbol)
						    " must have entity declaration before being referenced"))
				       ))
			     ))
		   (setq state state-readtag6)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error 
		    (concatenate 'string
		      "attribute value contains illegal reference name: '&"
		      (compute-coll-string coll)
		      "' in attribute value for: " (string attrib-name)))
		   ))
	  
	  (#.state-readtag3
	   (if* (eq #\> ch) then (return)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error 
		    (concatenate 'string
		      "expected '>' found '"
		      (compute-coll-string coll)
		      "' in element: " (string tag-to-return)))
		   ))
	  
	  (#.state-readtag-!-name
	   (if* (xml-name-char-p ch) ;; starting char already passed more restrictive test
	      then
		   (add-to-coll coll ch)
	      else
		   (when (not (xml-space-p ch))
		     (xml-error (concatenate 'string 
				  "expecting whitespace following: '<!"
				  (compute-coll-string coll)
				  "' ; got: '" (string ch) "'")))
		   (setq tag-to-return (compute-tag coll))
		   (clear-coll coll)
		   (setf state state-pre-!-contents)))
	  
	  (#.state-readtag-?
	   (if* (xml-name-char-p ch)
	      then
		   (add-to-coll coll ch)
	      else
		   (when (and (not (xml-space-p ch)) (not (eq #\? ch)))
		     (xml-error (concatenate 'string 
				  "expecting name following: '<?"
				  (compute-coll-string coll)
				  "' ; got: '" (string ch) "'"))
		     )
		   (when (= (collector-next coll) 0)
		     (xml-error "null <? token"))
		   (if* (and (= (collector-next coll) 3)
			     (eq (elt (collector-data coll) 0) #\x)
			     (eq (elt (collector-data coll) 1) #\m)
			     (eq (elt (collector-data coll) 2) #\l)
			     )
		      then
			   (when (eq #\? ch) (xml-error "null <?xml token"))
			   (setq tag-to-return :xml)
			   (setf state state-findattributename)
		    elseif (and (= (collector-next coll) 3)
				(or (eq (elt (collector-data coll) 0) #\x)
				    (eq (elt (collector-data coll) 0) #\X))
				(or (eq (elt (collector-data coll) 1) #\m)
				    (eq (elt (collector-data coll) 1) #\M))
				(or (eq (elt (collector-data coll) 2) #\l)
				    (eq (elt (collector-data coll) 2) #\L))
				) then
			   (xml-error "<?xml tag must be all lower case")
		      else
			   (setq tag-to-return (compute-tag coll))
			   (when (eq #\? ch) (un-next-char ch))
			   (setf state state-prereadpi))
		   (clear-coll coll)))
	  
	  (#.state-pre-!-contents
	   (if* (xml-space-p ch)
	      then nil
	    elseif (not (xml-char-p ch))
	      then (xml-error (concatenate 'string   ;; no test for this...
				"illegal character '"
				(string ch)
				" following <!" (string tag-to-return)))
	    elseif (eq #\> ch)
	      then (return)
	      else (un-next-char ch)
		   (setf state state-!-contents)))
	  
	  (#.state-begin-dtd
	   (un-next-char ch)
	   (let ((val (parse-dtd tokenbuf nil external-callback)))
	     (setf (iostruct-seen-any-dtd tokenbuf) t)
	     (push (append (list :[) val)
		   contents-to-return))
	     (setf state state-!-doctype-ext3))
	  
	  (#.state-!-contents
	   (if* (xml-name-char-p ch)
	      then (add-to-coll coll ch)
	    elseif (eq #\> ch)
	      then (push (compute-coll-string coll) contents-to-return)
		   (clear-coll coll)
		   (return)
	    elseif (eq #\[ ch)
	      then (push (compute-tag coll) contents-to-return)
		   (clear-coll coll)
		   (setf state state-begin-dtd)
	    elseif (and (xml-space-p ch) (eq tag-to-return :DOCTYPE))
		   ;; look at tag-to-return and set state accordingly
	      then (push (compute-tag coll) contents-to-return)
		   (clear-coll coll)
		   (setf state state-!-doctype)
	      else (xml-error 
		    (concatenate 'string
		      "illegal name: '"
		      (string tag-to-return)
		      "' in <! tag: "))
		   ))
	  
	  (#.state-!-doctype-ext
	   (if* (xml-name-char-p ch) ;; starting char already passed more restrictive test
	      then
		   (add-to-coll coll ch)
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
		     (if* (eq :SYSTEM token) then (setf state state-!-doctype-system)
		      elseif (eq :PUBLIC token) then (setf state state-!-doctype-public)
			else (xml-error 
			      (concatenate 'string
				"expected 'SYSTEM' or 'PUBLIC' got '"
				(string (first contents-to-return))
				"' in <! tag: " (string tag-to-return) " "
				(string (second contents-to-return))))
			     )
		     )))
	  
	  (#.state-!-doctype-public
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\" ch) then (setf state state-!-doctype-public2)
	    elseif (eq #\' ch) then (setf state state-!-doctype-public3)
	      else (xml-error 
		    (concatenate 'string
		      "expected quote or double-quote got: '"
		      (string ch)
		      "' in <! tag: " (string tag-to-return) " "
		      (string (second contents-to-return)) " "
		      (string (first contents-to-return))
		      ))
		   ))
	  
	  (#.state-!-doctype-system
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\" ch) then (setf state state-!-doctype-system2)
	    elseif (eq #\' ch) then (setf state state-!-doctype-system3)
	      else (xml-error 
		    (concatenate 'string
		      "expected quote or double-quote got: '"
		      (string ch)
		      "' in <! tag: " (string tag-to-return) " "
		      (string (second contents-to-return)) " "
		      (string (first contents-to-return))
		      ))
		   ))
	  
	  (#.state-!-doctype-public2
	   (if* (eq #\" ch) then (push (compute-coll-string coll) 
				       contents-to-return)
		   (clear-coll coll)
		   (setf state state-!-doctype-system)
	    elseif (pub-id-char-p ch) then (add-to-coll coll ch)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error
		    (concatenate 'string
		      "illegal character in DOCTYPE PUBLIC string: '"
		      (compute-coll-string coll) "'"))
		   ))
	  
	  (#.state-!-doctype-public3
	   (if* (eq #\' ch) then (push (compute-coll-string coll)
				       contents-to-return)
		   (clear-coll coll)
		   (setf state state-!-doctype-system)
	    elseif (pub-id-char-p ch) then (add-to-coll coll ch)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error
		    (concatenate 'string
		      "illegal character in DOCTYPE PUBLIC string: '"
		      (compute-coll-string coll) "'"))
		   ))
	  
	  (#.state-!-doctype-system2
	   (when (not (xml-char-p ch))
	     (xml-error "XML is not well formed")) ;; not tested
	   (if* (eq #\" ch) then (push (compute-coll-string coll) 
				       contents-to-return)
		   (clear-coll coll)
		   (setf state state-!-doctype-ext2)
	      else (add-to-coll coll ch)))
	  
	  (#.state-!-doctype-system3
	   (when (not (xml-char-p ch))
	     (xml-error "XML is not well formed")) ;; not tested
	   (if* (eq #\' ch) then (push (compute-coll-string coll)
				       contents-to-return)
		   (clear-coll coll)
		   (setf state state-!-doctype-ext2)
	      else (add-to-coll coll ch)))
	  
	  (#.state-!-doctype-ext2
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\> ch) then (return)
	    elseif (eq #\[ ch)
	      then (setf state state-begin-dtd)
	      else
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error
		    (concatenate 'string
		      "illegal char in DOCTYPE token: '"
		      (compute-coll-string coll) "'"))
		   ))
	  
	  (#.state-!-doctype-ext3
	   (if* (xml-space-p ch) then nil
	    elseif (eq #\> ch) then (return)
	      else
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error
		    (concatenate 'string
		      "illegal char in DOCTYPE token following dtd: '"
		      (compute-coll-string coll) "'"))
		   ))
			   	  
	  (#.state-!-doctype
	   ;; skip whitespace; possible exits: >, SYSTEM, PUBLIC, [
	   (if* (xml-space-p ch) then nil
	    elseif (xml-name-start-char-p ch)
	      then
		   (setf state state-!-doctype-ext)
		   (un-next-char ch)
	    elseif (eq #\> ch) then (return)
	    elseif (eq #\[ ch)
	      then (setf state state-begin-dtd)
	      else (xml-error 
		    (concatenate 'string
		      "illegal character: '"
		      (string ch)
		      "' in <! tag: " (string tag-to-return) " " 
		      (string (first contents-to-return))))
		   ))
	  
	  (#.state-prereadpi
	   (if* (xml-space-p ch)
	      then nil
	    elseif (not (xml-char-p ch))
	      then (xml-error "XML is not well formed") ;; no test
	      else (un-next-char ch)
		   (setf state state-readpi)))
	  
	  (#.state-readpi
	   (if* (eq #\? ch)
	      then (setf state state-readpi2)
	    elseif (not (xml-char-p ch))
	      then (xml-error "XML is not well formed") ;; no test
	      else (add-to-coll coll ch)))
	  
	  (#.state-readpi2
	   (if* (eq #\> ch)
	      then (return)
	    elseif (eq #\? ch) then
		   (add-to-coll coll #\?) ;; come back here to try again
	      else (setf state state-readpi)
		   (add-to-coll coll #\?)
		   (add-to-coll coll ch)))
	  
	  (#.state-findattributename0
	   (if* (xml-space-p ch) then (setf state state-findattributename)
	    elseif (eq ch empty-delim)
	      then (setf state state-noattributename)
	      else 
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error
		    (concatenate 'string
		      "expected space or tag end before: '"
		      (compute-coll-string coll) "'"))))
	  (#.state-findattributename
	   ;; search until we find the start of an attribute name
	   ;; or the end of the tag
	   (if* (eq ch empty-delim)
	      then (setf state state-noattributename)
	    elseif (xml-space-p ch)
	      then nil ;; skip whitespace
	    elseif (xml-name-start-char-p ch)
	      then
		   (un-next-char ch)
		   (setf state state-attribname)
	      else
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char tokenbuf))
		     (if* (null ch)
			then (return)))
		   (xml-error
		    (concatenate 'string
		      "illegal char in <?xml token: '"
		      (compute-coll-string coll) "'"))
		   ))
	  
	  (#.state-attribname
	   ;; collect attribute name
	   (if* (xml-name-char-p ch) ;; starting char already passed more restrictive test
	      then
		   (add-to-coll coll ch)
	    elseif (xml-space-p ch) then
		   (setq attrib-name (compute-tag coll))
		   (clear-coll coll)
		   (setq state state-attribname2)
	      else
		   (when (not (eq #\= ch))
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error
		      (concatenate 'string
			"illegal char in <?xml attribute token: '"
			(compute-coll-string coll) "'"))
		     )
		   (setq attrib-name (compute-tag coll))
		   (clear-coll coll)
		   (setq state state-attribstartvalue)))
	  
	  (#.state-attribname2
	   (if* (eq #\= ch) then (setq state state-attribstartvalue)
	    elseif (xml-space-p ch) then nil
	      else
		   (un-next-char ch)
		   (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error
		      (concatenate 'string
			"illegal char in <?xml attribute token: '"
			(compute-coll-string coll) "'"))))
	  (#.state-attribstartvalue
	   ;; begin to collect value
	   (if* (or (eq ch #\")
		    (eq ch #\'))
	      then (setq value-delim ch)
		   (setq state state-attribvaluedelim)
	    elseif (xml-space-p ch) then nil
	      else
		   (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error
		      (concatenate 'string
			"expected ' or \" before  <?xml attribute token value: '"
			(compute-coll-string coll) "'"))
		   ))
	  
	   (#.state-attribvaluedelim
	    (if* (eq ch value-delim)
	       then (setq attrib-value (compute-coll-string coll))
		    (clear-coll coll)
		    (push attrib-name attribs-to-return)
		    (push attrib-value attribs-to-return)
		    (setq state state-findattributename0)
	     elseif (and (xml-char-p ch) (not (eq #\< ch)))
	       then (add-to-coll coll ch)
	       else 
		    (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char tokenbuf))
		       (if* (null ch)
			  then (return)))
		     (xml-error
		      (concatenate 'string
			"illegal character in attribute token value: '"
			(compute-coll-string coll) "'"))
		    ))
	   
	   (#.state-noattributename
	    (if* (eq #\> ch)
	       then
		    (return) ;; ready to build return token
	       else
		    (xml-error
		     (concatenate 'string
		       "expected '>' found: '" (string ch) "' in <?xml token"))
		    ))
		   
	  (t
	   (error "need to support state:~s" state))
	  ))
      (put-back-collector entity)
      (case state
	(#.state-noattributename ;; it's a bug if this state occurs with a non-empty element
	 (put-back-collector coll)
	 (if* attribs-to-return
		 then (values (cons tag-to-return 
				    (nreverse attribs-to-return))
			      (if (eq tag-to-return :xml) :xml :start-tag) :end-tag)
	    else
		 (values tag-to-return :start-tag :end-tag)
		 ))
	(#.state-readtag-end-bracket
	 ;; this is a :commant tag
	 (let ((ret (compute-coll-string coll)))
	   (put-back-collector coll)
	   (values (cons tag-to-return (list ret)) :comment :nil)))
	(#.state-pcdata
	 (let ((next-char (collector-next coll)))
	   (put-back-collector coll)
	   (if* (zerop next-char)
	      then (values nil :eof nil)
	      else (values (compute-coll-string coll) :pcdata pcdatap))))
	(#.state-readpi2
	 (let ((ret (compute-coll-string coll)))
	   (put-back-collector coll)
	   (values (append (list :pi tag-to-return) (list ret)) :pi nil)))
	((#.state-readtag-!-conditional)
	 (put-back-collector coll)
	 (values (append (list tag-to-return) contents-to-return) :start-tag 
		 :end-tag))
	((#.state-!-contents 
	  #.state-!-doctype
	  #.state-!-doctype-ext2
	  #.state-!-doctype-ext3)
	 (put-back-collector coll)
	 (values (append (list tag-to-return) (nreverse contents-to-return)) :start-tag 
		 :end-tag))
	(#.state-readtag3
	 (put-back-collector coll)
	 (values (if* attribs-to-return
		    then (cons tag-to-return 
			       (nreverse attribs-to-return))
		    else tag-to-return) :start-tag :end-tag))
	((#.state-readtag2
	  #.state-readtag)
	 (put-back-collector coll)
	 (values (if* attribs-to-return
		    then (cons tag-to-return 
			       (nreverse attribs-to-return))
		    else tag-to-return) :start-tag nil))
	((#.state-readtag-end2
	  #.state-readtag-end3)
	 (put-back-collector coll)
	 (values tag-to-return :end-tag nil))
	(#.state-readtag-!-conditional7
	 (let ((ret (compute-coll-string coll)))
	   (put-back-collector coll)
	   (values (append (list :cdata) (list ret)) :cdata nil)))
	(t
	 ;; if ch is null that means we encountered unexpected EOF
	 (when (null ch)
	   (put-back-collector coll)
	   (xml-error "unexpected end of input"))
	 (print (list tag-to-return attribs-to-return))
	 (let ((ret (compute-coll-string coll)))
	   (put-back-collector coll)
	   (error "need to support state <post>:~s  ~s ~s ~s" state 
		  tag-to-return
		  contents-to-return
		  ret))))
      )))

(defun swallow-xml-token (tokenbuf external-callback)
  (declare (ignorable old-coll) (optimize (speed 3) (safety 1)))
  (let ((xml (next-token tokenbuf external-callback nil)))
    (if* (and (eq (fourth xml) :standalone) (stringp (fifth xml))
	      (equal (fifth xml) "yes")) then
	    (xml-error "external XML entity cannot be standalone document")
     elseif (and (eq (sixth xml) :standalone) (stringp (seventh xml))
		 (equal (seventh xml) "yes")) then
	    (xml-error "external XML entity cannot be standalone document"))))

;; return the string with entity references replaced by text
;; normalizing will happen later
;; we're ok on different types - just ignore IMPLIED & REQUIRED; and possibly skip FIXED
(defun parse-default-value (value-list tokenbuf external-callback)
  (declare (optimize (speed 3) (safety 1)))
  (let (value-string)
    (if* (stringp (first value-list)) then (setf value-string (first value-list))
     elseif (eq (first value-list) :FIXED) then (setf value-string (second value-list)))
    (let ((tmp-result (parse-xml
		      (concatenate 'string
			"<item x='"
			value-string
			"'/>")
		      :external-callback external-callback
		      :general-entities
		      (iostruct-general-entities tokenbuf))))
      (if* (stringp (first value-list)) then
	      (setf (first value-list)
		(third (first (first tmp-result))))
	 elseif (eq (first value-list) :FIXED) then
	      (setf (second value-list)
		(third (first (first tmp-result)))))))
  value-list)

(defun process-attlist (args attlist-data)
  (declare (optimize (speed 3) (safety 1)))
  (dolist (arg1 args attlist-data)
    ;;(format t "arg1: ~s~%" arg1)
    (dolist (item (rest arg1))
      ;;(format t "item: ~s~%" item)
      (when (eq :ATTLIST (first item))
	(let* ((name (second item))
	       (name-data (assoc name attlist-data))
	       (new-name-data (rest name-data)))
	  ;;(format t "name: ~s name-data: ~s new-name-data: ~s~%" name name-data new-name-data)
	  (dolist (attrib-data (rest (rest item)))
	    ;;(format t "attrib-data: ~s~%" attrib-data)
	    #+ignore
	    (setf (rest (rest attrib-data))
	      (parse-default-value (rest (rest attrib-data)) tokenbuf external-callback))
	    (when (not (assoc (first attrib-data) new-name-data))
	      (setf new-name-data (acons (first attrib-data) (rest attrib-data) new-name-data))))
	  (if* name-data then
		  (rplacd (assoc name attlist-data) (nreverse new-name-data))
	     else (setf attlist-data (acons name (nreverse new-name-data) attlist-data))))))))

(provide :pxml)