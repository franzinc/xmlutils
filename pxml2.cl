;; $Id: pxml2.cl,v 1.2 2000/07/17 20:03:07 layer Exp $

(in-package :net.xml.parser)

;; todo - compute-tag currently puts symbols in keyword package

;; state titles can be better chosen and explained

(defmethod parse-xml ((str string) &key callback-only external)
  (declare (optimize (speed 3) (safety 1)))
  (parse-xml (make-string-input-stream str) :callback-only callback-only
	     :external external))

(defmethod parse-xml ((p stream) &key callback-only external)
  (declare (optimize (speed 3) (safety 1)))
  (pxml-internal p nil callback-only external))

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

(defun pxml-internal (p read-sequence-func callback-only external)
  (declare (optimize (speed 3) (safety 1)))
  (let ((state state-docstart)
	(tokenbuf (get-tokenbuf))
	(entity-buf)
	(guts)
	(pending)
	(parameter-entities)
	(general-entities)
	)
    (loop 
      (multiple-value-bind (val kind kind2 p-ents g-ents e-buf) 
	  (next-token p nil read-sequence-func 
		      parameter-entities general-entities tokenbuf entity-buf external)
	(setf parameter-entities p-ents)
	(setf general-entities g-ents)
	(setf entity-buf e-buf)
	;;(format t "val: ~s kind: ~s kind2: ~s state: ~s external:~s~%" val kind kind2 state external)
	(case state
	  (#.state-docstart
	   (if* (and (listp val) (eq :xml (first val)) (eq kind :xml) (eq kind2 :end-tag))
	      then
		   (warn "not verifying that xmldecl is well-formed")
		   (push val guts)
		   (setf state state-docstart-misc)
	    elseif (eq kind :comment)
	      then
		   (push val guts)
		   (setf state state-docstart-misc)
	    elseif (and (listp val) (eq :DOCTYPE (first val)))
	      then
		   (when external
		     (xml-error "DOCTYPE not allowed in external subset"))
		   (push val guts)
		   (setf state state-docstart-misc2)
	    elseif (eq kind :pi)
	      then
		   (push val guts)
		   (setf state state-docstart-misc)
	    elseif (eq kind :pcdata)
	      then
		   (when external (setf external :seen-content))
		   (when (and (not external) (not (all-xml-whitespace-p val)))
		     (xml-error (concatenate 'string
				  "unrecognized initial contents: '"
				  (subseq val 0 (min (length val) 40)) "'")))
		   (when external
		     (push val guts)
		     )
		   (setf state state-docstart-misc)
	    elseif (and external (eq kind :eof)) then 
		   (put-back-tokenbuf tokenbuf)
		   (return (nreverse guts))
	    elseif (and (listp val) (eq :external (first val)))
	      then
		   (when (eq external :seen-content)
		     (xml-error "external subset cannot mix content and markup declarations"))
		   (push val guts)
	    elseif (or (symbolp val)
		       (and (listp val) (symbolp (first val))))
	      then
		   (when external (setf external :seen-content))
		   (if* (and (eq kind :start-tag) (eq kind2 :end-tag))
		      then (push (list val) guts)
			   (setf state state-element-done)
		    elseif (eq kind :start-tag)
		      then (push (list val) pending)
			   ;;(format t "pending: ~s guts: ~s <1>~%" pending guts)
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
		   (when external (setf external :seen-content))
		   (when (and (not external) (not (all-xml-whitespace-p val)))
		     (xml-error (concatenate 'string
				  "unrecognized content '"
				  (subseq val 0 (min (length val) 40)) "'")))
		   (when external
		     (push val guts)
		     )
	    elseif (and (listp val) (eq :comment (first val)))
	      then
		   (push val guts)
	    elseif (eq kind :pi)
	      then
		   (push val guts)
	    elseif (and external (eq kind :eof)) then 
		   (put-back-tokenbuf tokenbuf)
		   (return (nreverse guts))
	    elseif (eq kind :eof)
	      then
		   (xml-error "unexpected end of file encountered")
	    elseif (or (symbolp val)
		       (and (listp val) (symbolp (first val))))
	      then
		   (when external (setf external :seen-content))
		   (if* (and (eq kind :start-tag) (eq kind2 :end-tag))
		      then (push (list val) guts)
			   (setf state state-element-done)
		    elseif (eq kind :start-tag)
		      then (push (list val) pending)
			   ;;(format t "pending: ~s guts: ~s <2>~%" pending guts)
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
		   (when external (setf external :seen-content))
		   (when (and (not external) (not (all-xml-whitespace-p val)))
		     (xml-error (concatenate 'string
				  "unrecognized content '"
				  (subseq val 0 (min (length val) 40)) "'")))
		   (when external
		     (push val guts)
		     )
	    elseif (and (listp val) (eq :DOCTYPE (first val)))
	      then
		   (push val guts)
		   (setf state state-docstart-misc2)
		   (when external
		     (xml-error "DOCTYPE not allowed in external subset"))
	    elseif (and (listp val) (eq :comment (first val)))
	      then
		   (push val guts)
	    elseif (and (listp val) (eq :external (first val)))
	      then
		   (when (eq external :seen-content)
		     (xml-error "external subset cannot mix content and markup declarations"))
		   (push val guts)
	    elseif (eq kind :pi)
	      then
		   (push val guts)
	    elseif (and external (eq kind :eof)) then 
		   (put-back-tokenbuf tokenbuf)
		   (return (nreverse guts))
	    elseif (or (symbolp val)
		       (and (listp val) (symbolp (first val))))
	      then
		   (when external (setf external :seen-content))
		   (if* (and (eq kind :start-tag) (eq kind2 :end-tag))
		      then (push (list val) guts)
			   (setf state state-element-done)
			   elseif (eq kind :start-tag)
		      then (push (list val) pending)
			   ;;(format t "pending: ~s guts: ~s <3>~%" pending guts)
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
	   (when external (setf external :seen-content))
	   (if* (or (symbolp val)
		    (and (listp val) (symbolp (first val))))
	      then (if* (eq kind :end-tag)
		      then (let ((candidate (first (first pending))))
			     (when (listp candidate) (setf candidate (first candidate)))
			     (if* (eq candidate val)
				then 
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
		      then (push (list val) pending)
			   ;;(format t "pending: ~s guts: ~s <6>~%" pending guts)
		    elseif (or (eq kind :cdata) (eq kind :pi) (eq kind :comment))
		      then
			   (setf (first pending)
			     (append (first pending) (list val)))
		      else (error "unexpected token: ~s <1>" val))
	    elseif (eq kind :pcdata)
	      then (dotimes (i (- (length val) 2) nil)
		     (when (and (eq #\] (elt val i))
				(eq #\] (elt val (+ i 1)))
				(eq #\> (elt val (+ i 2))))
		       (xml-error "']]>' not allowed in element content characters")))
		   (setf (first pending)
		     (append (first pending) (list val)))
	      else (error "unexpected token: ~s <2>" val)))
	  (#.state-element-done
	   (if* (eq kind :pcdata)
	      then
		   (when (and (not external) (not (all-xml-whitespace-p val)))
		     (xml-error (concatenate 'string
				  "character data following element section '"
				  (subseq val 0 (min (length val) 40)) "'")))
		   (when external
		     (push val guts)
		     )
	    elseif (eq kind :eof) then 
		   (put-back-tokenbuf tokenbuf)
		   (return (nreverse guts))
	    elseif (and external
			(or (symbolp val)
			    (and (listp val) (symbolp (first val)))))
	      then
		   (when external (setf external :seen-content))
		   (if* (and (eq kind :start-tag) (eq kind2 :end-tag))
		      then (push (list val) guts)
			   (setf state state-element-done)
		    elseif (eq kind :start-tag)
		      then (push (list val) pending)
			   ;;(format t "pending: ~s guts: ~s <1>~%" pending guts)
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
	    elseif (or (eq kind :comment) (eq kind :pi))
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
  (defconstant state-readtag-!-conditional2 50)
  (defconstant state-readtag-!-conditional3 51)
  (defconstant state-readtag-!-conditional4 52)
  (defconstant state-readtag-!-conditional5 53)
  (defconstant state-readtag-!-conditional6 54)
  (defconstant state-readtag-!-conditional7 55)
  )

(defun next-token (stream ignore-strings read-sequence-func
		   parameter-entities general-entities tokenbuf entity-buf external)
  (declare (optimize (speed 3) (safety 1)))
  ;; return two values: 
  ;;    the next token from the stream.
  ;; 	the kind of token
  ;;
  ;; if read-sequence-func is non-nil,
  ;; read-sequence-func is called to fetch the next character
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
			  
	     
	     (un-next-char (stream ch)
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
    
    (let ((state state-pcdata)
	  (coll  (get-collector))
	  (entity  (get-collector))
	  (tag-to-return)
	  (attrib-name)
	  (empty-delim)
	  (value-delim)
	  (attrib-value)
	  (attribs-to-return)
	  (contents-to-return)
	  (char-code 0)
	  (special-tag-count 0)
	  (ch))
      
      (loop

	(setq ch (get-next-char stream))
	;;(format t "ch: ~s state:~s~%" ch state)
	(if* (null ch)
	   then (return) ; eof -- exit loop
		)
      
      
	(case state
	  (#.state-pcdata
	  (if* (eq ch #\<)
	      then
		   (if* (> (collector-next coll) 0)
		      then ; have collected something, return this string
			   (un-next-char stream ch) ; push back the <
			   (return)
		      else ; collect a tag
			   (setq state state-readtagfirst))
	   elseif (eq #\& ch)
	     then (setf state state-pcdata2)
	      else
		   (if* (not (eq ch #\return))
		      then (add-to-coll coll ch))))
	  
	  (#.state-pcdata2
	   (if* (eq #\# ch)
	      then (setf state state-pcdata3)
	    elseif (xml-name-start-char-p ch)
	      then (setf state state-pcdata4)
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
				"'"))
		   ))
	  
	  (#.state-pcdata3
	   (if* (eq #\x ch)
	      then (setf state state-pcdata5)
	    elseif (<= (char-code #\0) (char-code ch) (char-code #\9))
	      then (setf state state-pcdata6)
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
	  
	  (#.state-pcdata4
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
			else (let ((p-value (assoc entity-symbol general-entities)))
			       (if* p-value then
				       (setf p-value (rest p-value))
				       (let* ((buf (get-tokenbuf))
					      (tb (tokenbuf-data buf)))
					 (setf (tokenbuf-max buf) (length p-value))
					 (dotimes (i (length p-value))
					   (setf (schar tb i) (char p-value i)))
					 (push buf entity-buf))
				       else
				       (let ((entity-string (format nil "~s" entity-symbol)))
					 (dotimes (i (length entity-string))
					   (if* (= i 0)
					      then (add-to-coll coll #\&)
					      else (add-to-coll coll (elt entity-string i))))
					 (add-to-coll coll #\;)
					 )
				       ))
			     ))
		   (setq state state-pcdata)
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
	  
	  (#.state-pcdata5
	   (let ((code (char-code ch)))
	     (if* (eq #\; ch)
		then (add-to-coll coll (code-char char-code))
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
		       (setq ch (get-next-char stream))
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
		then (add-to-coll coll (code-char char-code))
		     (setf char-code 0)
		     (setq state state-pcdata)
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
	  
	  (#.state-readtag-end
	   (if* (xml-name-start-char-p ch)
	      then (setf state state-readtag-end2)
		   (un-next-char stream ch)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
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
		   (setq tag-to-return (compute-tag coll *package*))
		   (return)
	    elseif (xml-space-p ch) then (setf state state-readtag-end3)
		   (setq tag-to-return (compute-tag coll *package*))
	      else (let ((tmp (compute-coll-string coll)))
		     (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char stream))
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
		       (setq ch (get-next-char stream))
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
		   (un-next-char stream ch)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
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
		   (un-next-char stream ch)
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
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error (concatenate 'string
				"illegal character following '<!', starting at '<!"
				(compute-coll-string coll)
				"'"))
		   ))
	  
	  (#.state-readtag-!-conditional
	   (if* (xml-space-p ch)
	      then (setf state state-readtag-!-conditional2)
	    elseif (eq #\I ch) then
		   (setf state state-readtag-!-conditional3)
	    elseif (eq #\C ch) then
		   (setf state state-readtag-!-conditional4)
		   (setf special-tag-count 1)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
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
		     (setq ch (get-next-char stream))
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
	      else (add-to-coll coll ch)))
	  
	  (#.state-readtag-!-conditional6
	   (if* (eq #\] ch)
	      then (setf state state-readtag-!-conditional7)
	      else (setf state state-readtag-!-conditional5)
		   (add-to-coll coll #\])
		   (add-to-coll coll ch)))
	  
	  (#.state-readtag-!-conditional7
	   (if* (eq #\> ch)
	      then (return)
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
		     (setq ch (get-next-char stream))
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
	      else (add-to-coll coll ch)))
	  
	  (#.state-readtag-!-readcomment2
	   (if* (eq #\- ch)
	      then (setf state state-readtag-end-bracket)
	      else (setf state state-readtag-!-readcomment)
		   (add-to-coll coll #\-) (add-to-coll coll ch)))
	  
	  (#.state-readtag-end-bracket
	   (if* (eq #\> ch)
	      then (return)
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
	  
	  (#.state-readtag
	   (if* (xml-name-char-p ch) ;; starting char already passed more restrictive test
	      then
		   (add-to-coll coll ch)
	      else
		   (if* (xml-space-p ch) then
			   (setq tag-to-return 
			     (compute-tag coll *package*))
			   (clear-coll coll)
			   (setf state state-readtag2)
		    elseif (eq #\> ch) then 
			   (setq tag-to-return 
			     (compute-tag coll *package*))
			   (clear-coll coll)
			   (return)
		    elseif (eq #\/ ch) then 
			   (setq tag-to-return 
			     (compute-tag coll *package*))
			   (clear-coll coll)
			   (setf state state-readtag3)
		      else (dotimes (i 15)
			     (add-to-coll coll ch)
			     (setq ch (get-next-char stream))
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
		   (un-next-char stream ch)
		   (setf state state-readtag4)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
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
		   (setq attrib-name (compute-tag coll *package*))
		   (clear-coll coll)
		   (setf state state-readtag5)
	      else (let ((tmp (compute-coll-string coll)))
		     (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char stream))
		       (if* (null ch)
			  then (return)))
		     (xml-error 
		      (concatenate 'string
			"looking for attribute '=', found: '"
		      (compute-coll-string coll)
		      "' following attribute name: " tmp)))
		   ))
	  
	  (#.state-readtag5
	   ;; begin to collect attribute value
	   (if* (or (eq ch #\")
		    (eq ch #\'))
	      then (setq value-delim ch)
		   (setq state state-readtag6)
	      else
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error 
		    (concatenate 'string
		      "attribute value not delimited by ' or \" : '"
		      (compute-coll-string coll)
		      "' following attribute: " (string attrib-name)))
		   ))
	  
	  (#.state-readtag6
	   (if* (eq ch value-delim)
	       then (setq attrib-value (compute-coll-string coll))
		    (clear-coll coll)
		    (push attrib-name attribs-to-return)
		    (push attrib-value attribs-to-return)
		   (setq state state-readtag2)
	    elseif (eq #\& ch)
	      then (setq state state-readtag7)
	     elseif (and (xml-char-p ch) (not (eq #\< ch)))
	       then (add-to-coll coll ch)
	       else 
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error 
		    (concatenate 'string
		      "attribute value cannot contain '<': '"
		      (compute-coll-string coll)
		      "' following attribute: " (string attrib-name)))
		   ))
	  
	  (#.state-readtag7
	   (if* (eq #\# ch)
	      then (setf state state-readtag8)
	    elseif (xml-name-start-char-p ch)
	      then (setf state state-readtag9)
		   (un-next-char stream ch)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
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
		   (un-next-char stream ch)
	      else (clear-coll coll)
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
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
		then (add-to-coll coll (code-char char-code))
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
		       (setq ch (get-next-char stream))
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
		then (add-to-coll coll (code-char char-code))
		     (setf char-code 0)
		     (setq state state-readtag6)
	      elseif (<= (char-code #\0) code (char-code #\9))
		then (setf char-code (+ (* char-code 10) (- code (char-code #\0))))
		else (clear-coll coll)
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char stream))
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
		     (if* (eq entity-symbol :amp) then (add-to-coll coll #\&)
		      elseif (eq entity-symbol :lt) then (add-to-coll coll #\<)
		      elseif (eq entity-symbol :gt) then (add-to-coll coll #\>)
		      elseif (eq entity-symbol :apos) then (add-to-coll coll #\')
		      elseif (eq entity-symbol :quot) then (add-to-coll coll #\")
			else (let ((p-value (assoc entity-symbol general-entities)))
			       (if* p-value then
				       (setf p-value (rest p-value))
				       (let* ((buf (get-tokenbuf))
					      (tb (tokenbuf-data buf)))
					 (setf (tokenbuf-max buf) (length p-value))
					 (dotimes (i (length p-value))
					   (setf (schar tb i) (char p-value i)))
					 (push buf entity-buf))
				       else
				       (let ((entity-string (format nil "~s" entity-symbol)))
					 (dotimes (i (length entity-string))
					   (if* (= i 0)
					      then (add-to-coll coll #\&)
					      else (add-to-coll coll (elt entity-string i))))
					 (add-to-coll coll #\;)
					 )
				       ))
			     ))
		   (setq state state-readtag6)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
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
		     (setq ch (get-next-char stream))
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
			   (setq tag-to-return :xml)
			   (setf state state-findattributename)
		      else
			   (setq tag-to-return (compute-tag coll))
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
	      else (un-next-char stream ch)
		   (setf state state-!-contents)))
	  
	  (#.state-begin-dtd
	   (un-next-char stream ch)
	   (multiple-value-bind (dtd-val p-ents g-ents e-buf)
	       (parse-dtd stream ignore-strings read-sequence-func
			  parameter-entities general-entities tokenbuf entity-buf
			  external)
	     (push (append (list :[) dtd-val)
		   contents-to-return)
	     (setf entity-buf e-buf)
	     (setf parameter-entities p-ents)
	     (setf general-entities g-ents))
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
	    elseif (xml-space-p ch)
		   ;; look at tag-to-return and set state accordingly
	      then (if* (eq tag-to-return :DOCTYPE)
		      then (push (compute-tag coll) contents-to-return)
			   (clear-coll coll)
			   (setf state state-!-doctype)
		      else
			   (error "need to support ~s ! type" tag-to-return))
	    elseif external then
		   (dotimes (i (+ 4 (length (string tag-to-return))))
		     (un-next-char stream ch))
		   (setf tag-to-return :external)
		   (multiple-value-bind (dtd-val p-ents g-ents e-buf)
		       (parse-dtd stream ignore-strings read-sequence-func
				  parameter-entities general-entities tokenbuf entity-buf
				  external)
		     (setf contents-to-return dtd-val)
		     (setf entity-buf e-buf)
		     (setf parameter-entities p-ents)
		     (setf general-entities g-ents))
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
	   (if* (eq #\" ch) then (push (compute-coll-string coll) contents-to-return)
		   (clear-coll coll)
		   (setf state state-!-doctype-system)
	    elseif (pub-id-char-p ch) then (add-to-coll coll ch)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
		     (if* (null ch)
			then (return)))
		   (xml-error
		    (concatenate 'string
		      "illegal character in DOCTYPE PUBLIC string: '"
		      (compute-coll-string coll) "'"))
		   ))
	  
	  (#.state-!-doctype-public3
	   (if* (eq #\' ch) then (push (compute-coll-string coll) contents-to-return)
		   (clear-coll coll)
		   (setf state state-!-doctype-system)
	    elseif (pub-id-char-p ch) then (add-to-coll coll ch)
	      else (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
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
	   (if* (eq #\" ch) then (push (compute-coll-string coll) contents-to-return)
		   (clear-coll coll)
		   (setf state state-!-doctype-ext2)
	      else (add-to-coll coll ch)))
	  
	  (#.state-!-doctype-system3
	   (when (not (xml-char-p ch))
	     (xml-error "XML is not well formed")) ;; not tested
	   (if* (eq #\' ch) then (push (compute-coll-string coll) contents-to-return)
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
		     (setq ch (get-next-char stream))
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
		     (setq ch (get-next-char stream))
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
		   (un-next-char stream ch)
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
	      else (un-next-char stream ch)
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
	      else (setf state state-readpi)
		   (add-to-coll coll #\?)
		   (add-to-coll coll ch)))
	  
	  (#.state-findattributename
	   ;; search until we find the start of an attribute name
	   ;; or the end of the tag
	   (if* (eq ch empty-delim)
	      then (setf state state-noattributename)
	    elseif (xml-space-p ch)
	      then nil ;; skip whitespace
	    elseif (xml-name-start-char-p ch)
	      then
		   (un-next-char stream ch)
		   (setf state state-attribname)
	      else
		   (dotimes (i 15)
		     (add-to-coll coll ch)
		     (setq ch (get-next-char stream))
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
	      else
		   (when (not (eq #\= ch))
		     (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char stream))
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
	  
	  (#.state-attribstartvalue
	   ;; begin to collect value
	   (if* (or (eq ch #\")
		    (eq ch #\'))
	      then (setq value-delim ch)
		   (setq state state-attribvaluedelim)
	      else
		   (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char stream))
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
		    (setq state state-findattributename)
	     elseif (and (xml-char-p ch) (not (eq #\< ch)))
	       then (add-to-coll coll ch)
	       else 
		    (dotimes (i 15)
		       (add-to-coll coll ch)
		       (setq ch (get-next-char stream))
		       (if* (null ch)
			  then (return)))
		     (xml-error
		      (concatenate 'string
			"illegal character in <?xml attribute token value: '"
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
			      (if (eq tag-to-return :xml) :xml :start-tag) :end-tag
			      parameter-entities general-entities entity-buf)
	    else
		 (values tag-to-return :start-tag :end-tag
			 parameter-entities general-entities entity-buf)
		 ))
	(#.state-readtag-end-bracket
	 (if* (eq tag-to-return :comment)
	    then (let ((ret (compute-coll-string coll)))
		   (put-back-collector coll)
		   (values (cons tag-to-return (list ret)) :comment :nil
			   parameter-entities general-entities entity-buf))
	    else (put-back-collector coll)
		 (error "have to support end bracket tag: ~s" tag-to-return)))
	(#.state-pcdata
	 (let ((next-char (collector-next coll)))
	   (put-back-collector coll)
	   (if* (zerop next-char)
	      then (values nil :eof nil parameter-entities general-entities entity-buf)
	      else (values (compute-coll-string coll) :pcdata nil
			   parameter-entities general-entities entity-buf))))
	(#.state-readpi2
	 (let ((ret (compute-coll-string coll)))
	   (put-back-collector coll)
	   (values (append (list :pi tag-to-return) (list ret)) :pi nil
		   parameter-entities general-entities entity-buf)))
	((#.state-!-contents 
	  #.state-!-doctype
	  #.state-!-doctype-ext2
	  #.state-!-doctype-ext3)
	 (put-back-collector coll)
	 (values (append (list tag-to-return) (nreverse contents-to-return)) :start-tag 
		 :end-tag parameter-entities general-entities entity-buf))
	(#.state-readtag3
	 (put-back-collector coll)
	 (values (if* attribs-to-return
		    then (cons tag-to-return 
			       (nreverse attribs-to-return))
		    else tag-to-return) :start-tag :end-tag
		    parameter-entities general-entities entity-buf))
	((#.state-readtag2
	  #.state-readtag)
	 (put-back-collector coll)
	 (values (if* attribs-to-return
		    then (cons tag-to-return 
			       (nreverse attribs-to-return))
		    else tag-to-return) :start-tag nil
		    parameter-entities general-entities entity-buf))
	((#.state-readtag-end2
	  #.state-readtag-end3)
	 (put-back-collector coll)
	 (values tag-to-return :end-tag nil
		 parameter-entities general-entities entity-buf))
	(#.state-readtag-!-conditional7
	 (let ((ret (compute-coll-string coll)))
	   (put-back-collector coll)
	   (values (append (list :cdata) (list ret)) :cdata nil
		   parameter-entities general-entities entity-buf)))
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


