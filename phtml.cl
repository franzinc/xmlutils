;; phtml.cl  - parse html


; do character entity stuff
;

(defpackage net.html.parser
  (:use :lisp :clos :excl)
  (:export
   #:phtml-internal
   #:parse-html))

(in-package :net.html.parser)

(defmacro tag-auto-close (tag) `(get ,tag 'tag-auto-close))
(defmacro tag-auto-close-stop (tag) `(get ,tag 'tag-auto-close-stop))
(defmacro tag-no-end (tag) `(get ,tag 'tag-no-end))

; only subelements allowed in this element, no strings
(defmacro tag-no-pcdata (tag) `(get ,tag 'tag-no-pcdata))

;; given :foo or (:foo ...) return :foo
(defmacro tag-name (expr)
  `(let ((.xx. ,expr))
     (if* (consp .xx.)
	then (car .xx.)
	else .xx.)))





(eval-when (compile load eval)
  (defconstant state-pcdata 0) ; scanning for chars or a tag
  (defconstant state-readtagfirst 1)
  (defconstant state-readtag      2)
  (defconstant state-findattribname 3)
  (defconstant state-attribname    4)
  (defconstant state-attribstartvalue 5)
  (defconstant state-attribvaluedelim 6)
  (defconstant state-attribvaluenodelim 7)
  (defconstant state-readcomment 8)
  (defconstant state-readcomment-one 9)
  (defconstant state-readcomment-two 10)
  (defconstant state-findvalue 11)
  (defconstant state-rawdata 12)
)


(defstruct collector 
  next  ; next index to set
  max   ; 1+max index to set
  data  ; string vector
  )

;; keep a cache of collectors on this list

(defparameter *collectors* (list nil nil nil nil))

(defun get-collector ()
  (declare (optimize (speed 3) (safety 1)))
  (let (col)
    (mp::without-scheduling
      (do* ((cols *collectors* (cdr cols))
	    (this (car cols) (car cols)))
	  ((null cols))
	(if* this
	   then (setf (car cols) nil)
		(setq col this)
		(return))))
    (if*  col
       then (setf (collector-next col) 0)
	    col
       else (make-collector
	     :next 0
	     :max  100
	     :data (make-string 100)))))

(defun put-back-collector (col)
  (declare (optimize (speed 3) (safety 1)))
  (mp::without-scheduling 
    (do ((cols *collectors* (cdr cols)))
	((null cols)
	 ; toss it away
	 nil)
      (if* (null (car cols))
	 then (setf (car cols) col)
	      (return)))))
	 


(defun grow-and-add (coll ch)
  (declare (optimize (speed 3) (safety 1)))
  ;; increase the size of the data portion of the collector and then
  ;; add the given char at the end
  (let* ((odata (collector-data coll))
	 (ndata (make-string (* 2 (length odata)))))
    (dotimes (i (length odata))
      (setf (schar ndata i) (schar odata i)))
    (setf (collector-data coll) ndata)
    (setf (collector-max coll) (length ndata))
    (let ((next (collector-next coll)))
      (setf (schar ndata next) ch)
      (setf (collector-next coll) (1+ next)))))

	 


    
  
  
;; character characteristics
(defconstant char-tagcharacter   1) ; valid char for a tag
(defconstant char-attribnamechar 2) ; valid char for an attribute name
(defconstant char-attribundelimattribvalue 4) ; valid for undelimited value
(defconstant char-spacechar 8)

(defparameter *characteristics* 
    ;; array of bits describing character characteristics
    (let ((arr (make-array 128 :initial-element 0)))
      (declare (optimize (speed 3) (safety 1)))
      (macrolet ((with-range ((var from to) &rest body)
		   `(do ((,var (char-code ,from) (1+ ,var))
			 (mmax  (char-code ,to)))
			((> ,var mmax))
		      ,@body))
		 
		 (addit (index charistic)
		   `(setf (svref arr ,index)
		      (logior (svref arr ,index)
			      ,charistic)))
		 )
	
	(with-range (i #\A #\Z)
	    (addit i (+ char-tagcharacter
	       char-attribnamechar
	       char-attribundelimattribvalue)))
	
	(with-range (i #\a #\z)
	    (addit i (+ char-tagcharacter
	       char-attribnamechar
	       char-attribundelimattribvalue)))
		      
	(with-range (i #\0 #\9)
	    (addit i (+ char-tagcharacter
	       char-attribnamechar
	       char-attribundelimattribvalue)))
	
	;; let colon be legal tag character
	(addit (char-code #\:) char-tagcharacter)
	
	; now the unusual cases
	(addit (char-code #\-) char-attribundelimattribvalue)
	(addit (char-code #\.) char-attribundelimattribvalue)
	
	;; adding all typeable chars except for whitespace and >
	(addit (char-code #\:) char-attribundelimattribvalue)
	(addit (char-code #\@) char-attribundelimattribvalue)
	(addit (char-code #\/) char-attribundelimattribvalue)
	(addit (char-code #\!) char-attribundelimattribvalue)
	(addit (char-code #\#) char-attribundelimattribvalue)
	(addit (char-code #\$) char-attribundelimattribvalue)
	(addit (char-code #\%) char-attribundelimattribvalue)
	(addit (char-code #\^) char-attribundelimattribvalue)
	(addit (char-code #\&) char-attribundelimattribvalue)
	(addit (char-code #\() char-attribundelimattribvalue)
	(addit (char-code #\)) char-attribundelimattribvalue)
	(addit (char-code #\_) char-attribundelimattribvalue)
	(addit (char-code #\=) char-attribundelimattribvalue)
	(addit (char-code #\+) char-attribundelimattribvalue)
	(addit (char-code #\\) char-attribundelimattribvalue)
	(addit (char-code #\|) char-attribundelimattribvalue)
	(addit (char-code #\{) char-attribundelimattribvalue)
	(addit (char-code #\}) char-attribundelimattribvalue)
	(addit (char-code #\[) char-attribundelimattribvalue)
	(addit (char-code #\]) char-attribundelimattribvalue)
	(addit (char-code #\;) char-attribundelimattribvalue)
	(addit (char-code #\') char-attribundelimattribvalue)
	(addit (char-code #\") char-attribundelimattribvalue)
	(addit (char-code #\,) char-attribundelimattribvalue)
	(addit (char-code #\<) char-attribundelimattribvalue)
	(addit (char-code #\?) char-attribundelimattribvalue)
	
	; i'm not sure what can be in a tag name but we know that
	; ! and - must be there since it's used in comments
	
	(addit (char-code #\-) char-tagcharacter)
	(addit (char-code #\!) char-tagcharacter)
	
	; spaces
	(addit (char-code #\space) char-spacechar)
	(addit (char-code #\tab) char-spacechar)
	(addit (char-code #\return) char-spacechar)
	(addit (char-code #\linefeed) char-spacechar)
	
	)
      
      
      
      arr))
	

(defun char-characteristic (char bit)
  (declare (optimize (speed 3) (safety 1)))
  ;; return true if the given char has the given bit set in 
  ;; the characteristic array
  (let ((code (char-code char)))
    (if* (<= 0 code 127)
       then ; in range
	    (not (zerop (logand (svref *characteristics* code) bit))))))


(defstruct tokenbuf
  cur ;; next index to use to grab from tokenbuf
  max ;; index one beyond last character
  data ;; character array
  )

;; cache of tokenbuf structs
(defparameter *tokenbufs* (list nil nil nil nil))

(defun get-tokenbuf ()
  (declare (optimize (speed 3) (safety 1)))
  (let (buf)
    (mp::without-scheduling
      (do* ((bufs *tokenbufs* (cdr bufs))
	    (this (car bufs) (car bufs)))
	  ((null bufs))
	(if* this
	   then (setf (car bufs) nil)
		(setq buf this)
		(return))))
    (if* buf
       then (setf (tokenbuf-cur buf) 0)
	    (setf (tokenbuf-max buf) 0)
	    buf
       else (make-tokenbuf
	     :cur 0
	     :max  0
	     :data (make-array 1024 :element-type 'character)))))

(defun put-back-tokenbuf (buf)
  (declare (optimize (speed 3) (safety 1)))
  (mp::without-scheduling 
    (do ((bufs *tokenbufs* (cdr bufs)))
	((null bufs)
	 ; toss it away
	 nil)
      (if* (null (car bufs))
	 then (setf (car bufs) buf)
	      (return)))))


    
    
(defun next-token (stream ignore-strings raw-mode-delimiter
		   read-sequence-func tokenbuf)
  (declare (optimize (speed 3) (safety 1)))
  ;; return two values: 
  ;;    the next token from the stream.
  ;; 	the kind of token (:pcdata, :start-tag, :end-tag, :eof)
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
			  
	     
	     (un-next-char (stream ch)
	       `(decf (tokenbuf-cur tokenbuf)))
	     
	     (clear-coll (coll)
	       `(setf (collector-next coll) 0))
		     
	     (add-to-coll (coll ch)
	       `(let ((.next. (collector-next ,coll)))
		  (if* (>= .next. (collector-max ,coll))
		     then (grow-and-add ,coll ,ch)
		     else (setf (schar (collector-data ,coll) .next.)
			    ,ch)
			  (setf (collector-next ,coll) (1+ .next.)))))
	     
	     (to-preferred-case (ch)
	       ;; should check the case mode
	       (if* (eq excl:*current-case-mode* :CASE-INSENSITIVE-UPPER)
		       then `(char-upcase ,ch)
		       else `(char-downcase ,ch)))
	       
	     )
    
    (let ((state (if* raw-mode-delimiter then state-rawdata else state-pcdata))
	  (coll  (get-collector))
	  (ch)

	  (value-delim)
	  
	  (tag-to-return)
	  (attribs-to-return)
	  
	  (end-tag)
	  
	  (attrib-name)
	  (attrib-value)
	  
	  (name-length 0) ;; count only when it could be a comment
	  
	  (raw-length 0)
          (xml-bailout)
	  )
    
      (loop
      
	(setq ch (next-char stream))
	;;(format t "ch: ~s state: ~s~%" ch state)
      
	(if* (null ch)
	   then (return) ; eof -- exit loop
		)
      
      
	(case state
	  (#.state-pcdata
	   ; collect everything until we see a <
	   (if* (eq ch #\<)
	      then ; if we've collected nothing then get a tag 
		   (if* (> (collector-next coll) 0)
		      then ; have collected something, return this string
			   (un-next-char stream ch) ; push back the <
			   (return)
		      else ; collect a tag
			   (setq state state-readtagfirst))
	      else ; we will check for & here eventually
		   (if* (not (eq ch #\return))
		      then (add-to-coll coll ch))))
	
	  (#.state-readtagfirst
	   ; starting to read a tag name
	   (if* (eq #\/ ch)
	      then ; end tag
		   (setq end-tag t)
	      else (if* (eq #\! ch) ; possible comment
		      then (setf xml-bailout t)
			   (setq name-length 0))
		   (un-next-char stream ch))
	   (setq state state-readtag))
	
	  (#.state-readtag
	   ;; reading the whole tag name
	   (if* (char-characteristic ch char-tagcharacter)
	      then (add-to-coll coll (to-preferred-case ch))
		   (incf name-length)
		   (if* (and (eq name-length 3)
			     (coll-has-comment coll))
		      then (clear-coll coll)
			   (setq state state-readcomment))
			   
	      else (setq tag-to-return (compute-tag coll))
		   (clear-coll coll)
		   (if* (eq ch #\>)
		      then (return)	; we're done
		    elseif xml-bailout then (return)
		      else (if* (eq tag-to-return :!--)
			      then ; a comment
				   (setq state state-readcomment)
			      else (setq state state-findattribname)))))
	
	  (#.state-findattribname
	   ;; search until we find the start of an attribute name
	   ;; or the end of the tag
	   (if* (eq ch #\>)
	      then ; end of the line
		   (return)
	    elseif (eq ch #\=)
	      then ; value for previous attribute name
		   ; (syntax  "foo = bar" is bogus I think but it's
		   ; used some places, here is where we handle this
		   (pop attribs-to-return)
		   (setq attrib-name (pop attribs-to-return))
		   (setq state state-findvalue)
	    elseif (char-characteristic ch char-attribnamechar)
	      then (un-next-char stream ch)
		   (setq state state-attribname)
	      else nil ; ignore other things
		   ))
	  
	  (#.state-findvalue
	   ;; find the start of the value
	   (if* (char-characteristic ch char-spacechar)
	      thenret ; keep looking
	    elseif (eq ch #\>)
	      then ; no value, set the value to be the
		   ; name as a string
		   (setq attrib-value 
		     (string-downcase (string attrib-name)))
		   
		   (push attrib-name attribs-to-return)
		   (push attrib-value attribs-to-return)
		   (un-next-char stream ch)
		   (setq state state-findattribname)
	      else (un-next-char stream ch)
		   (setq state state-attribstartvalue)))
	   
	
	  (#.state-attribname
	   ;; collect attribute name

	   (if* (char-characteristic ch char-attribnamechar)
	      then (add-to-coll coll (to-preferred-case ch))
	    elseif (eq #\= ch)
	      then ; end of attribute name, value is next
		   (setq attrib-name (compute-tag coll))
		   (clear-coll coll)
		   (setq state state-attribstartvalue)
	      else ; end of attribute name with no value, 
		   (setq attrib-name (compute-tag coll))
		   (clear-coll coll)
		   (setq attrib-value 
		     (string-downcase (string attrib-name)))
		   (push attrib-name attribs-to-return)
		   (push attrib-value attribs-to-return)
		   (un-next-char stream ch)
		   (setq state state-findattribname)))
	
	  (#.state-attribstartvalue
	   ;; begin to collect value
	   (if* (or (eq ch #\")
		    (eq ch #\'))
	      then (setq value-delim ch)
		   (setq state state-attribvaluedelim)
	      else (un-next-char stream ch)
		   (setq state state-attribvaluenodelim)))
	
	  (#.state-attribvaluedelim
	   (if* (eq ch value-delim)
	      then (setq attrib-value (compute-coll-string coll))
		   (clear-coll coll)
		   (push attrib-name attribs-to-return)
		   (push attrib-value attribs-to-return)
		   (setq state state-findattribname)
	      else (add-to-coll coll ch)))
	
	  (#.state-attribvaluenodelim
	   ;; an attribute value not delimited by ' or " and thus restricted
	   ;; in the possible characters
	   (if* (char-characteristic ch char-attribundelimattribvalue)
	      then (add-to-coll coll ch)
	      else (un-next-char stream ch)
		   (setq attrib-value (compute-coll-string coll))
		   (clear-coll coll)
		   (push attrib-name attribs-to-return)
		   (push attrib-value attribs-to-return)
		   (setq state state-findattribname)))
	  
	  (#.state-readcomment
	   ;; a comment ends on the first --, but we'll look for -->
	   ;; since that's what most people expect
	   (if* (eq ch #\-)
	      then (setq state state-readcomment-one)
	      else (add-to-coll coll ch)))
	  
	  (#.state-readcomment-one
	   ;; seen one -, looking for ->
	   
	   (if* (eq ch #\-)
	      then (setq state state-readcomment-two)
	      else ; not a comment end, put back the -'s
		   (add-to-coll coll #\-)
		   (add-to-coll coll ch)
		   (setq state state-readcomment)))
	  
	  (#.state-readcomment-two
	   ;; seen two -'s, looking for >
	   
	   (if* (eq ch #\>)
	      then ; end of the line
		   (return)
	    elseif (eq ch #\-)
	      then ; still at two -'s, have to put out first
		   (add-to-coll coll #\-)
	      else ; put out two hypens and back to looking for a hypen
		   (add-to-coll coll #\-)
		   (add-to-coll coll #\-)
		   (setq state state-readcomment)))
	  
	  (#.state-rawdata
	   ;; collect everything until we see the delimiter
	   (if* (eq (to-preferred-case ch) (elt raw-mode-delimiter raw-length))
	      then
		   (incf raw-length)
		   (when (= raw-length (length raw-mode-delimiter))
		     ;; push the end tag back so it can then be lexed
		     ;; but don't do it for xml stuff
		     (when (/= (length  raw-mode-delimiter) 1)
		       (dotimes (i (length raw-mode-delimiter))
			 (un-next-char stream 
				       ch
				       #+ignore ;; un-next-char doesn't
				       ;; use args, so save time by not doing the
				       ;; char manipulation
				       (elt raw-mode-delimiter
					    (- raw-length (+ 1 i))))))
		     ;; set state to state-pcdata for next section
		     (setf state state-pcdata) 
		     (return))
	      else
		   ;; push partial matches into data string
		   (dotimes (i raw-length)
		     (add-to-coll coll (elt raw-mode-delimiter i)))
		   (setf raw-length 0)
		   (add-to-coll coll ch)))
		     
	  ))
      
      
      ;; out of the loop. 
      ;; if we're in certain states then it means we should return a value
      ;;
      (case state
	(#.state-pcdata
	 ;; return the buffer as a string
	 (if* (zerop (collector-next coll))
	    then (values nil :eof)
	    else (values (prog1 
			     (if* (null ignore-strings)
				then (compute-coll-string coll))
			   (put-back-collector coll))
			 :pcdata)))
	
	(#.state-readtag
	 (when (null tag-to-return)
	       (error "unexpected end of input encountered"))
	 ;; we've read a tag with no attributes
	 (put-back-collector coll)
	 (values tag-to-return
		 (if* end-tag
		    then :end-tag
		    else (if* xml-bailout then :xml else :start-tag))
		 ))
	
	(#.state-findattribname
	 ;; returning a tag with possible attributes
	 (put-back-collector coll)
	 (if* end-tag
	    then ; ignore any attributes
		 (values tag-to-return :end-tag)
	  elseif attribs-to-return
	    then (values (cons tag-to-return 
			       (nreverse attribs-to-return))
			 :start-tag)
	    else (values tag-to-return :start-tag)))
	
	(#.state-readcomment-two
	 ;; returning a comment
	 (values (prog1 (if* (null ignore-strings)
			   then (compute-coll-string coll))
		   (put-back-collector coll))
		 :comment))
	
	(t 
	 (if* (null ch) then (error "unexpected end of input encountered")
	    else (error "internal error, can't be here in state ~d" state)))))))


(defvar *kwd-package* (find-package :keyword))

(defun compute-tag (coll)
  (declare (optimize (speed 3) (safety 1)))
  ;; compute the symbol named by what's in the collector
  (excl::intern* (collector-data coll) (collector-next coll) *kwd-package*))



(defun compute-coll-string (coll)
  (declare (optimize (speed 3) (safety 1)))
  ;; return the string that's in the collection
  (let ((str (make-string (collector-next coll)))
	(from (collector-data coll)))
    (dotimes (i (collector-next coll))
      (setf (schar str i) (schar from i)))
    
    str))

(defun coll-has-comment (coll)
  (declare (optimize (speed 3) (safety 1)))
  ;; true if the collector has exactly "!--" in it
  (and (eq 3 (collector-next coll))
       (let ((data (collector-data coll)))
	 (and (eq #\! (schar data 0))
	      (eq #\- (schar data 1))
	      (eq #\- (schar data 2))))))
		 

;;;;;;;;;;; quick and dirty parse

; the elements with no body and thus no end tag
(dolist (opt '(:area :base :basefont :bgsound :br :button :col 
	       ;;:colgroup - no, this is an element with contents
	       :embed :hr :img
	       :input :isindex :keygen :link :meta 
	       :plaintext :spacer :wbr))
  (setf (tag-no-end opt) t))

(defvar *in-line* '(:tt :i :b :big :small :em :strong :dfn :code :samp :kbd
		    :var :cite :abbr :acronym :a :img :object :br :script :map
		    :q :sub :sup :span :bdo :input :select :textarea :label :button :font))

(defvar *ch-format* '(:i :b :tt :big :small :strike :s :u
		      :em :strong :font))

; the elements whose start tag can end a previous tag

(setf (tag-auto-close :tr) '(:tr :td :th :colgroup))
(setf (tag-auto-close-stop :tr) '(:table))

(setf (tag-auto-close :td) '(:td :th))
(setf (tag-auto-close-stop :td) '(:table))

(setf (tag-auto-close :th) '(:td :th))
(setf (tag-auto-close-stop :td) '(:table))

(setf (tag-auto-close :dt) '(:dt :dd))
(setf (tag-auto-close-stop :dt) '(:dl))

(setf (tag-auto-close :li) '(:li))
(setf (tag-auto-close-stop :li) '(:ul :ol))

;; new stuff to close off tags with optional close tags
(setf (tag-auto-close :address) '(:head :p))
(setf (tag-auto-close :blockquote) '(:head :p))
(setf (tag-auto-close :body) '(:body :frameset :head))

(setf (tag-auto-close :dd) '(:dd :dt))
(setf (tag-auto-close-stop :dd) '(:dl))

(setf (tag-auto-close :dl) '(:head :p))
(setf (tag-auto-close :div) '(:head :p))
(setf (tag-auto-close :fieldset) '(:head :p))
(setf (tag-auto-close :form) '(:head :p))
(setf (tag-auto-close :frameset) '(:body :frameset :head))
(setf (tag-auto-close :hr) '(:head :p))
(setf (tag-auto-close :h1) '(:head :p))
(setf (tag-auto-close :h2) '(:head :p))
(setf (tag-auto-close :h3) '(:head :p))
(setf (tag-auto-close :h4) '(:head :p))
(setf (tag-auto-close :h5) '(:head :p))
(setf (tag-auto-close :h6) '(:head :p))
(setf (tag-auto-close :noscript) '(:head :p))
(setf (tag-auto-close :ol) '(:head :p))

(setf (tag-auto-close :option) '(:option))
(setf (tag-auto-close-stop :option) '(:select))

(setf (tag-auto-close :p) '(:head :p))

(setf (tag-auto-close :pre) '(:head :p))
(setf (tag-auto-close :table) '(:head :p))

(setf (tag-auto-close :tbody) '(:colgroup :tfoot :tbody :thead))
(setf (tag-auto-close-stop :tbody) '(:table))

(setf (tag-auto-close :tfoot) '(:colgroup :tfoot :tbody :thead))
(setf (tag-auto-close-stop :tfoot) '(:table))

(setf (tag-auto-close :thead) '(:colgroup :tfoot :tbody :thead))
(setf (tag-auto-close-stop :thead) '(:table))

(setf (tag-auto-close :ul) '(:head :p))

(setf (tag-no-pcdata :table) t)
(setf (tag-no-pcdata :tr) t)


(defmethod parse-html ((p stream) &key callback-only callbacks)
  (declare (optimize (speed 3) (safety 1)))
  (phtml-internal p nil callback-only callbacks))

(defmacro tag-callback (tag)
  `(rest (assoc ,tag callbacks)))

(defun phtml-internal (p read-sequence-func callback-only callbacks)
  (declare (optimize (speed 3) (safety 1)))
  (let ((first-pass nil)
	(raw-mode-delimiter nil)
	(pending nil)
	(current-tag :start-parse)
	(last-tag :start-parse)
	(current-callback-tags nil)
	(pending-ch-format nil)
	(closed-pending-ch-format nil)
	(new-opens nil)
	(tokenbuf (get-tokenbuf))
	(guts)
	)
    (labels ((close-off-tags (name stop-at)
	       ;; close off an open 'name' tag, but search no further
	       ;; than a 'stop-at' tag.
	       (if* (member (tag-name current-tag) name :test #'eq)
		  then ;; close current tag(s)
		       (loop
			 (close-current-tag)
			 (when (or (member (tag-name current-tag)
					   *ch-format*)
				(not (member 
				      (tag-name current-tag) name :test #'eq)))
			     (return)))
		elseif (member (tag-name current-tag) stop-at :test #'eq)
		  then nil
		  else ; search if there is a tag to close
		       (dolist (ent pending)
			 (if* (member (tag-name (car ent)) name :test #'eq)
			    then ; found one to close
				 (loop
				   (close-current-tag)
				   (if* (member (tag-name current-tag) name
						:test #'eq)
				      then (close-current-tag)
					   (return)))
				 (return)
			  elseif (member (tag-name (car ent)) stop-at
					 :test #'eq)
			    then (return) ;; do nothing
				 ))))
	   
	     (close-current-tag ()
	       ;; close off the current tag and open the pending tag
	       (when (member (tag-name current-tag) *ch-format* :test #'eq)
		 (push (tag-name current-tag) closed-pending-ch-format))
	       (let (element)
		 (if* (tag-no-pcdata (tag-name current-tag))
		    then (setq element `(,current-tag
					 ,@(strip-rev-pcdata guts)))
		    else (setq element `(,current-tag ,@(nreverse guts))))
		 (let ((callback (tag-callback (tag-name current-tag))))
		   (when callback
		     (setf current-callback-tags (rest current-callback-tags))
		     (funcall callback element)))
		 (let* ((prev (pop pending)))
		   (setq current-tag (car prev)
			 guts (cdr prev))
		   (push element guts))))
	     
	     (save-state ()
	       ;; push the current tag state since we're starting
	       ;; a new open tag
	       (push (cons current-tag guts) pending))
	     
	     
	     (strip-rev-pcdata (stuff)
	       ;; reverse the list stuff, omitting all the strings
	       (let (res)
		 (dolist (st stuff)
		   (if* (not (stringp st)) then (push st res)))
		 res))
	     (check-in-line (check-tag)
	       (setf new-opens nil)
	       (let (val kind (i 0)
		     (length (length first-pass)))
		 (loop
		   (if* (< i length) then
			   (setf val (nth i first-pass))
			   (setf kind (nth (+ i 1) first-pass))
			   (setf i (+ i 2))
			   (if* (= i length) then (setf first-pass (nreverse first-pass)))
		      else
			   (multiple-value-setq (val kind)
			     (get-next-token t))
			   (push val first-pass)
			   (push kind first-pass)
			   )
		   (when (eq kind :eof)
		     (if* (= i length) then (setf first-pass (nreverse first-pass)))
		     (return))
		   (when (and (eq val check-tag) (eq kind :end-tag))
		     (if* (= i length) then (setf first-pass (nreverse first-pass)))
		     (return))
		   (when (member val *ch-format* :test #'eq)
		     (if* (eq kind :start-tag) then (push val new-opens)
		      elseif (member val new-opens :test #'eq) then
			     (setf new-opens (remove val new-opens :count 1))
			else (close-off-tags (list val) nil)
			     )))))
		 
	     (get-next-token (force)
	       (if* (or force (null first-pass)) then
		       (multiple-value-bind (val kind)
			   (next-token p nil raw-mode-delimiter read-sequence-func
				       tokenbuf)
			(values val kind))
		  else
		       (let ((val (first first-pass))
			     (kind (second first-pass)))
			 (setf first-pass (rest (rest first-pass)))
			 (values val kind))))
	     )
      (loop
	(multiple-value-bind (val kind)
	    (get-next-token nil)
	  ;;(format t "val: ~s kind: ~s~%" val kind)
	  (case kind
	    (:pcdata
	     (when (or (and callback-only current-callback-tags)
		       (not callback-only))
	       (if* (member last-tag *in-line*)
		  then
		       (push val guts)
		  else
		       (when (dotimes (i (length val) nil)
			       (when (not (char-characteristic (elt val i) 
							       char-spacechar))
				 (return t)))
			 (push val guts))))
	     (when (and (= (length raw-mode-delimiter) 1) ;; xml tag...
			(or (and callback-only current-callback-tags)
			    (not callback-only)))
	       (close-off-tags (list last-tag) nil))
	     (setf raw-mode-delimiter nil)
	     )
	    
	    (:xml
	     (setf last-tag val)
	     (setf raw-mode-delimiter ">")
	     (let* ((name (tag-name val)))
	       (when (and callback-only (tag-callback name))
		 (push name current-callback-tags))
	       (save-state)
	       (setq current-tag val)
	       (setq guts nil)
	       ))
	    
	    (:start-tag
	     (setf last-tag val)
	     (if* (or (eq last-tag :style)
		      (and (listp last-tag) (eq (first last-tag) :style)))
		then
		     (setf raw-mode-delimiter
		       (if* (eq excl:*current-case-mode* :CASE-INSENSITIVE-UPPER)
			  then "</STYLE>"
			  else "</style>"))
	      elseif (or (eq last-tag :script)
		      (and (listp last-tag) (eq (first last-tag) :script)))
		then
		     (setf raw-mode-delimiter
		       (if* (eq excl:*current-case-mode* :CASE-INSENSITIVE-UPPER)
			  then "</SCRIPT>"
			  else "</script>")))
	     ; maybe this is an end tag too
	     (let* ((name (tag-name val))
		    (auto-close (tag-auto-close name))
		    (auto-close-stop nil)
		    (no-end (tag-no-end name)))
	       (when (and callback-only (tag-callback name))
		 (push name current-callback-tags))
	       (when (or (and callback-only current-callback-tags)
			 (not callback-only))
		 (if* auto-close
		    then (setq auto-close-stop (tag-auto-close-stop name))
			 (close-off-tags auto-close auto-close-stop))
		 (when (and pending-ch-format (not no-end))
		   (if* (member name *ch-format* :test #'eq) then nil
		    elseif (member name *in-line* :test #'eq) then
			   ;; close off only tags that are within *in-line* block
			   (check-in-line name)
		      else ;; close ALL pending char tags and then reopen 
			   (dolist (this-tag (reverse pending-ch-format))
			     (close-off-tags (list this-tag) nil))
			   ))
		 (if* no-end
		    then		; this is a singleton tag
			 (push (if* (atom val)
				  then val
				  else (list val))
			       guts)
		    else (save-state)
			 (setq current-tag val)
			 (setq guts nil))
		 (if* (member name *ch-format* :test #'eq)
		    then (push (if* (listp val) then (first val) else val)
			       pending-ch-format)
		    else (dolist (tmp (reverse closed-pending-ch-format))
			   (save-state)
			   (setf current-tag tmp)
			   (setf guts nil))
			 )
		 (setf closed-pending-ch-format nil)
		 )))
	  
	    (:end-tag
	     (setf raw-mode-delimiter nil)
	     (when (or (and callback-only current-callback-tags)
		       (not callback-only))
	       (close-off-tags (list val) nil)
	       (when (member val *ch-format* :test #'eq)
		 (setf pending-ch-format (remove val pending-ch-format :count 1))
		 (setf closed-pending-ch-format 
		   (remove val closed-pending-ch-format :count 1)))
	       (dolist (tmp (reverse closed-pending-ch-format))
		 (save-state)
		 (setf current-tag tmp)
		 (setf guts nil))
	       (setf closed-pending-ch-format nil)
	       ))

	    (:comment
	     (setf raw-mode-delimiter nil)
	     (when (or (and callback-only current-callback-tags)
		       (not callback-only))
	       (push `(:comment ,val) guts)))
	    
	    (:eof
	     (setf raw-mode-delimiter nil)
	     ;; close off all tags
	     (when (or (and callback-only current-callback-tags)
		       (not callback-only))
	       (close-off-tags '(:start-parse) nil))
	     (put-back-tokenbuf tokenbuf)
	     (return (cdar guts)))))))))

	      

(defmethod parse-html (file &key callback-only callbacks)
  (declare (optimize (speed 3) (safety 1)))
  (with-open-file (p file :direction :input)
    (parse-html p :callback-only callback-only :callbacks callbacks)))	     
	     

(defmethod parse-html ((str string) &key callback-only callbacks)
  (declare (optimize (speed 3) (safety 1)))
  (parse-html (make-string-input-stream str) 
	      :callback-only callback-only :callbacks callbacks))

		 
	      
  
  
	
		 
			 
		 
;;;;;;;;;;;; test

;;;(defun doit (ignore-data)
;;;  (with-open-file (p "readme.htm")
;;;    (loop
;;;      (multiple-value-bind (val kind) (next-token p ignore-data)
;;;	 ;(format t "~s -> ~s~%" kind val)
;;;      
;;;	(if* (eq kind :eof) then (return))))))
;;;
;;;(defun pdoit (&optional (file "testa.html"))
;;;  (with-open-file (p file)
;;;    (parse-html p)))
;;;
;;;
;;;;; requires http client module to work
;;;(defun getparse (host path)
;;;  (parse-html (httpr-body 
;;;	  (parse-response
;;;	   (simple-get host path)))))

(provide :phtml)
