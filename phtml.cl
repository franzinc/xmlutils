;; phtml.cl  - parse html

; to do
;  when the start tag is <!-- meaning comment we have to go into
;	a state where we scan up to the next -- and the ignore everything
;	until the next >
;  Likewise inside a <script> tag we have to turn off char entity checking
;
;
; do character entity stuff
;

(in-package :user)

(declaim (optimize (speed 3) (safety 1)))


(defparameter *entity-mapping*
    ;; hash table to map entity to number
    (let ((ht (make-hash-table :test #'equal)))
      
      (macrolet ((setem (list)
		   `(dolist (val ',list)
		      (setf (gethash (car val) ht) (cadr val)))))
	;; special.ent
	(setem 
	 (("quot" 34) ("amp" 38) ("lt" 60) ("gt" 62) ("OElig" 338)
		      ("oelig" 339) ("Scaron" 352) ("scaron" 353) 
		      ("Yuml" 376) ("circ" 710) ("tilde" 732)
		      ("ensp" 8194) ("emsp" 8195) ("thinsp" 8201) 
		      ("zwnj" 8204) ("zwj" 8205)
		      ("lrm" 8206) ("rlm" 8207) ("ndash" 8211) 
		      ("mdash" 8212) ("lsquo" 8216)
		      ("rsquo" 8217) ("sbquo" 8218) ("ldquo" 8220) 
		      ("rdquo" 8221) ("bdquo" 8222)
		      ("dagger" 8224) ("Dagger" 8225) ("permil" 8240) 
		      ("lsaquo" 8249) ("rsaquo" 8250) ("euro" 8364)))
      
	;; symbol.ent
	(setem (("fnof" 402) ("Alpha" 913) ("Beta" 914) ("Gamma" 915) 
			     ("Delta" 916) ("Epsilon" 917) ("Zeta" 918)
			     ("Eta" 919) ("Theta" 920) ("Iota" 921)
			     ("Kappa" 922) ("Lambda" 923) ("Mu" 924) 
			     ("Nu" 925) ("Xi" 926) ("Omicron" 927)
			     ("Pi" 928) ("Rho" 929) ("Sigma" 931) 
			     ("Tau" 932) ("Upsilon" 933) ("Phi" 934)
			     ("Chi" 935) ("Psi" 936) ("Omega" 937) 
			     ("alpha" 945) ("beta" 946)
			     ("gamma" 947) ("delta" 948) ("epsilon" 949) 
			     ("zeta" 950) ("eta" 951)
			     ("theta" 952) ("iota" 953) ("kappa" 954) 
			     ("lambda" 955) ("mu" 956) ("nu" 957)
			     ("xi" 958) ("omicron" 959) ("pi" 960) 
			     ("rho" 961) ("sigmaf" 962)
			     ("sigma" 963) ("tau" 964) ("upsilon" 965) 
			     ("phi" 966) ("chi" 967) ("psi" 968)
			     ("omega" 969) ("thetasym" 977) ("upsih" 978) 
			     ("piv" 982) ("bull" 8226)
			     ("hellip" 8230) ("prime" 8242) ("Prime" 8243) 
			     ("oline" 8254) ("frasl" 8260)
			     ("weierp" 8472) ("image" 8465) ("real" 8476) 
			     ("trade" 8482) ("alefsym" 8501)
			     ("larr" 8592) ("uarr" 8593) ("rarr" 8594) 
			     ("darr" 8595) ("harr" 8596)
			     ("crarr" 8629) ("lArr" 8656) ("uArr" 8657) 
			     ("rArr" 8658) ("dArr" 8659)
			     ("hArr" 8660) ("forall" 8704) ("part" 8706) 
			     ("exist" 8707) ("empty" 8709)
			     ("nabla" 8711) ("isin" 8712) ("notin" 8713) 
			     ("ni" 8715) ("prod" 8719)
			     ("sum" 8721) ("minus" 8722) ("lowast" 8727) 
			     ("radic" 8730) ("prop" 8733)
			     ("infin" 8734) ("ang" 8736) ("and" 8743) 
			     ("or" 8744) ("cap" 8745)
			     ("cup" 8746) ("int" 8747) ("sim" 8764) 
			     ("cong" 8773) ("asymp" 8776)
			     ("ne" 8800) ("equiv" 8801) ("le" 8804) 
			     ("ge" 8805) ("sub" 8834) ("sup" 8835)
			     ("nsub" 8836) ("sube" 8838) ("supe" 8839) 
			     ("oplus" 8853) ("otimes" 8855)
			     ("perp" 8869) ("sdot" 8901) ("lceil" 8968) 
			     ("rceil" 8969) ("lfloor" 8970)
			     ("rfloor" 8971) ("lang" 9001) ("rang" 9002) 
			     ("loz" 9674) ("spades" 9824)
			     ("clubs" 9827) ("hearts" 9829) ("diams" 9830)))
      
      
      
	;; latin1.ent
	(setem (("nbsp" 160) ("iexcl" 161) ("cent" 162) ("pound" 163) 
			     ("curren" 164) ("yen" 165) ("brvbar" 166) 
			     ("sect" 167) ("uml" 168) ("copy" 169) ("ordf" 170)
			     ("laquo" 171) ("not" 172) ("shy" 173) ("reg" 174)
			     ("macr" 175) ("deg" 176) ("plusmn" 177) 
			     ("acute" 180) ("micro" 181) ("para" 182) 
			     ("middot" 183) ("cedil" 184) ("ordm" 186) 
			     ("raquo" 187) ("iquest" 191) ("Agrave" 192)
			     ("Aacute" 193) ("Acirc" 194) ("Atilde" 195) 
			     ("Auml" 196) ("Aring" 197)
			     ("AElig" 198) ("Ccedil" 199) ("Egrave" 200)
			     ("Eacute" 201) ("Ecirc" 202)
			     ("Euml" 203) ("Igrave" 204) ("Iacute" 205) 
			     ("Icirc" 206) ("Iuml" 207)
			     ("ETH" 208) ("Ntilde" 209) ("Ograve" 210)
			     ("Oacute" 211) ("Ocirc" 212)
			     ("Otilde" 213) ("Ouml" 214) ("times" 215)
			     ("Oslash" 216) ("Ugrave" 217)
			     ("Uacute" 218) ("Ucirc" 219) ("Uuml" 220)
			     ("Yacute" 221) ("THORN" 222)
			     ("szlig" 223) ("agrave" 224) ("aacute" 225)
			     ("acirc" 226) ("atilde" 227)
			     ("auml" 228) ("aring" 229) ("aelig" 230)
			     ("ccedil" 231) ("egrave" 232)
			     ("eacute" 233) ("ecirc" 234) ("euml" 235)
			     ("igrave" 236) ("iacute" 237)
			     ("icirc" 238) ("iuml" 239) ("eth" 240)
			     ("ntilde" 241) ("ograve" 242)
			     ("oacute" 243) ("ocirc" 244) ("otilde" 245)
			     ("ouml" 246) ("divide" 247)
			     ("oslash" 248) ("ugrave" 249) ("uacute" 250)
			     ("ucirc" 251) ("uuml" 252)
			     ("yacute" 253) ("thorn" 254) ("yuml" 255)))
      
	ht)))


      






;; tag info on plist of kwd symbol


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
)


(defstruct collector 
  next  ; next index to set
  max   ; 1+max index to set
  data  ; string vector
  )

;; keep a cache of collectors on this list

(defparameter *collectors* (list nil nil nil nil))

(defun get-collector ()
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
  (mp::without-scheduling 
    (do ((cols *collectors* (cdr cols)))
	((null cols)
	 ; toss it away
	 nil)
      (if* (null (car cols))
	 then (setf (car cols) col)
	      (return)))))
	 


(defun grow-and-add (coll ch)
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
	
	; now the unusual cases
	(addit (char-code #\-) char-attribundelimattribvalue)
	(addit (char-code #\.) char-attribundelimattribvalue)
	
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
  ;; return true if the given char has the given bit set in 
  ;; the characteristic array
  (let ((code (char-code char)))
    (if* (<= 0 code 127)
       then ; in range
	    (not (zerop (logand (svref *characteristics* code) bit))))))


	
      



(defparameter *tokenbuf* (make-array 1024 :element-type 'character))
(defparameter *max-tokenbuf* 0) ; index one beyond last character
(defparameter *cur-tokenbuf* 0) ; next index to use to grab from tokenbuf
(defun reset-tokenbuf ()
  (setf *max-tokenbuf* 0
	*cur-tokenbuf* 0))


    
    
(defun next-token (stream ignore-strings)
  ;; return two values: 
  ;;    the next token from the stream.
  ;; 	the kind of token (:pcdata, :start-tag, :end-tag, :eof)
  ;;
  (macrolet ((next-char (stream)
	       `(let ((cur *cur-tokenbuf*)
		      (tb *tokenbuf*))
		  (if* (>= cur *max-tokenbuf*)
		     then ; fill buffer
			  (if* (zerop (setq *max-tokenbuf* 
					(read-sequence tb stream)))
			     then (setq cur nil) ; eof
			     else (setq cur 0)))
		  (if* cur
		     then (prog1 (schar tb cur)
			    (setq *cur-tokenbuf* (1+ cur))))))
			  
	     
	     (un-next-char (stream ch)
	       `(decf *cur-tokenbuf*))
	     
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
	       `(char-downcase ,ch))
	       
	     )
    
    (let ((state state-pcdata)
	  (coll  (get-collector))
	  (ch)

	  (value-delim)
	  
	  (tag-to-return)
	  (attribs-to-return)
	  
	  (end-tag)
	  
	  (attrib-name)
	  (attrib-value)
	  
	  (name-length 0) ; count only when it could be a comment
	  
	  )
    
      (loop
      
	(setq ch (next-char stream))
      
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
		      then (setq name-length 0))
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
		   (if* (eq ch #\>)
		      then (return) ; we're done
		      else (clear-coll coll)
			   (if* (eq tag-to-return :!--)
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
	 ;; we've read a tag with no attributes
	 (put-back-collector coll)
	 (values tag-to-return
		 (if* end-tag
		    then :end-tag
		    else :start-tag)))
	
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
	
	(t (error "internal error, can't be here in state ~d" state))))))


(defvar *kwd-package* (find-package :keyword))

(defun compute-tag (coll)
  ;; compute the symbol named by what's in the collector
  (excl::intern* (collector-data coll) (collector-next coll) *kwd-package*))



(defun compute-coll-string (coll)
  ;; return the string that's in the collection
  (let ((str (make-string (collector-next coll)))
	(from (collector-data coll)))
    (dotimes (i (collector-next coll))
      (setf (schar str i) (schar from i)))
    
    str))

(defun coll-has-comment (coll)
  ;; true if the collector has exactly "!--" in it
  (and (eq 3 (collector-next coll))
       (let ((data (collector-data coll)))
	 (and (eq #\! (schar data 0))
	      (eq #\- (schar data 1))
	      (eq #\- (schar data 2))))))
		 



	      


		 


;;;;;;;;;;; quick and dirty parse

; the elements with no body and thus no end tag
(dolist (opt '(:area :base :basefont :bgsound :br :button :col 
	       :colgroup :embed :hr :img
	       :input :isindex :keygen :link :meta 
	       :object 
	       :p  ;; legally isn't but this makes thing work better
	       :plaintext :spacer :wbr))
  (setf (tag-no-end opt) t))

; the elements whose start tag can end a previous tag

(setf (tag-auto-close :tr) '(:tr :td th))
(setf (tag-auto-close-stop :tr) '(:table))

(setf (tag-auto-close :td) '(:td :th))
(setf (tag-auto-close-stop :td) '(:table))

(setf (tag-auto-close :th) '(:td :th))
(setf (tag-auto-close-stop :td) '(:table))

(setf (tag-auto-close :dt) '(:dt dd))
(setf (tag-auto-close-stop :dt) '(:dl))

(setf (tag-auto-close :li) '(:li))
(setf (tag-auto-close-stop :li) '(:ul :ol))


(setf (tag-no-pcdata :table) t)
(setf (tag-no-pcdata :tr) t)





(defmethod phtml ((p stream))
  
  (let ((pending nil)
	(current-tag :start-parse)
	(guts))

    (labels ((close-off-tags (name stop-at)
	       ;; close off an open 'name' tag, but search no further
	       ;; than a 'stop-at' tag.
	       (if* (member (tag-name current-tag) name :test #'eq)
		  then ; close current tag
		       (close-current-tag)
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
			    then (return) ; do nothign
				 ))))
	   
	     (close-current-tag ()
	       ; close off the current tag and open the pending tag
	       (let (element)
		 (if* (tag-no-pcdata (tag-name current-tag))
		    then (setq element `(,current-tag
					 ,@(strip-rev-pcdata guts)))
		    else (setq element `(,current-tag ,@(nreverse guts))))
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
	     )
      
    
      (loop 
	(multiple-value-bind (val kind) (next-token p nil)
	  (case kind
	    (:pcdata
	     (push val guts))
	  
	    (:start-tag
	     ; maybe this is an end tag too
	     (let* ((name (tag-name val))
		    (auto-close (tag-auto-close name))
		    (auto-close-stop nil)
		    (no-end (tag-no-end name)))
	       (if* auto-close
		  then (setq auto-close-stop (tag-auto-close-stop name))
		       (close-off-tags auto-close auto-close-stop))
	     
	     
	       (if* no-end
		  then ; this is a singleton tag
		       (push (if* (atom val)
				then val
				else (list val))
			     guts)
		  else (save-state)
		       (setq current-tag val)
		       (setq guts nil))))
	  
	    (:end-tag
	     (close-off-tags (list val) nil))

	    (:comment
	     (push `(:comment ,val) guts))
	    
	    (:eof
	     ; close off all tags
	     (close-off-tags '(:start-parse) nil)
	     (return (cdar guts)))))))))

	  
	     
(defmethod phtml (file)
  (with-open-file (p file :direction :input)
    (phtml p)))	     
	     

(defmethod phtml ((str string))
  (phtml (make-string-input-stream str)))

		 
	      
  
  
	
		 
			 
		 
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
;;;    (phtml p)))
;;;
;;;
;;;;; requires http client module to work
;;;(defun getparse (host path)
;;;  (phtml (httpr-body 
;;;	  (parse-response
;;;	   (simple-get host path)))))

