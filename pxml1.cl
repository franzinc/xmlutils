;;
;; copyright (c) 1986-2000 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2000-2004 Franz Inc, Oakland, CA - All rights reserved.
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
;; Change Log
;;
;; 10/14/00 add namespace support; xml-error fix

(in-package :net.xml.parser)

(pxml-dribble-bug-hook "$Id: pxml1.cl,v 1.2.2.3.22.5 2004/03/03 16:06:48 layer Exp $")

(defparameter *collectors* (list nil nil nil nil nil nil nil nil))

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

(defun pub-id-char-p (char)
  (declare (optimize (speed 3) (safety 1)))
  (let ((code (char-code char)))
    (or (= #x20 code) (= #xD code) (= #xA code)
	(<= (char-code #\a) code (char-code #\z))
	(<= (char-code #\A) code (char-code #\Z))
	(<= (char-code #\0) code (char-code #\9))
	(member char '( #\- #\' #\( #\) #\+ #\, #\. #\/ #\: #\= #\?
		       #\; #\! #\* #\# #\@ #\$ #\_ #\%)))))

(defparameter *keyword-package* (find-package :keyword))

;; cache of tokenbuf structs
(defparameter *tokenbufs* (list nil nil nil nil))

(defstruct iostruct
  unget-char ;; character pushed back
  tokenbuf ;; main input tokenbuf
  read-sequence-func ;; optional alternative to read-sequence
  entity-bufs ;; active entity tokenbufs
  entity-names ;; active entity names
  parameter-entities
  general-entities
  do-entity  ;; still substituting entity text
  seen-any-dtd
  seen-external-dtd
  seen-parameter-reference
  standalonep
  uri-to-package
  ns-to-package
  ns-scope
  )

(defstruct tokenbuf
  cur ;; next index to use to grab from tokenbuf
  max ;; index one beyond last character
  data ;; character array
  stream ;; for external sources
  )

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
	    (setf (tokenbuf-stream buf) nil)
	    buf
       else (make-tokenbuf
	     :cur 0
	     :max  0
	     :data (make-array 1024 :element-type 'character)))))

(defstruct collector
  next  ; next index to set
  max   ; 1+max index to set
  data  ; string vector
  )

(defun compute-tag (coll &optional (package *keyword-package*) ns-tokenbuf
			 &aux (ns-to-package 
			       (when ns-tokenbuf (iostruct-ns-to-package ns-tokenbuf))))
  (declare (optimize (speed 3) (safety 1)))
  ;; compute the symbol named by what's in the collector
  (if* (not ns-to-package)
       then (excl::intern* (collector-data coll) (collector-next coll) package)
       else
       (let (new-package (data (collector-data coll)))
	 (if* (and (eq (schar data 0) #\x)
		   (eq (schar data 1) #\m)
		   (eq (schar data 2) #\l)
		   (eq (schar data 3) #\n)
		   (eq (schar data 4) #\s)
		   (or (eq (schar data 5) #\:)
		       (= (collector-next coll) 5)))
	      then
	      ;; putting xmlns: in :none namespace
	      (and (setf new-package (assoc :none ns-to-package))
		   ;; bug13668 - look for xmlns="" in this scope
		   (rest new-package)
		   (setf package (rest new-package)))
	      (excl::intern* (collector-data coll) (collector-next coll) package)
	      else
	      (let ((colon-index -1)
		    (data (collector-data coll)))
		(dotimes (i (collector-next coll))
		  (when (eq (schar data i) #\:)
		    (setf colon-index i)
		    (return)))
		(if* (> colon-index -1)
		     then
		     (let ((string1 (make-string colon-index))
			   new-package string2)
		       (dotimes (i colon-index)
			 (setf (schar string1 i) (schar data i)))
		       (setf new-package (assoc string1 ns-to-package :test 'string=))
		       (if* new-package
			    then
			    (setf string2 (make-string (- (collector-next coll)
							  (+ 1 colon-index))))
			    (dotimes (i (- (collector-next coll)
					   (+ 1 colon-index)))
			      (setf (schar string2 i)
				    (schar data (+ colon-index 1 i))))
			    (excl::intern string2 (rest new-package))
			    else
			    (excl::intern* (collector-data coll)
					   (collector-next coll) package)))
		     else
		     (let ((new-package (assoc :none ns-to-package)))
		       (and new-package
			    ;; bug13668 - look for xmlns="" in this scope
			    (rest new-package)
			    (setf package (rest new-package))))
		     (excl::intern* (collector-data coll)
				    (collector-next coll) package)))
	      ))
       ))

(defun tag-from-string (name ns-to-p)
  (let* ((len (length name))
	 (pos (position #\: name))
	 prefix suffix entry)
    (if* pos
	 then
	 ;; this may be a qualified name
	 (if* (eql pos (1- len))
	      then
	      ;; foo: is not a well-formed qualified name
	      (setf suffix name)
	      else
	      ;; we have foo:bar
	      (setf prefix (subseq name 0 pos))
	      (if (setf entry (assoc prefix ns-to-p :test #'equal))
		  ;; and we know about namespace foo
		  (setf suffix (subseq name (1+ pos)))
		;; but we do not know about namespace foo
		;;  so treat the name as unqualified
		(setf suffix name))
	      )
	 elseif (setf entry (assoc :none ns-to-p :test #'equal))
	 then
	 ;; bug13668 - look for xmlns="" in this scope
	 (when (null (cdr entry)) (setf entry nil))
	 ;; this is an unqualified name and there is a default namespace
	 (setf suffix name)
	 else
	 (setf suffix name))
    (if* entry
	 then
	 (intern suffix (cdr entry))
	 else
	 (intern name))))



(defun compute-coll-string (coll)
  (declare (optimize (speed 3) (safety 1)))
  ;; return the string that's in the collection
  (let ((str (make-string (collector-next coll)))
	(from (collector-data coll)))
    (dotimes (i (collector-next coll))
      (setf (schar str i) (schar from i)))

    str))

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

(defmacro next-char (tokenbuf read-sequence-func)
  `(let ((cur (tokenbuf-cur ,tokenbuf))
	 (tb (tokenbuf-data ,tokenbuf)))
     (if* (>= cur (tokenbuf-max ,tokenbuf))
	then				;; fill buffer
	     (if* (or (not (tokenbuf-stream ,tokenbuf))
		      (zerop (setf (tokenbuf-max ,tokenbuf)
			       (if* ,read-sequence-func
				  then (funcall ,read-sequence-func tb
						(tokenbuf-stream ,tokenbuf))
				  else (read-sequence tb (tokenbuf-stream ,tokenbuf))))))
		then (setq cur nil)	;; eof
		else (setq cur 0)))
     (if* cur
	then (prog1
		 (let ((cc (schar tb cur)))
		   (if (and (tokenbuf-stream ,tokenbuf) (eq #\return cc)) #\newline cc))
	       (setf (tokenbuf-cur ,tokenbuf) (1+ cur))))))

(defun get-next-char (iostruct)
  (declare (optimize (speed 3) (safety 1)))
  (let* (from-stream (tmp-char
	 (let (char)
	   (if* (iostruct-unget-char iostruct) then
		   ;; from-stream is used to do input CR/LF normalization
		   (setf from-stream t)
		   (setf char (first (iostruct-unget-char iostruct)))
		   (setf (iostruct-unget-char iostruct) (rest (iostruct-unget-char iostruct)))
		   char
	    elseif (iostruct-entity-bufs iostruct) then
		   (let (entity-buf)
		     (loop
		       (setf entity-buf (first (iostruct-entity-bufs iostruct)))
		       (if* (streamp (tokenbuf-stream entity-buf))
			  then (setf from-stream t)
			  else (setf from-stream nil))
		       (setf char (next-char entity-buf (iostruct-read-sequence-func iostruct)))
		       (when char (return))
		       (when (streamp (tokenbuf-stream entity-buf))
			 (close (tokenbuf-stream entity-buf))
			 (put-back-tokenbuf entity-buf))
		       (setf (iostruct-entity-bufs iostruct) (rest (iostruct-entity-bufs iostruct)))
		       (setf (iostruct-entity-names iostruct) (rest (iostruct-entity-names iostruct)))
		       (when (not (iostruct-entity-bufs iostruct)) (return))))
		   (if* char then char
		      else (next-char (iostruct-tokenbuf iostruct)
				      (iostruct-read-sequence-func iostruct)))
	      else (setf from-stream t)
		   (next-char (iostruct-tokenbuf iostruct)
			      (iostruct-read-sequence-func iostruct))))))
    (if* (and from-stream (eq tmp-char #\return)) then #\newline else tmp-char)))

;; Force unicode external-format to be loaded at load time so that
;; it will be available at runtime without needing to autoload.
(find-external-format :unicode)

(defun unicode-check (p tokenbuf)
  (declare (ignorable tokenbuf) (optimize (speed 3) (safety 1)))
  ;; need no-OO check because external format support isn't completely done yet
  (when (not (typep p 'string-input-simple-stream))
    #+(version>= 6 0 pre-final 1)
    (let ((format (ignore-errors (excl:sniff-for-unicode p))))
      (if* (eq format (find-external-format :unicode))
	 then
	      (setf (stream-external-format p) format)
	 else 
	      (setf (stream-external-format p) (find-external-format :utf8))
	      (encoding-to-external-format p tokenbuf)))
    #-(version>= 6 0 pre-final 1)
    (let* ((c (read-char p nil)) c2
	   (c-code (if c (char-code c) nil)))
      (if* (eq #xFF c-code) then
	      (setf c2 (read-char p nil))
	      (setf c-code (if c (char-code c2) nil))
	      (if* (eq #xFE c-code) then
		      (format t "set unicode~%")
		      (setf (stream-external-format p)
			(find-external-format #+(version>= 6 0 pre-final 1) :unicode
					      #-(version>= 6 0 pre-final 1) :fat-little))
		 else
		      (xml-error "stream has incomplete Unicode marker"))
	 else (setf (stream-external-format p)
		(find-external-format :utf8))
	      (when c
		(push c (iostruct-unget-char tokenbuf))
		#+ignore (unread-char c p)  ;; bug when there is single ^M in file
		)))))

;; spr27942.  Try to set stream-external-format to encoding before we
;; fill parser buffers.
(defun encoding-to-external-format (p tokenbuf)
  (let ((chars nil)
	(saw-> nil)
	(saw-< nil)
	(saw-<? nil))
    (dotimes (i 100)
      (let ((ch (read-char p nil)))
	(if* ch
	   then (push ch chars)
		(unless (excl::whitespace-char-p ch)
		  (if* saw-<
		     then (if* saw-<?
			     then (when (eq ch #\>)
				    (setq saw-> t)
				    (return))
			   elseif (eq ch #\?)
			     then (setq saw-<? t)
			     else (return))
		   elseif (eq ch #\<)
		     then (setq saw-< t)
		     else (return)))
	   else (return))))
    (setq chars (nreverse chars))
    (when saw->
      (let* ((st (coerce chars 'string))
	     (enc-start (search "encoding" st)))
	(when enc-start
	  (let* ((enc-string (subseq st enc-start))
		 (=-start (position #\= enc-string)))
	    (when =-start
	      (let ((encoding
		     (do* ((pos (1+ =-start) (1+ pos))
			   (lg (length enc-string))
			   (ch #1=(if* (< pos lg)
				     then (schar enc-string pos)
				     else ':eof)
			       #1#)
			   (result nil)
			   (start-quote nil))
			 ((eq start-quote ch)
			  (coerce (nreverse result) 'string))
		       (when (eq ':eof ch)
			 (return nil))
		       (if* start-quote
			  then (push ch result)
			  else (unless (excl::whitespace-char-p ch)
				 (setq start-quote ch))))))
		(when encoding
		  (setf (stream-external-format p)
		    (find-external-format encoding)))))))))
    (dolist (ch (nreverse chars))
      (push ch (iostruct-unget-char tokenbuf)))))
	  

(defun add-default-values (val attlist-data)
  (declare (ignorable old-coll) (optimize (speed 3) (safety 1)))
  (if* (symbolp val)
     then
	  (let* ((tag-defaults (assoc val attlist-data)) defaults)
	    (dolist (def (rest tag-defaults))
	      (if* (stringp (third def)) then
		      (push (first def) defaults)
		      (push (if (eq (second def) :CDATA) (third def)
			      (normalize-attrib-value (third def))) defaults)
	       elseif (and (eq (third def) :FIXED) (stringp (fourth def))) then
		      (push (first def) defaults)
		      (push (if (eq (second def) :CDATA) (fourth def)
			      (normalize-attrib-value (fourth def))) defaults)
		      ))
	    (if* defaults then
		    (setf val (append (list val) (nreverse defaults)))
	       else val)
	    )
     else
	  ;; first make sure there are no errors in given list
	  (let ((pairs (rest val)))
	    (loop
	      (when (null pairs) (return))
	      (let ((this-one (first pairs)))
		(setf pairs (rest (rest pairs)))
		(when (member this-one pairs)
		  (xml-error "Entity: " (first val) " has multiple "
			     this-one " attribute values")))))
	  (let ((tag-defaults (assoc (first val) attlist-data)) defaults)
	    (dolist (def (rest tag-defaults))
	      (let ((old (member (first def) (rest val))))
		(if* (not old) then
			(if* (stringp (third def)) then
				(push (first def) defaults)
				(push (third def) defaults)
			 elseif (and (eq (third def) :FIXED) (stringp (fourth def))) then
				(push (first def) defaults)
				(push (fourth def) defaults))
		   else
			(push (first old) defaults)
			(push (second old) defaults))))
	    (if* defaults then
		    ;; now look for attributes in original list that weren't in dtd
		    (let ((tmp-val (rest val)) att att-val)
		      (loop
			(when (null tmp-val) (return))
			(setf att (first tmp-val))
			(setf att-val (second tmp-val))
			(setf tmp-val (rest (rest tmp-val)))
			(when (not (member att defaults))
			  (push att defaults)
			  (push att-val defaults))))
		    (setf val (append (list (first val)) (nreverse defaults)))
	       else val))
	  ))

(defun normalize-public-value (public-value)
  (setf public-value (string-trim '(#\space) public-value))
  (let ((count 0) (stop (length public-value)) (last-ch nil) cch)
    (loop
      (when (= count stop) (return public-value))
      (setf cch (schar public-value count))
      (if* (and (eq cch #\space) (eq last-ch #\space)) then
	      (setf public-value
		(remove #\space public-value :start count :count 1))
	      (decf stop)
	 else (incf count)
	      (setf last-ch cch)))))


(defun normalize-attrib-value (attrib-value &optional first-pass)
  (declare (optimize (speed 3) (safety 1)))
  (when first-pass
    (let ((count 0) (stop (length attrib-value)) (last-ch nil) cch)
      (loop
	(when (= count stop) (return))
	(setf cch (schar attrib-value count))
	(if* (or (eq cch #\return) (eq cch #\tab)) then (setf (schar attrib-value count) #\space)
	 elseif (and (eq cch #\newline) (not (eq last-ch #\return))) then
		(setf (schar attrib-value count) #\space)
	 elseif (and (eq cch #\newline) (eq last-ch #\return)) then
		(setf attrib-value
		  (remove #\space attrib-value :start count :count 1))
		(decf stop))
	(incf count)
	(setf last-ch cch))))
  (setf attrib-value (string-trim '(#\space) attrib-value))
  (let ((count 0) (stop (length attrib-value)) (last-ch nil) cch)
    (loop
      (when (= count stop) (return attrib-value))
      (setf cch (schar attrib-value count))
      (if* (and (eq cch #\space) (eq last-ch #\space)) then
	      (setf attrib-value
		(remove #\space attrib-value :start count :count 1))
	      (decf stop)
	 else (incf count)
	      (setf last-ch cch)))))

(defun check-xmldecl (val tokenbuf &aux ver sa enc (tail (cdr val)))
  (declare (ignorable old-coll) (optimize (speed 3) (safety 1)))
  (if (and (symbolp (first tail)) (string= "version" (symbol-name (first tail))))
      (setf ver (second tail) tail (cddr tail))
    (xml-error "XML declaration tag does not include 'version' attribute"))
  (when (and (symbolp (first tail)) (string= "encoding" (symbol-name (first tail))))
    (setf enc (second tail))
    (setf tail (cddr tail)))
  (when (and (symbolp (first tail)) (string= "standalone" (symbol-name (first tail))))
    (setf sa (second tail))
    (setf tail (cddr tail)))
  (when tail
    (xml-error 
     (format nil "XMLDecl contains unexpected or out-of-order components: '~{~A ~}'."
	     tail)))
  (when sa
    (if* (equal sa "yes")
	 then
	 (setf (iostruct-standalonep tokenbuf) t)
	 elseif (not (equal sa "no"))
	 then
	 (xml-error 
	  "XMLDecl does not include correct 'standalone' attribute value")))
  (dotimes (i (length ver))
    (let ((c (schar ver i)))
      (when (and (not (alpha-char-p c))
		 (not (digit-char-p c))
		 (not (member c '(#\. #\_ #\- #\:)))
		 )
	(xml-error
	 "XMLDecl does not include correct 'version' attribute value"))))
  (when enc
    (dotimes (i (length enc))
      (let ((c (schar enc i)))
	(when (and (not (alpha-char-p c))
		   (if* (> i 0) then
			(and (not (digit-char-p c))
			     (not (member c '(#\. #\_ #\-))))
			else t))
	  (xml-error
	   "XMLDecl does not include correct 'encoding' attribute value"))))
    ;; jkf 3/26/02 
    ;; if we have a stream we're reading from set its external-format
    ;; to the encoding
    ;; note - tokenbuf is really an iostruct, not a tokenbuf
    (if* (tokenbuf-stream (iostruct-tokenbuf tokenbuf))
	 then (setf (stream-external-format 
		     (tokenbuf-stream (iostruct-tokenbuf tokenbuf)))
		    (find-external-format enc)))
			 
    
    ))

(defun xml-error (&rest parts &aux text)
  (if (and (stringp (first parts)) (null (cdr parts)))
      (setf text (first parts))
    (setf text (format nil "~{~A~}" parts)))
  (error "XML not well-formed - ~A" text))


(defmacro with-coll-macros (&rest body)
  `(macrolet ((add-to-entity-buf
	       (entity-symbol p-value)
	       `(progn
		  (push (make-tokenbuf :cur 0 :max (length ,p-value) :data ,p-value)
			(iostruct-entity-bufs tokenbuf))))
	      (clear-coll (coll) `(setf (collector-next ,coll) 0))
	      (un-next-char (ch) `(push ,ch (iostruct-unget-char tokenbuf)))
	      (add-to-coll
	       (coll ch)
	       `(let ((.next. (collector-next ,coll)))
		  (if* (>= .next. (collector-max ,coll))
		       then (grow-and-add ,coll ,ch)
		       else (setf (schar (collector-data ,coll) .next.)
				  ,ch)
		       (setf (collector-next ,coll) (1+ .next.)))))

	      (token-error2  (&rest errargs)
			    `(token-error-fn t   ch coll tokenbuf ,@errargs))
	      (token-error0 (&rest errargs)
			    `(token-error-fn nil ch coll tokenbuf ,@errargs))
	      (token-error1
	       (&rest errargs)
	       `(token-error-fn nil ch coll tokenbuf ,@errargs :coll-string "'"))
	      (token-error3
	       (&rest errargs)
	       `(token-error-fn t   ch coll tokenbuf ,@errargs :coll-string "'"))

	      )
     ,@body))

(defun token-error-fn (clear ch coll tokenbuf &rest errargs)
  (with-coll-macros
   (when clear (clear-coll coll))
   (dotimes (i 15)
     (add-to-coll coll ch)
     (setq ch (get-next-char tokenbuf))
     (if* (null ch)
	  then (return)))
   (apply 'xml-error  
	  (mapcar #'(lambda (e)
		      (case e
			(:coll-string (compute-coll-string coll))
			(otherwise e)))
		  errargs))))

