(in-package :net.xml.parser)

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

(defstruct tokenbuf
  cur ;; next index to use to grab from tokenbuf
  max ;; index one beyond last character
  data ;; character array
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
	    buf
       else (make-tokenbuf
	     :cur 0
	     :max  0
	     :data (make-array 1024 :element-type 'character)))))

(defun compute-tag (coll &optional (package *keyword-package*))
  (declare (optimize (speed 3) (safety 1)))
  ;; compute the symbol named by what's in the collector
  ;; this will change when namespace stuff is determined...
  (excl::intern* (collector-data coll) (collector-next coll) package))

(defun compute-coll-string (coll)
  (declare (optimize (speed 3) (safety 1)))
  ;; return the string that's in the collection
  (let ((str (make-string (collector-next coll)))
	(from (collector-data coll)))
    (dotimes (i (collector-next coll))
      (setf (schar str i) (schar from i)))
    
    str))

(defstruct collector 
  next  ; next index to set
  max   ; 1+max index to set
  data  ; string vector
  )

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