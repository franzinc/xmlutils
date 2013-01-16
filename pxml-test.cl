;;
;; copyright (c) 1986-2003 Franz Inc, Berkeley, CA  - All rights reserved.
;; copyright (c) 2003-2013 Franz Inc, Oakland, CA - All rights reserved.
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
;; 10/14/00 add namespace example; xml-error related change

(eval-when (compile load eval)
  (require :tester))

(defpackage :user (:use :net.uri :net.xml.parser))  ;; assumes pxml.cl loaded
(in-package :user)

;; these functions are used in the OASIS xmltest subdirectories
;; see pxml.txt for more information

(defun file-callback (filename token &optional public)
  (declare (ignorable token public))
  ;;(format t "filename: ~s token: ~s public: ~s~%" filename token public)
  (ignore-errors (open (uri-path filename))))

(defun test-one-file (int external-callback)
  (let ((filename (concatenate 'string (format nil "~3,'0d" int) ".xml")))
    (equalp (with-open-file (p filename) 
	      (parse-xml p :external-callback external-callback
			 :content-only t))
	    (with-open-file (p (concatenate 'string "out/" filename))
	      (parse-xml p)))))

(defun test-some-files (max &key skip-list external-callback)
  (dotimes (i max)
    (if* (member (+ 1 i) skip-list) then
	    (format t "i: ~s skipping...~%" (+ 1 i))
       else
	    (format t "i: ~s equalp: ~s~%" (+ 1 i) (test-one-file (+ 1 i) external-callback)))))

;; have to be in valid/sa directory when this is run
(defun test-sa-files ()
  (test-some-files 119 :external-callback 'file-callback :skip-list (list 52 64 89)))

;; have to be in valid/ext-sa directory when this is run
(defun test-ext-sa-files ()
  (test-some-files 14 :external-callback 'file-callback ))

;; have to be in valid/not-sa directory when this is run
(defun test-not-sa-files ()
  (test-some-files 31 :external-callback 'file-callback ))

(defun test-one-bad-file (filename external-callback)
  (ignore-errors
   (with-open-file (p filename) 
     (parse-xml p :external-callback external-callback
		:content-only t))))

(defun test-some-bad-files (max external-callback)
  (dotimes (i max)
    (let* ((index (+ 1 i))
	   (filename (concatenate 'string (format nil "~3,'0d" index) ".xml")))
      (multiple-value-bind (val error)
	  (test-one-bad-file filename external-callback) 
	(format t "i: ~s error: ~s~%"
		index (if error
			  (simple-condition-format-arguments error) val))))))

;; have to be in not-wf/sa directory when this is run
(defun test-not-wf-sa-files ()
  (test-some-bad-files 186 'file-callback))

;; have to be in not-wf/ext-sa directory when this is run
(defun test-not-wf-ext-sa-files ()
  (test-some-bad-files 3 'file-callback))

;; have to be in not-wf/not-sa directory when this is run
(defun test-not-wf-not-sa-files ()
  (test-some-bad-files 8 'file-callback))

;; the next stuff is used in the .txt file for documentation

(defvar *xml-example-external-url*
    "<!ENTITY ext1 'this is some external entity %param1;'>")

(defun example-callback (var-name token &optional public)
  (declare (ignorable token public))
  (setf var-name (uri-path var-name))
  (if* (equal var-name "null") then nil
     else
	  (let ((string (eval (intern var-name (find-package :user)))))
	    (make-string-input-stream string))))

(defvar *xml-example-string*
    "<?xml version='1.0' encoding='utf-8'?>
<!-- the following XML input is well-formed but its validity has not been checked ... -->
<?piexample this is an example processing instruction tag ?>
<!DOCTYPE example SYSTEM '*xml-example-external-url*' [
   <!ELEMENT item1 (item2* | (item3+ , item4))>
   <!ELEMENT item2 ANY>
   <!ELEMENT item3 (#PCDATA)>
   <!ELEMENT item4 (#PCDATA)>
   <!ATTLIST item1
        att1 CDATA #FIXED 'att1-default'
        att2 ID #REQUIRED
        att3 ( one | two | three ) 'one'
        att4 NOTATION ( four | five ) 'four' >
   <!ENTITY % param1 'text'>
   <!ENTITY nentity SYSTEM 'null' NDATA somedata>
   <!NOTATION notation SYSTEM 'notation-processor'>
]>
<item1 att2='1'><item3>&ext1;</item3></item1>")

(defvar *xml-example-string2*)
(defvar *xml-example-string3*)

;; bug fix testing
(setf *xml-example-string2*
    "<!DOCTYPE example [
<!ELEMENT item1 (item2* | (item3+ , item4))>
]>
<item1/>")

(setf *xml-example-string3*
    "<!DOCTYPE example [
<!ELEMENT item1 (item2* | (item3+ , item4*))>
]>
<item1/>")

(defvar *xml-example-string4*)

(setf *xml-example-string4*
  "<bibliography
      xmlns:bib='http://www.bibliography.org/XML/bib.ns'
      xmlns='urn:com:books-r-us'>
    <bib:book owner='Smith'>
       <bib:title>A Tale of Two Cities</bib:title>
       <bib:bibliography
         xmlns:bib='http://www.franz.com/XML/bib.ns'
         xmlns='urn:com:books-r-us'>
        <bib:library branch='Main'>UK Library</bib:library>
        <bib:date calendar='Julian'>1999</bib:date>
        </bib:bibliography>
       <bib:date calendar='Julian'>1999</bib:date>
       </bib:book>
     </bibliography>")


;;; MORE TESTS
;;; 
;;; Top level function is test-all
;;; Var *test-root* points to OASIS test suite root

;; This encoding (external-format) is mentioned in the IBM tests.
#+old (or (find-external-format :us-ascii :errorp nil)
	  (def-external-format :latin1 :nicknames '(:us-ascii)))
(pushnew :us-ascii (ef-nicknames (find-external-format :latin1)))
(pushnew :euc-jp (ef-nicknames (find-external-format :euc)))
(pushnew :shift_jis (ef-nicknames (find-external-format :shiftjis)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;  Testing
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *test-xml-file* "")
(defvar *parse-xml* 'default-parse-xml)

(defun test-one-pattern (&key root out prefix modifier index suffix pattern)
  (ignore-errors
    (let* ((name (apply 'concatenate 'string
			(mapcar
			 #'(lambda (k)
			     (case k
			       (:p (or prefix ""))
			       (:m (or modifier ""))
			       (:s (or suffix ""))
			       ((0 1) (format nil "~A" index))
			       (2 (format nil "~2,'0D" index))
			       (3 (format nil "~3,'0D" index))
			       (4 (format nil "~4,'0D" index))))
			 pattern)))
	   (path (make-pathname :type "xml"
				:name name
				:defaults root))
	   (parse1
	    (when (probe-file path)
	      (with-open-file (s path) (funcall *parse-xml* s))))
	   (path2 (when out (make-pathname :type "xml"
					   :name name
					   :defaults out)))
	   (parse2 (and out (probe-file path2)
		     (with-open-file (s path2) (funcall *parse-xml* s)))))
      (if (probe-file path)
	  (if out
	      (equalp parse1 parse2)
	    parse1)
	(list :no-file name)))))


(defun test-one (&key file out parse stop)
  (cond ((null stop)
	 (ignore-errors (test-one-inner file out parse)))
	((string-equal (pathname-name file) stop)
	 (test-one-inner file out parse))
	(t :skipped)))

(defun test-one-inner (file out parse)
  (let* ((fl (probe-file file))
	 (*test-xml-file* (or fl ""))
	 (parse0
	  (when fl
	    (with-open-file (s file) 
			    (funcall *parse-xml* s :content-only t))))
	 (parse1
	  (when fl
	    (with-open-file (s file) 
			    (funcall *parse-xml* 
				     s :content-only (eq parse :content-only)))))
	 (path2 (when out (make-pathname :type "xml"
					 :name (pathname-name file)
					 :defaults out)))
	 (parse2 (and path2 (probe-file path2)
		      (with-open-file (s path2) 
				      (funcall *parse-xml* s :content-only t)))))
    (if fl
	(if (and path2 (probe-file path2))
	    (if (equal parse0 parse2)
		(if parse parse1 :equal)
	      (if parse 
		  (values nil (list :not-equal parse1 parse2))
		(values nil :not-equal)))
	  (if parse parse1 :parsed))
      (values nil (list :no-file (pathname-name file))))))

(defun test-error (e &optional (ln 45))
  (let* ((s (format nil "~A" e))
	 (l (length s)))
    (cond ((and (null ln) (not (typep e 'condition)))
	   e)
	  ((null ln) s)
	  (t (when (< ln l) (setf s (subseq s 0 ln)))
	     (dotimes (i (length s))
	       (when (< (char-code (elt s i)) 14)
		 (setf (elt s i) #\?)))
	     s))))


(defun test-all-folders (&key root prefix fail pass out file parse 
			      pass-invalid errlen
			      &aux (all 0) good bad maybe-good maybe-bad files)
  (dolist (fl (directory root) (values 
				(list all
				      (length bad)
				      (length good) 
				      (length maybe-bad) 
				      (length maybe-good))
				bad 
				good 
				maybe-bad 
				maybe-good
				files
				))
    (when (file-directory-p fl)
      (multiple-value-bind (a b g mb mg ff)
	  (test-one-folder :root (test-folder fl nil) :file file
			   :prefix prefix :fail fail :pass pass :parse parse
			   :pass-invalid pass-invalid :errlen errlen
			   :out (if (stringp out)
				    (test-folder out fl)
				  out))
	(incf all (first a))
	(setf bad (append bad b))
	(setf good (append good g))
	(setf maybe-bad (append maybe-bad mb))
	(setf maybe-good (append maybe-good mg))
	(setf files (append files ff))
	))))
 
(defun test-one-folder (&key root prefix fail pass out file parse 
			     pass-invalid errlen
			     &aux (all 0) good bad res maybe-good maybe-bad files)
  (dolist (fl (directory root) (values 
				(list all
				      (length bad)
				      (length good) 
				      (length maybe-bad) 
				      (length maybe-good))
				(reverse bad) 
				(reverse good) 
				(reverse maybe-bad) 
				(reverse maybe-good)
				(reverse files)
				))
    (let* ((type (pathname-type fl))
	   (name (pathname-name fl))
	   flag)
      (when (and (equalp "xml" type)
		 (or (null prefix)
		     (eql 0 (search prefix name))))
	(when (member name pass-invalid :test 'string-equal)
	  (setf flag :pass))
	(or flag
	    (case pass
	      ((nil) nil)
	      (:all (setf flag :pass))
	      (otherwise
	       (dolist (p (if (consp pass) pass (list pass)))
		 (when (search p name) 
		   (setf flag :pass) (return))))))
	(or flag
	    (case fail
	      ((nil) nil)
	      (:all (setf flag :fail))
	      (otherwise 
	       (dolist (p (if (consp fail) fail (list fail)))
		 (when (search p name) 
		   (setf flag :fail) (return))))))
	(multiple-value-bind (v e)
	    (test-one :file fl :out out :parse parse :stop file)
	  (case v
	    (:skipped (setf res (list nil :skipped)))
	    (otherwise
	     (incf all)
	     (setf res
		   (case flag
		     (:pass (if (and v (null e))
				(list name :good v)
			      (list name :bad (test-error e errlen))))
		     (:fail (if (and (null v) e)
				(list name :good (test-error e errlen))
			      (list name :bad (if e
						 (test-error e errlen)
					       v))))
		     (otherwise (if (and v (null e))
				    (list name :parsed v)
				  (list name :error (test-error e errlen)))))))))
	(push res files)
	(case (second res)
	  (:skipped nil)
	  (:good (push res good))
	  (:parsed (push res maybe-good))
	  (:error  (push res maybe-bad))
	  (otherwise (push res bad)))
	))))
		   

	   

(defvar *test-root*)
(defun test-folder (sub &optional root) 
  (or root
      (when (and (boundp '*test-root*) (probe-file *test-root*))
	(setf root *test-root*))
      (error "Variable ~S must be set to a valid folder path." '*test-root*))
  (let* ((p (namestring (if root (merge-pathnames sub root) sub)))
	 (sl #+unix "/" #+mswindows "\\")
	 (ln (length p)))
    (if (eql (elt sl 0) (elt p (1- ln)))
	(pathname p)
      (pathname (concatenate 'string p sl)))))

(defun test-oasis (case1 case2 
			 &key file parse (errlen 50) (root (test-folder "oasis/")))
  (declare (ignore case1 case2))
  (test-one-folder :root root :prefix "p" :pass "pass" :fail "fail"
		   :pass-invalid '(
				   "p06fail1"
				   "p08fail1"
				   "p08fail2"
				   "p16fail3"
				   )
		   :errlen errlen :parse parse :file file))

(defun test-sun (case case2 &key file parse (errlen 50) (root (test-folder "sun/")))
  (declare (ignore case2))
  (case case
    (:valid   (test-one-folder :root (test-folder "valid/" root)
			       :out (test-folder "valid/out/" root)
			       :errlen errlen :parse parse 
			       :file file :pass :all :fail nil))
    (:invalid (test-one-folder :root (test-folder "invalid/" root)
			       :errlen errlen :parse parse 
			       :file file :pass :all :fail nil))
    (:not-wf  (test-one-folder :root (test-folder "not-wf/" root)
			       :errlen errlen :parse parse 
			       :file file :pass nil :fail :all))))

(defun test-japanese (case case2 &key file parse (errlen 50)
			   (root (test-folder "japanese/")))
  (declare (ignore case case2))
  (test-one-folder :root root
		   :errlen errlen
		   :parse parse
		   :file file
		   :pass :all
		   :fail nil))
	      
(defun test-xml (case sub-case 
		  &key file parse (errlen 50) (root (test-folder "xmltest/")))
  (case case
    (:valid   (case sub-case
		(:sa     (test-one-folder :root (test-folder "valid/sa/" root)
					  :out (test-folder "valid/sa/out/" root)
					  :errlen errlen :parse parse 
					  :file file :pass :all :fail nil))
		(:not-sa (test-one-folder :root (test-folder "valid/not-sa/" root)
					  :out (test-folder "valid/not-sa/out/" root)
					  :errlen errlen :parse parse 
					  :file file :pass :all :fail nil))
		(:ext-sa (test-one-folder :root (test-folder "valid/ext-sa/" root)
					  :out (test-folder "valid/ext-sa/out/" root)
					  :errlen errlen :parse parse 
					  :file file :pass :all :fail nil))))
    (:invalid (test-one-folder :root (test-folder "invalid/" root)
			       :errlen errlen :parse parse 
			       :file file :pass :all :fail nil))
    (:not-wf  (case sub-case
		(:sa     (test-one-folder :root (test-folder "not-wf/sa/" root)
					  :errlen errlen :parse parse 
					  :file file :pass nil :fail :all))
		(:not-sa (test-one-folder :root (test-folder "not-wf/not-sa/" root)
					  :errlen errlen :parse parse 
					  :file file :pass nil :fail :all))
		(:ext-sa (test-one-folder 
			  :root (test-folder "not-wf/ext-sa/" root)
			  :errlen errlen :parse parse 
			  :file file :pass nil :fail :all))))))

	    
  
(defun test-ibm (case case2 &key file parse (errlen 50) (root (test-folder "ibm/")))
  (declare (ignore case2))
  (case case
    (:valid   (test-all-folders :root (test-folder "valid/" root)
				:out "out/" :file file :parse parse
				:prefix "ibm" :errlen errlen
				:pass :all :fail nil))
    (:invalid (test-all-folders :root (test-folder "invalid/" root)
				:out "out/" :file file :parse parse
				:prefix "ibm" :errlen errlen
				:pass :all :fail nil))
    (:not-wf  (test-all-folders :root (test-folder "not-wf/" root)
				:prefix "ibm" :file file :parse parse
				:errlen errlen :pass nil :fail :all))))


(eval-when (compile load eval)
  (defmacro char-state () 
    (format nil "chars:~S  case~S"
	    excl:real-char-code-limit *current-case-mode*)))
 
(defparameter *xml-compile-state* (char-state))

(defvar *test-prefix* "test")

(defun test-all (&key case 
		      parser file parse (errlen 15) 
		      log (report (if log :all :count))
		      &aux (all 0) good bad maybe-good maybe-bad a)
  
  ;; report -> :count  :each  :final  :good  :bad  :maybe-good  :maybe-bad

  (when parser (setf *parse-xml* parser))

  (typecase log
    ((or string pathname)
     (setf log (merge-pathnames (pathname log) (make-pathname :type "log"))))
    ((member :new)
     (multiple-value-bind (s m h d mo) 
	 (get-decoded-time)
       (declare (ignore s m h))
       (let (suffix)
	 (loop
	  (setf log (merge-pathnames 
		     (pathname (format nil "~A-~A-~A~A~A" *test-prefix* mo d 
				       (if suffix "-" "") (or suffix "")))
		     (make-pathname :type "log")))
	  (or (probe-file log) (return))
	  (if suffix (incf suffix) (setf suffix 1))))))
    ((member nil) nil)
    (otherwise (setf log (merge-pathnames 
			  (pathname *test-prefix*) 
			  (make-pathname :type "log")))))

  (unwind-protect
      (labels 
	  ((report-nums 
	    (report marker a)
	    (when (report-if report :each :final :all)
	       (let* ((hd (format nil "all: ~4D  bad: ~4D  good: ~4D  "
				  (pop a) (pop a) (pop a)))
		      (tl (if (and (eql 0 (first a)) (eql 0 (second a)))
			      ""
			    (format nil "maybe-bad: ~4D  maybe-good ~4D"
				    (first a) (second a)))))
		 (format t "~&~A ~A~A~%" marker hd tl))))

	   (report-test
	    (report marker kind res)
	    (when res
	      (when (report-if report kind)
		(format t "~&~A ~S~%" marker kind)
		(format t "~{~S~%~}~%" res))))

	   (report-if (report &rest kinds)
		      (dolist (kind kinds)
			(when (or (eq report kind) 
				  (and (consp report) (member kind report)))
			  (return t))))
	   )
	      
	(macrolet 
	    ((report-one
	      (form &aux file)
	      `(let (marker)
		 ,(when (setf file (member :file form))
		    `(setf file ,(second file)))
		 (setf marker 
		       (format nil ";; ~2D ~10A ~7A ~7A ~10A "
			       i ',(first form) ',(second form) ',(third form)
			       (or file "---all---")))
		 (when (report-if report :each) (format t "~&~A~%" marker))
		 (multiple-value-bind (a b g mb mg all)
		     ,form
		   (report-nums report marker a)
		   (report-test report marker :bad b)
		   (report-test report marker :good g)
		   (report-test report marker :maybe-bad mb)
		   (report-test report marker :maybe-good mg)
		   (report-test report marker :all all)
		   (values a b g mb mg)))))

      
	  (when log (dribble log))
	  (format t "~&~%;;; Compiled as ~A ~%" *xml-compile-state*)
	  (format t     ";;;  Running as ~A ~%~%" (eval '(char-state)))
	  (dotimes (i 15)
	    (when (or (null case) (eql case i))
	      (multiple-value-bind (a b g mb mg)

		  (case i
		    (0 (report-one 
			(test-oasis nil nil :file file :parse parse :errlen errlen)))
		    (1 (report-one 
			(test-xml :valid :sa 
				  :file file :parse parse :errlen errlen)))
		    (2 (report-one 
			(test-xml :valid :not-sa 
				  :file file :parse parse :errlen errlen)))
		    (3 (report-one 
			(test-xml :valid :ext-sa 
				  :file file :parse parse :errlen errlen)))
		    (4 (report-one 
			(test-xml :invalid nil 
				  :file file :parse parse :errlen errlen)))
		    (5 (report-one 
			(test-xml :not-wf :sa 
				  :file file :parse parse :errlen errlen)))
		    (6 (report-one 
			(test-xml :not-wf :not-sa 
				  :file file :parse parse :errlen errlen)))
		    (7 (report-one 
			(test-xml :not-wf :ext-sa 
				  :file file :parse parse :errlen errlen)))
		    (8 (report-one 
			(test-sun :valid nil
				  :file file :parse parse :errlen errlen)))
		    (9 (report-one 
			(test-sun :invalid nil
				  :file file :parse parse :errlen errlen)))
		    (10 (report-one 
			 (test-sun :not-wf nil
				   :file file :parse parse :errlen errlen)))
		    (11 (report-one 
			 (test-ibm :valid nil
				   :file file :parse parse :errlen errlen)))
		    (12 (report-one 
			 (test-ibm :invalid nil
				   :file file :parse parse :errlen errlen)))
		    (13 (report-one 
			 (test-ibm :not-wf nil
				   :file file :parse parse :errlen errlen)))
		    (14 (report-one
			 (test-japanese nil nil :file file :parse parse
					:errlen errlen)))
		    )
		(incf all (first a))
		(setf bad (append b bad))
		(setf good (append g good))
		(setf maybe-bad (append mb maybe-bad))
		(setf maybe-good (append mg maybe-good)))))

	  (setf a (list all
			(length bad)
			(length good) 
			(length maybe-bad) 
			(length maybe-good)))
	  (cond
	    ((report-if report :each)
	     (report-nums :final ";;;     Summary              ==> " a)
	     a) 
	    ((report-if report :count) a)
	    ((report-if report :bad) (reverse bad))
	    ((report-if report :all)
	     (report-nums :final ";;;     Summary              ==> " a)
	     (values a bad good maybe-bad maybe-good)))


	  ))
    (when log (dribble))))


(defun test-logs (new old)
  (setf new (merge-pathnames (pathname new) (make-pathname :type "log")))
  (setf old (merge-pathnames (pathname old) (make-pathname :type "log")))
  (with-open-file
   (n new)
   (with-open-file
    (o old)
    (let (nl ol res (count 0) res2)
      ;; skip the "dribbling" line
      (read-line n)
      (read-line o)

      (setf res2
	    (loop
	     (loop
	      (when (setf nl (ignore-errors (read n nil :eof)))
		(return))
	      (read-line n))
	     (loop
	      (when (setf ol (ignore-errors (read o nil :eof)))
		(return))
	      (read-line n))
	     ;;(format t "~&~S~%~S~%~%" nl ol)
	     (incf count)
	     (if (eq :eof nl)
		 (if (eq :eof ol)
		     (return :done)
		   (return (list :more-old ol)))
	       (if (eq :eof ol)
		   (return (list :more-new nl))
		 (cond
		  ((equal nl ol))
		  ((or (atom nl) (atom ol))
		   (return (list :atom nl ol)))
		  ((not (equal (car nl) (car ol)))
		   (return (list :out-of-seq nl ol)))
		  ((eq (second nl) (second ol)))
		  ((eq (second nl) :good))
		  (t (push (list nl ol) res)))))))
      (values
       (reverse res)
       res2
       count)))))

(defun default-file-callback (filename token &optional public)
  (declare (ignorable token public))
  ;;(format t "filename: ~s token: ~s public: ~s~%" filename token public)
  (multiple-value-bind (v e)
      (ignore-errors 
	(values
	 (open 
	  (merge-pathnames
	   (net.uri:uri-path filename)
	   *test-xml-file*
	   ))))
    (when e
      (format t "~&;; default-file-callback: ~S~%" filename)
      (format t "~&;;  error: ~A ~%" e))
    v))

(defun default-parse-xml (stream &rest opts)
  (apply 'net.xml.parser:parse-xml stream 
	 :external-callback 'default-file-callback opts))




;;; NAMESPACE TESTS


(defun n1 ()
  ;; 23-Apr-03  mm: parsed incorrectly
  ;;            foo:fooelement1 is not in the foo namespace package
  (parse-xml 
   "<foo:fooelement1 xmlns:foo='url1'><foo:fooelt2>body</foo:fooelt2></foo:fooelement1>"
   ))

(defun n2 ()
  ;; 23-Apr-03  mm: parsed incorrectly
  ;;            fooelement1 is not in the default namespace package
  (parse-xml 
   "<fooelement1 xmlns='url1'><fooelt2>body</fooelt2></fooelement1>"
   ))

(defun n3 ()
  ;; 23-Apr-03  mm: parsed incorrectly
  ;;            foo:barelt1 is in the foo namespace package
  ;;                        should be in the bar namespace package
  (parse-xml 
   "<foo:fooelement1 xmlns:foo='url1'><foo:fooelt2><foo:barelt1 xmlns:foo='url2'><foo:barelt2>body</foo:barelt2></foo:barelt1><foo:fooelt3/></foo:fooelt2></foo:fooelement1>"
   ))


(defun n4 ()
  (parse-xml "<nsa:atag1 nsa:aattr1='17' xmlns:nsa='urla'/>"))

(defun n5 ()
  (or (find-package :n5) (make-package :n5 :use nil))
  (parse-xml "<nsa:atag1 nsa:aattr1='17' xmlns:nsa='urla'/>"
	     :uri-to-package
	     (list (cons (net.uri:parse-uri "urla")
			 (find-package :n5)))
	     ))



(defun time-xml (&optional (fl "tempest.xml"))
  (with-open-file
   (s fl)
   (time
    (consp
     (net.xml.parser:parse-xml s :external-callback 'default-file-callback
			       :content-only t)))))


