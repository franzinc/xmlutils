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
      xmlns='urn:royal-mail.gov.uk/XML/ns/postal.ns,1999'>
    <bib:book owner='Smith'>
       <bib:title>A Tale of Two Cities</bib:title>
       <bib:bibliography
         xmlns:bib='http://www.franz.com/XML/bib.ns'
         xmlns='urn:royal-mail2.gov.uk/XML/ns/postal.ns,1999'>
        <bib:library branch='Main'>UK Library</bib:library>
        <bib:date calendar='Julian'>1999</bib:date>
        </bib:bibliography>
       <bib:date calendar='Julian'>1999</bib:date>
       </bib:book>
     </bibliography>")