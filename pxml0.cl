(sys:defpatch "pxml" 1
  "Version 7.0.1, needed by new SOAP interface."
  :type :system
  :post-loadable t)

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
;; pxml.cl - parse xml
;;
;; Change Log
;;
;; 12/05/00 changes to allow using in ANSI mode lisp
;; 12/20/00 namespace example fix; correct whitespace bug when
;;          looking for xml? tag in external entity files
;;

(defpackage net.xml.parser
  (:use :lisp :clos :excl :net.uri)
  (:export
   #:parse-xml
   #:pxml-version
   ))

(in-package :net.xml.parser)

(unless (fboundp 'pxml-dribble-bug-hook)
  (let ((pxml-version-strings nil))
    (defun pxml-dribble-bug-hook (stream-or-string)
      (if (stringp stream-or-string)
	  (push stream-or-string pxml-version-strings)
	(loop for string in (reverse pxml-version-strings)
	    do (write-string string stream-or-string)
	       (terpri stream-or-string))))

    (push 'pxml-dribble-bug-hook excl:*dribble-bug-hooks*)))

(funcall 'pxml-dribble-bug-hook "$Id: pxml0.cl,v 1.2.2.2.22.3 2003/07/28 20:17:26 layer Exp $")

(defparameter *pxml-version* (list 7 0 1))
(defun pxml-version (&optional v1-or-s v2 v3 error-p &aux (v1 v1-or-s))
  (typecase v1
    (integer (if (or (< (first *pxml-version*) v1)
		     (and v2
			  (or 
			   (and (= (first *pxml-version*) v1)
				(< (second *pxml-version*) v2))
			   (and v3
				(= (second *pxml-version*) v2)
				(< (third *pxml-version*) v3)))))
		 (if error-p
		     (error "PXML Version ~A.~A.~A needed, but ~{~A.~A.~A~} is loaded."
			    v1 (or v2 0) (or v3 0) *pxml-version*)
		   nil)
	       *pxml-version*))
    (otherwise (format v1-or-s "PXML Version ~{~A.~A.~A~}" *pxml-version*))))
    

(eval-when (compile eval)

  (defun xml-char-q (code)
    (if* (eq code #x9) then t
	 elseif (eq code #xA) then t
	 elseif (eq code #xD) then t
	 elseif (<= #x20 code #xD7FF) then t
	 elseif (<= #xE000 code #xFFFD) then t
	 else nil))

  (defun xml-space-q (code)
    (or (eq code #x20)
	(eq code #x9)
	(eq code #xD)
	(eq code #xA)))

  #+unused
  (defmacro xml-eql-char (char) `(eq ,char #\=))

  (defun xml-base-char-q (code)
    (or (<= #x0041 code #x005A) (<= #x0061 code #x007A)
	(<= #x00C0 code #x00D6) (<= #x00D8 code #x00F6)
	(<= #x00F8 code #x00FF) (<= #x0100 code #x0131)
	(<= #x0134 code #x013E) (<= #x0141 code #x0148)
	(<= #x014A code #x017E) (<= #x0180 code #x01C3)
	(<= #x01CD code #x01F0) (<= #x01F4 code #x01F5)
	(<= #x01FA code #x0217) (<= #x0250 code #x02A8)
	(<= #x02BB code #x02C1) (= code #x0386) (<= #x0388 code #x038A)
	(= code #x038C) (<= #x038E code #x03A1) (<= #x03A3 code #x03CE)
	(<= #x03D0 code #x03D6) (= code #x03DA) (= code #x03DC) (= code #x03DE)
	(= code #x03E0) (<= #x03E2 code #x03F3) (<= #x0401 code #x040C)
	(<= #x040E code #x044F) (<= #x0451 code #x045C)
	(<= #x045E code #x0481) (<= #x0490 code #x04C4)
	(<= #x04C7 code #x04C8) (<= #x04CB code #x04CC)
	(<= #x04D0 code #x04EB) (<= #x04EE code #x04F5)
	(<= #x04F8 code #x04F9) (<= #x0531 code #x0556) (= code #x0559)
	(<= #x0561 code #x0586) (<= #x05D0 code #x05EA)
	(<= #x05F0 code #x05F2) (<= #x0621 code #x063A)
	(<= #x0641 code #x064A) (<= #x0671 code #x06B7)
	(<= #x06BA code #x06BE) (<= #x06C0 code #x06CE)
	(<= #x06D0 code #x06D3) (= code #x06D5) (<= #x06E5 code #x06E6)
	(<= #x0905 code #x0939) (= code #x093D) (<= #x0958 code #x0961)
	(<= #x0985 code #x098C) (<= #x098F code #x0990)
	(<= #x0993 code #x09A8) (<= #x09AA code #x09B0) (= code #x09B2)
	(<= #x09B6 code #x09B9) (<= #x09DC code #x09DD)
	(<= #x09DF code #x09E1) (<= #x09F0 code #x09F1)
	(<= #x0A05 code #x0A0A) (<= #x0A0F code #x0A10)
	(<= #x0A13 code #x0A28) (<= #x0A2A code #x0A30)
	(<= #x0A32 code #x0A33) (<= #x0A35 code #x0A36)
	(<= #x0A38 code #x0A39) (<= #x0A59 code #x0A5C) (= code #x0A5E)
	(<= #x0A72 code #x0A74) (<= #x0A85 code #x0A8B) (= code #x0A8D)
	(<= #x0A8F code #x0A91) (<= #x0A93 code #x0AA8)
	(<= #x0AAA code #x0AB0) (<= #x0AB2 code #x0AB3)
	(<= #x0AB5 code #x0AB9) (<= #x0ABD code #x0AE0)
	(<= #x0B05 code #x0B0C) (<= #x0B0F code #x0B10)
	(<= #x0B13 code #x0B28) (<= #x0B2A code #x0B30)
	(<= #x0B32 code #x0B33) (<= #x0B36 code #x0B39) (= code #x0B3D)
	(<= #x0B5C code #x0B5D) (<= #x0B5F code #x0B61)
	(<= #x0B85 code #x0B8A) (<= #x0B8E code #x0B90)
	(<= #x0B92 code #x0B95) (<= #x0B99 code #x0B9A) (= code #x0B9C)
	(<= #x0B9E code #x0B9F) (<= #x0BA3 code #x0BA4)
	(<= #x0BA8 code #x0BAA) (<= #x0BAE code #x0BB5)
	(<= #x0BB7 code #x0BB9) (<= #x0C05 code #x0C0C)
	(<= #x0C0E code #x0C10) (<= #x0C12 code #x0C28)
	(<= #x0C2A code #x0C33) (<= #x0C35 code #x0C39)
	(<= #x0C60 code #x0C61) (<= #x0C85 code #x0C8C)
	(<= #x0C8E code #x0C90) (<= #x0C92 code #x0CA8)
	(<= #x0CAA code #x0CB3) (<= #x0CB5 code #x0CB9) (= code #x0CDE)
	(<= #x0CE0 code #x0CE1) (<= #x0D05 code #x0D0C)
	(<= #x0D0E code #x0D10) (<= #x0D12 code #x0D28)
	(<= #x0D2A code #x0D39) (<= #x0D60 code #x0D61)
	(<= #x0E01 code #x0E2E) (= code #x0E30) (<= #x0E32 code #x0E33)
	(<= #x0E40 code #x0E45) (<= #x0E81 code #x0E82) (= code #x0E84)
	(<= #x0E87 code #x0E88) (= code #x0E8A) (= code #x0E8D)
	(<= #x0E94 code #x0E97) (<= #x0E99 code #x0E9F)
	(<= #x0EA1 code #x0EA3) (= code #x0EA5) (= code #x0EA7)
	(<= #x0EAA code #x0EAB) (<= #x0EAD code #x0EAE) (= code #x0EB0)
	(<= #x0EB2 code #x0EB3) (= code #x0EBD) (<= #x0EC0 code #x0EC4)
	(<= #x0F40 code #x0F47) (<= #x0F49 code #x0F69)
	(<= #x10A0 code #x10C5) (<= #x10D0 code #x10F6) (= code #x1100)
	(<= #x1102 code #x1103) (<= #x1105 code #x1107) (= code #x1109)
	(<= #x110B code #x110C) (<= #x110E code #x1112) (= code #x113C)
	(= code #x113E) (= code #x1140) (= code #x114C) (= code #x114E) (= code #x1150)
	(<= #x1154 code #x1155) (= code #x1159) (<= #x115F code #x1161)
	(= code #x1163) (= code #x1165) (= code #x1167) (= code #x1169)
	(<= #x116D code #x116E) (<= #x1172 code #x1173) (= code #x1175)
	(= code #x119E) (= code #x11A8) (= code #x11AB) (<= #x11AE code #x11AF)
	(<= #x11B7 code #x11B8) (= code #x11BA) (<= #x11BC code #x11C2)
	(= code #x11EB) (= code #x11F0) (= code #x11F9) (<= #x1E00 code #x1E9B)
	(<= #x1EA0 code #x1EF9) (<= #x1F00 code #x1F15)
	(<= #x1F18 code #x1F1D) (<= #x1F20 code #x1F45)
	(<= #x1F48 code #x1F4D) (<= #x1F50 code #x1F57) (= code #x1F59)
	(= code #x1F5B) (= code #x1F5D) (<= #x1F5F code #x1F7D)
	(<= #x1F80 code #x1FB4) (<= #x1FB6 code #x1FBC) (= code #x1FBE)
	(<= #x1FC2 code #x1FC4) (<= #x1FC6 code #x1FCC)
	(<= #x1FD0 code #x1FD3) (<= #x1FD6 code #x1FDB)
	(<= #x1FE0 code #x1FEC) (<= #x1FF2 code #x1FF4)
	(<= #x1FF6 code #x1FFC) (= code #x2126) (<= #x212A code #x212B)
	(= code #x212E) (<= #x2180 code #x2182) (<= #x3041 code #x3094)
	(<= #x30A1 code #x30FA) (<= #x3105 code #x312C)
	(<= #xAC00 code #xD7A3)
	))

  (defun xml-ideographic-q (code)
    (or (<= #x4E00 code #x9FA5) (= code #x3007) (<= #x3021 code #x3029)))

  (defun xml-combining-char-q (code)
    (or (<= #x0300 code #x0345) (<= #x0360 code #x0361)
	(<= #x0483 code #x0486) (<= #x0591 code #x05A1)
	(<= #x05A3 code #x05B9) (<= #x05BB code #x05BD) (= code #x05BF)
	(<= #x05C1 code #x05C2) (= code #x05C4) (<= #x064B code #x0652)
	(= code #x0670) (<= #x06D6 code #x06DC) (<= #x06DD code #x06DF)
	(<= #x06E0 code #x06E4) (<= #x06E7 code #x06E8)
	(<= #x06EA code #x06ED) (<= #x0901 code #x0903) (= code #x093C)
	(<= #x093E code #x094C) (= code #x094D) (<= #x0951 code #x0954)
	(<= #x0962 code #x0963) (<= #x0981 code #x0983) (= code #x09BC)
	(<= #x09BE code #x09BF) (<= #x09C0 code #x09C4)
	(<= #x09C7 code #x09C8) (<= #x09CB code #x09CD) (= code #x09D7)
	(<= #x09E2 code #x09E3) (= code #x0A02) (= code #x0A3C) (= code #x0A3E)
	(= code #x0A3F) (<= #x0A40 code #x0A42) (<= #x0A47 code #x0A48)
	(<= #x0A4B code #x0A4D) (<= #x0A70 code #x0A71)
	(<= #x0A81 code #x0A83) (= code #x0ABC) (<= #x0ABE code #x0AC5)
	(<= #x0AC7 code #x0AC9) (<= #x0ACB code #x0ACD)
	(<= #x0B01 code #x0B03) (= code #x0B3C) (<= #x0B3E code #x0B43)
	(<= #x0B47 code #x0B48) (<= #x0B4B code #x0B4D)
	(<= #x0B56 code #x0B57) (<= #x0B82 code #x0B83)
	(<= #x0BBE code #x0BC2) (<= #x0BC6 code #x0BC8)
	(<= #x0BCA code #x0BCD) (= code #x0BD7) (<= #x0C01 code #x0C03)
	(<= #x0C3E code #x0C44) (<= #x0C46 code #x0C48)
	(<= #x0C4A code #x0C4D) (<= #x0C55 code #x0C56)
	(<= #x0C82 code #x0C83) (<= #x0CBE code #x0CC4)
	(<= #x0CC6 code #x0CC8) (<= #x0CCA code #x0CCD)
	(<= #x0CD5 code #x0CD6) (<= #x0D02 code #x0D03)
	(<= #x0D3E code #x0D43) (<= #x0D46 code #x0D48)
	(<= #x0D4A code #x0D4D) (= code #x0D57) (= code #x0E31)
	(<= #x0E34 code #x0E3A) (<= #x0E47 code #x0E4E) (= code #x0EB1)
	(<= #x0EB4 code #x0EB9) (<= #x0EBB code #x0EBC)
	(<= #x0EC8 code #x0ECD) (<= #x0F18 code #x0F19) (= code #x0F35)
	(= code #x0F37) (= code #x0F39) (= code #x0F3E) (= code #x0F3F)
	(<= #x0F71 code #x0F84) (<= #x0F86 code #x0F8B)
	(<= #x0F90 code #x0F95) (= code #x0F97) (<= #x0F99 code #x0FAD)
	(<= #x0FB1 code #x0FB7) (= code #x0FB9) (<= #x20D0 code #x20DC)
	(= code #x20E1) (<= #x302A code #x302F) (= code #x3099) (= code #x309A)
	))

  (defun xml-digit-q (code)
    (or (<= #x0030 code #x0039) (<= #x0660 code #x0669)
	(<= #x06F0 code #x06F9) (<= #x0966 code #x096F)
	(<= #x09E6 code #x09EF) (<= #x0A66 code #x0A6F)
	(<= #x0AE6 code #x0AEF) (<= #x0B66 code #x0B6F)
	(<= #x0BE7 code #x0BEF) (<= #x0C66 code #x0C6F)
	(<= #x0CE6 code #x0CEF) (<= #x0D66 code #x0D6F)
	(<= #x0E50 code #x0E59) (<= #x0ED0 code #x0ED9)
	(<= #x0F20 code #x0F29)
	))

  (defun xml-extender-q (code)
    (or (= code #x00B7) (= code #x02D0) (= code #x02D1) (= code #x0387) (= code #x0640)
	(= code #x0E46) (= code #x0EC6) (= code #x3005) (<= #x3031 code #x3035)
	(<= #x309D code #x309E) (<= #x30FC code #x30FE)
	))

  (defun xml-letter-q (char)
    (or (xml-base-char-q char) (xml-ideographic-q char)))

  (defun xml-name-char-q (code)
    (or (xml-letter-q code)
	(xml-digit-q code) 
	(eq code (char-code #\.))
	(eq code (char-code #\-))
	(eq code (char-code #\_))
	(eq code (char-code #\:))
	(xml-combining-char-q code)
	(xml-extender-q code)))

  (defun xml-name-start-char-q (code)
    (or (xml-letter-q code)
	(eq (char-code #\_) code)
	(eq (char-code #\:) code)
	))


  )

(eval-when (compile load eval)
  (defconstant xml-base           #x01)
  (defconstant xml-ideographic    #x02)
  (defconstant xml-extender       #x04)
  (defconstant xml-combining-char #x08)
  (defconstant xml-name-char      #x10)
  (defconstant xml-name-start     #x20)

  (defconstant xml-other          #x40)
  (defconstant xml-space          #x80)
  (defconstant xml-digit          #xc0)

  (defconstant xml-char           #xc0)
  (defconstant xml-letter         #x03)
  )

(eval-when (compile eval)

  (defmacro make-table (&aux (chars (make-array
				     #x10000 
				     :element-type '(unsigned-byte 8)
				     :initial-element 0
				     )))
    (dotimes (i #x10000 chars)
      #+ignore
      (when (eql #xff (logand i #xff))
	;; When macro is interpreted, this loop takes a long long time
	;;  so it is nice to see some progress...
	(format t ";")
	(when (eql #x3fff (logand i #x3fff))
	  (format t "~%")))
      (setf (elt chars i)
	    (if (xml-char-q i)
		(logior
		 (cond ((xml-space-q i) xml-space)
		       ((xml-digit-q i) xml-digit)
		       (t xml-other))
		 (if (xml-base-char-q i)       xml-base           0)
		 (if (xml-ideographic-q i)     xml-ideographic    0)
		 (if (xml-combining-char-q i)  xml-combining-char 0)
		 (if (xml-extender-q i)        xml-extender       0)
		 (if (xml-name-char-q i)       xml-name-char      0)
		 (if (xml-name-start-char-q i) xml-name-start     0)
		 )
	      0))
      ))

  )
 
(eval-when (compile)
  (compile 'xml-char-q)
  (compile 'xml-space-q)
  (compile 'xml-base-char-q)
  (compile 'xml-ideographic-q)
  (compile 'xml-combining-char-q)
  (compile 'xml-digit-q)
  (compile 'xml-extender-q)
  (compile 'xml-letter-q)
  (compile 'xml-name-char-q)
  (compile 'xml-name-start-char-q)
  (compile 'make-table))

(defparameter *xml-chars* (make-table))
  
(defun xml-char-p (c &optional (table *xml-chars*))
  (not (eql 0 (logand xml-char (elt table (char-code c))))))
(define-compiler-macro xml-char-p (&whole form c &optional (tvar '*xml-chars*))
  (if (and (symbolp c) (symbolp tvar))
      `(not (eql 0 (logand xml-char 
			   (the fixnum
			     (aref
			      (the (simple-array (unsigned-byte 8) (*))
				,tvar)
			      (the (integer 0 #xffff) 
				(char-code (the character ,c))))))))
    form))

;;#+debug
(defun test-char-p (x)
  (declare (optimize (speed 3) (safety 1))
	   (:explain :calls))
  (xml-char-p x))

(defun xml-space-p (c &optional (table *xml-chars*))
  (eql xml-space (elt table (char-code c))))
(define-compiler-macro xml-space-p (&whole form c &optional (tvar '*xml-chars*))
  (if (and (symbolp c) (symbolp tvar))
      `(eql xml-space (the fixnum
			(aref
			 (the (simple-array (unsigned-byte 8) (*))
			   ,tvar)
			 (the (integer 0 #xffff) 
			   (char-code (the character ,c))))))
    form))


(defun xml-base-char-p (c &optional (table *xml-chars*))
  (not (eql 0 (logand xml-base (elt table (char-code c))))))
(define-compiler-macro xml-base-char-p (&whole form c &optional (tvar '*xml-chars*))
  (if (and (symbolp c) (symbolp tvar))
      `(not (eql 0 (logand xml-base
			   (the fixnum
			     (aref
			      (the (simple-array (unsigned-byte 8) (*))
				,tvar)
			      (the (integer 0 #xffff) 
				(char-code (the character ,c))))))))
    form))

(defun xml-ideographic-p (c &optional (table *xml-chars*))
  (not (eql 0 (logand xml-ideographic (elt table (char-code c))))))
(define-compiler-macro xml-ideographic-p (&whole form c &optional (tvar '*xml-chars*))
  (if (and (symbolp c) (symbolp tvar))
      `(not (eql 0 (logand xml-ideographic
			   (the fixnum
			     (aref
			      (the (simple-array (unsigned-byte 8) (*))
				,tvar)
			      (the (integer 0 #xffff) 
				(char-code (the character ,c))))))))
    form))


(defun xml-combining-char-p (c &optional (table *xml-chars*))
  (not (eql 0 (logand xml-combining-char (elt table (char-code c))))))
(define-compiler-macro xml-combining-char-p (&whole form c &optional (tvar '*xml-chars*))
  (if (and (symbolp c) (symbolp tvar))
      `(not (eql 0 (logand xml-combining-char
			   (the fixnum
			     (aref
			      (the (simple-array (unsigned-byte 8) (*))
				,tvar)
			      (the (integer 0 #xffff) 
				(char-code (the character ,c))))))))
    form))


(defun xml-digit-p (c &optional (table *xml-chars*))
  (eql xml-digit (logand xml-digit (elt table (char-code c)))))
(define-compiler-macro xml-digit-p (&whole form c &optional (tvar '*xml-chars*))
  (if (and (symbolp c) (symbolp tvar))
      `(eql xml-digit (logand xml-digit
			   (the fixnum
			     (aref
			      (the (simple-array (unsigned-byte 8) (*))
				,tvar)
			      (the (integer 0 #xffff) 
				(char-code (the character ,c)))))))
    form))


(defun xml-extender-p (c &optional (table *xml-chars*))
  (not (eql 0 (logand xml-extender (elt table (char-code c))))))
(define-compiler-macro xml-extender-p (&whole form c &optional (tvar '*xml-chars*))
  (if (and (symbolp c) (symbolp tvar))
      `(not (eql 0 (logand xml-extender
			   (the fixnum
			     (aref
			      (the (simple-array (unsigned-byte 8) (*))
				,tvar)
			      (the (integer 0 #xffff) 
				(char-code (the character ,c))))))))
    form))


(defun xml-letter-p (c &optional (table *xml-chars*))
  (not (eql 0 (logand xml-letter (elt table (char-code c))))))
(define-compiler-macro xml-letter-p (&whole form c &optional (tvar '*xml-chars*))
  (if (and (symbolp c) (symbolp tvar))
      `(not (eql 0 (logand xml-letter
			   (the fixnum
			     (aref
			      (the (simple-array (unsigned-byte 8) (*))
				,tvar)
			      (the (integer 0 #xffff) 
				(char-code (the character ,c))))))))
    form))


(defun xml-name-char-p (c &optional (table *xml-chars*))
  (not (eql 0 (logand xml-name-char (elt table (char-code c))))))
(define-compiler-macro xml-name-char-p (&whole form c &optional (tvar '*xml-chars*))
  (if (and (symbolp c) (symbolp tvar))
      `(not (eql 0 (logand xml-name-char
			   (the fixnum
			     (aref
			      (the (simple-array (unsigned-byte 8) (*))
				,tvar)
			      (the (integer 0 #xffff) 
				(char-code (the character ,c))))))))
    form))


(defun xml-name-start-char-p (c &optional (table *xml-chars*))
  (not (eql 0 (logand xml-name-start (elt table (char-code c))))))
(define-compiler-macro xml-name-start-char-p (&whole form 
						     c &optional (tvar '*xml-chars*))
  (if (and (symbolp c) (symbolp tvar))
      `(not (eql 0 (logand xml-name-start
			   (the fixnum
			     (aref
			      (the (simple-array (unsigned-byte 8) (*))
				,tvar)
			      (the (integer 0 #xffff) 
				(char-code (the character ,c))))))))
    form))



;;#+debug
(defun verify-table (&aux v c b one two all (r (list 0 0 0 0 0 0 0 0 0 0 0 0)))
  (labels ((count-bits (x)
		       (if (zerop x)
			   0
			 (+ (logand 1 x) (count-bits (ash x -1)))))
	   (same-bool (test char p q)
		      (or (and p q) (and (not p) (not q))
			  (error "Not same-bool for ~S ~S" test char))))
    (dotimes (i #x10000)
      (when (eql #xff (logand i #xff))
	(format t ";")
	(when (eql #x3fff (logand i #x3fff))
	  (format t "~%")))
      (setf c (code-char i))
      (setf v (elt *xml-chars* i))

      (same-bool 'xml-char-p c (xml-char-p c) (xml-char-q c))
      (same-bool 'xml-space-p c (xml-space-p c) (xml-space-q c))
      (same-bool 'xml-digit-p c (xml-digit-p c) (xml-digit-q c))
      (same-bool 'xml-base-char-p c (xml-base-char-p c) (xml-base-char-q c))
      (same-bool 'xml-ideographic-p c (xml-ideographic-p c) (xml-ideographic-q c))
      (same-bool 'xml-combining-char-p c 
		 (xml-combining-char-p c) (xml-combining-char-q c))
      (same-bool 'xml-extender-p c (xml-extender-p c) (xml-extender-q c))
      (same-bool 'xml-name-char-p c (xml-name-char-p c) (xml-name-char-q c))
      (same-bool 'xml-name-start-char-p c 
		 (xml-name-start-char-p c) (xml-name-start-char-q c))
      (same-bool 'xml-letter-p c (xml-letter-p c) (xml-letter-q c))

      (incf (elt r (setf b (count-bits v))))
      (case b
	(1 (pushnew v one))
	(2 (pushnew v two)))
      (pushnew v all)
      )
    (setf all (sort all #'<))

    (format t "~&~%~{ ~9B~%~}" all)

    (values r one two all )))




