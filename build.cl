
(in-package :user)

(let ((filenames 
       (list
	"pxml0"
	"pxml1"
	"pxml3"
	"pxml2")))
  (dolist (f filenames)
    (compile-file-if-needed (concatenate 'string f ".cl"))
    (load (concatenate 'string f ".fasl")))
  
  (with-open-file (out "pxml.fasl"
		   :element-type '(unsigned-byte 8)
		   :direction :output
		   :if-exists :supersede 
		   :if-does-not-exist :create)
    (dolist (file filenames)
      (with-open-file (in (concatenate 'string file ".fasl")
		       :element-type '(unsigned-byte 8))
        (format t "~%; ~s" file)
	(let ((buf (make-array 2048 :element-type '(unsigned-byte 8))))
	  (loop as x = (read-sequence buf in)
	      until (= x 0)
	      do (write-sequence buf out :end x)))))))
  
