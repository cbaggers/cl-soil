(in-package :cl-soil)

(defun gen-docs ()
  (let ((org.tymoonnext.staple::*extension-file* #p"docs/staple.ext.lisp"))
    (labels ((@ (x) (asdf:system-relative-pathname :cl-soil x)))
      (staple:generate
       :cl-soil
       :packages '(:cl-soil)
       :template (@ #p"docs/template.ctml")
       :out (@ "docs/cl-soil-api.html")
       :if-exists :supersede)))
  :booya)
