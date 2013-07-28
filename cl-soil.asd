;;;; cl-soil.asd

(asdf:defsystem #:cl-soil
  :serial t
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "cl-soil")
               (:file "wrapper")))

