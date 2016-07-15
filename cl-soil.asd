;;;; cl-soil.asd

(asdf:defsystem #:cl-soil
  :serial t
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "backend")
               (:file "cl-soil")
	       (:file "stbi")))
