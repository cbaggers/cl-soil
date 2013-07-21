;;;; cl-soil.asd

(asdf:defsystem #:cl-soil
  :serial t
  :description "Wrapper around Soil (http://www.lonesock.net/soil.html)"
  :author "Baggers"
  :license "Specify license here"
  :components ((:file "package")
               (:file "cl-soil")))

