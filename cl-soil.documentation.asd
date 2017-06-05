;;;; cl-soil.documentation.asd

(asdf:defsystem #:cl-soil.documentation
  :description "A thin binding over libSOIL.so which allows easy loading of images"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (#:cl-soil #:staple)
  :components ((:file "package")
               (:file "docs/gen")))
