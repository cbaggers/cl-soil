;;;; cl-soil.asd

(asdf:defsystem #:cl-soil
  :description "A thin binding over libSOIL.so which allows easy loading of images"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "Public Domain"
  :serial t
  :depends-on (#:cffi #:cl-opengl)
  :components ((:file "package")
               (:file "backend")
               (:file "cl-soil")
               (:file "stbi")))
