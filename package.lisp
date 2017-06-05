;;;; package.lisp

(defpackage #:cl-soil
  (:use #:cl #:cffi #:documentation-utils)
  (:export #:load-ogl-texture
           #:load-ogl-texture-from-memory
           #:create-ogl-texture
           #:load-ogl-hdr-texture
           #:load-image
           #:load-image-from-memory
           #:save-image
           #:free-image-data
           #:load-ogl-cubemap
           #:load-ogl-cubemap-from-memory
           #:load-ogl-single-cubemap
           #:load-ogl-single-cubemap-from-memory
           #:create-ogl-single-cubemap
           #:save-screenshot
           #:last-result))

(defpackage #:stb-image
  (:use #:cl #:cffi #:documentation-utils)
  (:nicknames :stbi)
  (:export #:loadf))
