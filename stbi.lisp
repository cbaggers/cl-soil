(in-package :stbi)

(cffi:defcfun ("stbi_loadf" %loadf) :pointer
  (filename :pointer)
  (x :pointer)
  (y :pointer)
  (comp :pointer)
  (req-comp :int))

(defun loadf (filepath)
  (with-foreign-string (c-filepath filepath)
    (with-foreign-objects ((width :int)
			   (height :int)
			   (components-per-pixel :int))
      (cl-soil::with-zero-being-an-error "loadf"
        (let ((result (%loadf c-filepath width height components-per-pixel 0)))
          (if (null-pointer-p result)
              (error "null pointer returned from loadf~%Failed to load ~a"
                     filepath)
              (list result
                    (mem-aref width :int)
                    (mem-aref height :int)
                    (mem-aref components-per-pixel :int))))))))
