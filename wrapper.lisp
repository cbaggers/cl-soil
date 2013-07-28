(cl:in-package :cl-soil)

;; [TODO] Should we abstract buffers into a lisp object?
;;        hmm maybe not, I could then write a cepl wrapper (dirt)
;;        to use gl-arrays

;; [TODO] How do we handle multiple flags?
;; [TODO] Flags should not be 0 a default, should if to make it 0

(defun load-ogl-texture (filepath &optional (force-channels 4) 
                                    (reuse-texture-id 0) (flags 0))
  (with-foreign-string (c-filepath filepath)
    (soil-load-ogl-texture c-filepath force-channels reuse-texture-id flags)))


(defun create-ogl-texture (data-pointer width height
                           &optional (channels 4) (reuse-texture-id 0)
                             (flags 0))
  (soil-create-ogl-texture data-pointer width height
                           channels reuse-texture-id flags))

;;-----------------------------------------------------------------

(defun load-ogl-cubemap (x-pos-filepath x-neg-filepath
                         y-pos-filepath y-neg-filepath
                         z-pos-filepath z-neg-filepath
                         &optional (force-channels 4) (reuse-texture-id 0)
                           (flags 0))
  (with-foreign-strings ((xpf x-pos-filepath) (xnf x-neg-filepath)
                         (ypf y-pos-filepath) (ynf y-neg-filepath)
                         (zpf z-pos-filepath) (znf z-neg-filepath))
    (soil-load-ogl-cubemap xpf xnf ypf ynf zpf znf force-channels 
                           reuse-texture-id flags)))

(defun load-ogl-single-cubemap (filepath face-order 
                                &optional (force-channels 4)
                                  (reuse-texture-id 0) (flags 0))
  (let ((order (symbol-name face-order))) 
    (if (and (every #'(lambda (char) (eql 1 (count char order))) "NSEWUD") 
             (eql 6 (length order)))
        (with-foreign-strings ((c-filepath filepath)
                               (c-face-order order))
          (soil-load-ogl-single-cubemap c-filepath c-face-order force-channels 
                                        reuse-texture-id flags))
        (error "CL-SOIL: Face order spec incorrect"))))


;;-----------------------------------------------------------------

;; (with-foreign-object (info-log '%gl:char info-log-length)
;;   )
