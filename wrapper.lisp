(cl:in-package :cl-soil)

;; [TODO] Should we abstract buffers into a lisp object?
;;        hmm maybe not, I could then write a cepl wrapper (dirt)
;;        to use gl-arrays

;; [TODO] How do we handle multiple flags?
;; [TODO] Check if allow one or list of flags
;; [TODO] Make sure all cfuncs are using enums
;; [TODO] Force channels enum? - yes!
;; [TODO] Make all enums not have SOIL- at beginning
;; [TODO] give option for failure being error

(defun load-ogl-texture (filepath &optional (force-channels 4) 
                                    (reuse-texture-id 0) flags)
  (with-foreign-string (c-filepath filepath)
    (soil-load-ogl-texture c-filepath force-channels reuse-texture-id 
                           (if flags flags 0))))

;; [TODO] In header but not in cl-soil? &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
;; (defun load-ogl-texture (data-pointer data-length
;;                          &optional (force-channels 4) 
;;                            (reuse-texture-id 0) flags)  
;;   (soil-load-ogl-texture-from-memory c-filepath force-channels reuse-texture-id 
;;                          (if flags flags 0)))

(defun create-ogl-texture (data-pointer width height
                           &optional (channels 4) (reuse-texture-id 0)
                             flags)
  (soil-create-ogl-texture data-pointer width height
                           channels reuse-texture-id 
                           (if flags flags 0)))

(defun load-ogl-hdr-texture (filepath 
                             &optional fake-hdr-format (rescale-to-max 0)
                               (reuse-texture-id 0) flags)
  (with-foreign-string (c-filepath filepath)
    (soil-load-ogl-hdr-texture c-filepath fake-hdr-format rescale-to-max
                               reuse-texture-id flags)))

;;-----------------------------------------------------------------

;; [TODO] channels will be to be turned back to a keyword
(defun load-image (filepath &optional (force-channels 4))
  (with-foreign-string (c-filepath filepath)
    (with-foreign-objects ((c-width :int) (c-height :int) (c-channels :int))
      (let ((result-pointer (soil-load-image c-filepath c-width c-height 
                                           c-channels force-channels)))
          (list result-pointer (mem-aref c-width :int) 
                (mem-aref c-height :int) (mem-aref c-channels :int))))))

(defun load-image-from-memory (data-pointer data-length
                               &optional (force-channels 4))
  (with-foreign-objects ((c-width :int) (c-height :int) (c-channels :int))
    (let ((result-pointer (soil-load-image-from-memory 
                           data-pointer data-length
                           c-width c-height
                           c-channels force-channels)))
      (list result-pointer (mem-aref c-width :int) 
            (mem-aref c-height :int) (mem-aref c-channels :int)))))

;; [TODO] Image-type must be a enum right?
(defun save-image (filepath image-type width height channels
                   data)
  (with-foreign-string (c-filepath filepath)
    (soil-save-image c-filepath image-type width height channels
                     data)))

(defun free-image-data ())

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

(defun load-ogl-cubemap-from-memory (x-pos-buffer-pointer x-pos-buffer-length
                                     x-neg-buffer-pointer x-neg-buffer-length
                                     y-pos-buffer-pointer y-pos-buffer-length
                                     y-neg-buffer-pointer y-neg-buffer-length
                                     z-pos-buffer-pointer z-pos-buffer-length
                                     z-neg-buffer-pointer z-neg-buffer-length
                                     &optional (force-channels 4) 
                                       (reuse-texture-id 0) flags)
  (soil-load-ogl-cubemap-from-memory x-pos-buffer-pointer x-pos-buffer-length
                                     x-neg-buffer-pointer x-neg-buffer-length
                                     y-pos-buffer-pointer y-pos-buffer-length
                                     y-neg-buffer-pointer y-neg-buffer-length
                                     z-pos-buffer-pointer z-pos-buffer-length
                                     z-neg-buffer-pointer z-neg-buffer-length
                                     force-channels reuse-texture-id 
                                     (if flags flags 0)))

;; [TODO] format check into func
(defun load-ogl-single-cubemap (filepath face-order 
                                &optional (force-channels 4)
                                  (reuse-texture-id 0) flags)
  (let ((order (symbol-name face-order))) 
    (if (and (every #'(lambda (char) (eql 1 (count char order))) "NSEWUD") 
             (eql 6 (length order)))
        (with-foreign-strings ((c-filepath filepath)
                               (c-face-order order))
          (soil-load-ogl-single-cubemap c-filepath c-face-order force-channels 
                                        reuse-texture-id flags))
        (error "CL-SOIL: Face order spec incorrect"))))

(defun load-ogl-single-cubemap-from-memory (data-pointer data-length face-order 
                                &optional (force-channels 4)
                                  (reuse-texture-id 0) flags)
  (let ((order (symbol-name face-order))) 
    (if (and (every #'(lambda (char) (eql 1 (count char order))) "NSEWUD") 
             (eql 6 (length order)))        
        (soil-load-ogl-single-cubemap-from-memory 
         data-pointer data-length c-face-order force-channels reuse-texture-id
         (if flags flags 0))
        (error "CL-SOIL: Face order spec incorrect"))))

;;-----------------------------------------------------------------

;; [TODO] get width and height if nil
(defun save-screenshot (filepath image-type x y width height)
  (with-foreign-string (path filepath)
    (soil-save-screenshot path image-type x y width height)))



;; (with-foreign-object (info-log '%gl:char info-log-length)
;;   )
