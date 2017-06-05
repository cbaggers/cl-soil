(in-package #:cl-soil)

;;-----------------------------------------------------------------
;; Helpers

(defmacro with-foreign-filepath ((c-str-var filepath &key directory-p)
                                 &body body)
  (let ((path (gensym "path")))
    `(let* ((,path ,filepath)
            (,path (uiop:native-namestring
                    (if ,directory-p
                        (uiop:ensure-directory-pathname ,path)
                        ,path))))
       (with-foreign-string (,c-str-var ,path)
         ,@body))))

(defmacro with-foreign-filepaths (var-path-dir-tripples &body body)
  (reduce (lambda (acc x) ` (with-foreign-filepath ,acc ,x))
          var-path-dir-tripples
          :initial-value `(progn ,@body)
          :from-end t))

(defmacro with-zero-being-an-error (func-name &body body)
  (let ((result (gensym "result")))
    `(let ((,result (progn ,@body)))
       (if (eql 0 ,result)
           (error ,(format nil "CL-Soil: ~a returned a 0 signalling a failure~~%Last Result was: ~~a" func-name)
                  (last-result))
           ,result))))

(defun handle-tex-flags (flags)
  (reduce (lambda (a x)
            (logior a (cffi:foreign-enum-value 'ogl-texture-flags x)))
          (uiop:ensure-list flags)
          :initial-value 0))

;;-----------------------------------------------------------------

(defun load-ogl-texture (filepath &optional (force-channels :rgba)
                                    (reuse-texture-id 0) flags)
  (with-foreign-filepath (c-filepath filepath)
    (with-zero-being-an-error "load-ogl-texture"
      (soil-load-ogl-texture c-filepath force-channels reuse-texture-id
                             (handle-tex-flags flags)))))

(defun load-ogl-texture-from-memory (data-pointer data-length
                                     &optional (force-channels :rgba)
                                       (reuse-texture-id 0) flags)
  (with-zero-being-an-error "load-ogl-texture-from-memory"
    (soil-load-ogl-texture-from-memory data-pointer data-length force-channels
                                       reuse-texture-id
                                       (handle-tex-flags flags))))

(defun create-ogl-texture (data-pointer width height
                           &optional (channels 4) (reuse-texture-id 0)
                             flags)
  (with-zero-being-an-error "create-ogl-texture"
    (soil-create-ogl-texture data-pointer width height channels reuse-texture-id
                             (handle-tex-flags flags))))

(defun load-ogl-hdr-texture (filepath
                             &optional fake-hdr-format (rescale-to-max 0)
                               (reuse-texture-id 0) flags)
  (with-foreign-filepath (c-filepath filepath)
    (with-zero-being-an-error "load-ogl-hdr-texture"
      (soil-load-ogl-hdr-texture
       c-filepath fake-hdr-format rescale-to-max reuse-texture-id
       (handle-tex-flags flags)))))

;;-----------------------------------------------------------------

;; [TODO] channels will be to be turned back to a keyword/s
;; [TODO] error detection
(defun load-image (filepath &optional (force-channels :rgba))
  (assert (member force-channels '(:auto :l :la :rgb :rgba)))
  (with-foreign-filepath (c-filepath filepath)
    (with-foreign-objects ((c-width :int) (c-height :int) (c-channels :int))
      (let ((result-pointer (soil-load-image c-filepath c-width c-height
                                             c-channels force-channels)))
        (if (null-pointer-p result-pointer)
            (error "Could not load image ~a~%Recieved NULL pointer" filepath)
            (let* ((data-comp-len (mem-aref c-channels :int))
                   (img-comp-lens (cdr (assoc force-channels
                                              `((:auto . ,data-comp-len)
                                                (:l . 1)
                                                (:la . 2)
                                                (:rgb . 3)
                                                (:rgba . 4))))))
              (values result-pointer
                      (mem-aref c-width :int)
                      (mem-aref c-height :int)
                      data-comp-len
                      img-comp-lens)))))))

;; [TODO] channels will be to be turned back to a keyword/s
;; [TODO] error detection
(defun load-image-from-memory (data-pointer data-length
                               &optional (force-channels :rgba))
  (with-foreign-objects ((c-width :int) (c-height :int) (c-channels :int))
    (let ((result-pointer (soil-load-image-from-memory
                           data-pointer data-length
                           c-width c-height
                           c-channels force-channels)))
      (let* ((data-comp-len (mem-aref c-channels :int))
             (img-comp-lens (cdr (assoc force-channels
                                        `((:auto . ,data-comp-len)
                                          (:l . 1)
                                          (:la . 2)
                                          (:rgb . 3)
                                          (:rgba . 4))))))
        (values result-pointer
                (mem-aref c-width :int)
                (mem-aref c-height :int)
                data-comp-len
                img-comp-lens)))))

(defun save-image (filepath image-type width height channels data)
  (let ((path (uiop:native-namestring filepath)))
    (with-foreign-filepath (c-filepath path)
      (with-zero-being-an-error "save-image"
        (soil-save-image c-filepath image-type width height channels
                         data)))
    filepath))

(defun free-image-data (data-pointer)
  (soil-free-image-data data-pointer))

;;-----------------------------------------------------------------

(defun load-ogl-cubemap (x-pos-filepath x-neg-filepath
                         y-pos-filepath y-neg-filepath
                         z-pos-filepath z-neg-filepath
                         &optional (force-channels :rgba)
                           (reuse-texture-id 0) flags)
  (with-foreign-filepaths ((xpf x-pos-filepath) (xnf x-neg-filepath)
                           (ypf y-pos-filepath) (ynf y-neg-filepath)
                           (zpf z-pos-filepath) (znf z-neg-filepath))
    (with-zero-being-an-error "load-ogl-cubemap"
      (soil-load-ogl-cubemap xpf xnf ypf ynf zpf znf force-channels
                             reuse-texture-id (handle-tex-flags flags)))))

(defun load-ogl-cubemap-from-memory (x-pos-buffer-pointer x-pos-buffer-length
                                     x-neg-buffer-pointer x-neg-buffer-length
                                     y-pos-buffer-pointer y-pos-buffer-length
                                     y-neg-buffer-pointer y-neg-buffer-length
                                     z-pos-buffer-pointer z-pos-buffer-length
                                     z-neg-buffer-pointer z-neg-buffer-length
                                     &optional (force-channels :rgba)
                                       (reuse-texture-id 0) flags)
  (with-zero-being-an-error "load-ogl-cubemap-from-memory"
    (soil-load-ogl-cubemap-from-memory x-pos-buffer-pointer x-pos-buffer-length
                                       x-neg-buffer-pointer x-neg-buffer-length
                                       y-pos-buffer-pointer y-pos-buffer-length
                                       y-neg-buffer-pointer y-neg-buffer-length
                                       z-pos-buffer-pointer z-pos-buffer-length
                                       z-neg-buffer-pointer z-neg-buffer-length
                                       force-channels reuse-texture-id
                                       (handle-tex-flags flags))))

;; [TODO] format check into func
(defun load-ogl-single-cubemap (filepath face-order
                                &optional (force-channels :rgba)
                                  (reuse-texture-id 0) flags)
  (let ((order (symbol-name face-order)))
    (if (and (every #'(lambda (char) (= (count char order) 1))
                    "NSEWUD")
             (= (length order) 6))
        (with-foreign-filepath (c-filepath filepath)
          (with-foreign-string (c-face-order order)
            (with-zero-being-an-error "load-ogl-single-cubemap"
              (soil-load-ogl-single-cubemap c-filepath c-face-order force-channels
                                            reuse-texture-id
                                            (handle-tex-flags flags)))))
        (error "CL-SOIL: Face order spec incorrect. Recieved ~a"
               face-order))))

(defun load-ogl-single-cubemap-from-memory (data-pointer data-length face-order
                                            &optional (force-channels :rgba)
                                              (reuse-texture-id 0) flags)
  (let ((order (symbol-name face-order)))
    (if (and (every #'(lambda (char) (= (count char order) 1))
                    "NSEWUD")
             (= 6 (length order)))
        (with-foreign-string (c-face-order face-order)
          (with-zero-being-an-error "load-ogl-cubemap-from-memory"
            (soil-load-ogl-single-cubemap-from-memory
             data-pointer data-length c-face-order force-channels
             reuse-texture-id (handle-tex-flags flags))))
        (error "CL-SOIL: Face order spec incorrect. Recieved ~a"
               face-order))))

(defun create-ogl-single-cubemap (data-pointer width height channels face-order
                                  reuse-texture-id flags)
  (let ((order (symbol-name face-order)))
    (if (and (every #'(lambda (char) (= (count char order) 1))
                    "NSEWUD")
             (= 6 (length order)))
        (with-foreign-string (c-face-order face-order)
          (with-zero-being-an-error "create_ogl_single_cubemap"
            (soil-create-ogl-single-cubemap data-pointer
                                            width
                                            height
                                            channels
                                            c-face-order
                                            reuse-texture-id
                                            (handle-tex-flags flags))))
        (error "CL-SOIL: Face order spec incorrect. Recieved ~a"
               face-order))))

;;-----------------------------------------------------------------

;; [TODO] get width and height if nil
(defun save-screenshot (filepath image-type x y width height)
  (with-foreign-filepath (path filepath)
    (with-zero-being-an-error "save-screenshot"
      (soil-save-screenshot path image-type x y width height)))
  filepath)

;;-----------------------------------------------------------------

(defun last-result ()
  (cffi:foreign-string-to-lisp (soil-last-result)))
