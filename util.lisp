(in-package :multi-test)

(defmacro -> (&body body)
  `(bt:make-thread #'(lambda () ,@body)))

(defun vec2->arr (v2)
  (make-array 2 :element-type '(unsigned-byte 8)
                :initial-contents (list (round (x v2)) (round (y v2)))))

(defun arr->vec2 (arr)
  (vec2 (aref arr 0) (aref arr 1)))

(defmacro append! (place &rest lists)
  `(setf ,place (append ,place ,@lists)))

(defun pack (id &key (v (vec2 0 0)) set-id)
  (make-array 4 :element-type '(unsigned-byte 8)
                :initial-contents (list (if set-id 1 0) id (round (x v)) (round (y v)))))

(defun unpack (data)
  (values (aref data 0)
          (aref data 1)
          (vec2 (aref data 2) (aref data 3))))

(defun keyify (host port)
  (with-output-to-string (str)
    (loop :for n :across host
          :do (princ n str))
    (princ port str)))
