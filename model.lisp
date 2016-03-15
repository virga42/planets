
(in-package :planets)

(defclass planet ()
  ((diameter :accessor diameter :initarg :diameter :initform 100)
   (velocity :accessor velocity :initarg :velocity :initform 100)))
