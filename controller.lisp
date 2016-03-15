(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :ltk))

(defpackage :planets
  (:use :common-lisp :ltk)
  (:export :main))

(load "~/bottega/planets/view.lisp")
(load "~/bottega/planets/model.lisp")

(in-package :planets)

(defun main ()
  (with-ltk ()
    (let ((my-planet-view (make-instance 'planet-view))
	  (my-planets '()))
      (setf (command (add-planet-button my-planet-view)) (lambda () 
							   (setf my-planets (cons (add-planet my-planet-view) my-planets))))
      (setf (command (remove-planet-button my-planet-view)) (lambda () (progn
									 (remove-planet my-planet-view)
									 (setf my-planets (cdr my-planets))))))))
    

