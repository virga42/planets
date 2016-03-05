


(defwidget control-panel (frame)
    ((value :accessor value)
     (command :accessor command :initform (lambda () )))
  ()
(let ((chord-queue-length-title (make-instance 'text 
					       :text "Number of chords"
					       :master self))
      (chord-queue-slider (make-instance 'scale 
			:orientation :horizontal
			:from 0
			:to 100
			:master self))
      (chord-queue-slider-label (make-instance 'label :master self)))
  (setf (command slider) (lambda (e) (progn
				  (setf (text slider-label) e)
				  (setf (value self) e)
				  (funcall (command self) e))))
  (pack slider)
  (pack slider-label)))



(defun main ()
  (with-ltk ()
    (let ((c (make-instance 'canvas :height 600 :width 600))
	  (panel (make-instance 'control-panel)))
      (setf (command panel) (lambda (e) (format t "~a~%" (value panel))))
      (pack c)
      (pack panel))))
