
(in-package :planets)

(defwidget planet-view (frame)
  ((canvas :accessor canvas)
   (add-planet-button :accessor add-planet-button :initform nil)
   (remove-planet-button :accessor remove-planet-button :initform nil))
  ()
  (setf canvas (make-instance 'canvas :width 800 :height 600 :master self))
  (setf add-planet-button (make-instance 'button :text "Add planet"))
  (setf remove-planet-button (make-instance 'button :text "Kill planet"))

  (pack canvas)
  (pack add-planet-button)
  (pack remove-planet-button))

(defgeneric add-planet-internal (planet-view))
(defmethod add-planet-internal (planet-view)
  (make-oval (canvas planet-view) 100 (random 100) 200 200))

(defgeneric add-planet (planet-view))
(defmethod add-planet ((p planet-view))
  (add-planet-internal p))
