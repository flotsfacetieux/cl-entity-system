(in-package #:cl-entity-system)

;; Entity Component Sytem base code

(defclass entity ()
  ((id :accessor entity-id
       :initarg :id)
   (group :accessor entity-group
	  :initarg :group)))

(defclass component () ())

(defclass system () ())

(defgeneric update (system entity-manager)
  (:documentation "Send Update to all entities into the entity manager"))

(defmacro defcomponent (name &rest body)
    `(defclass ,name (component)
       ,@body))

