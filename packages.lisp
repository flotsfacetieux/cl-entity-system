(in-package #:cl-user)

(defpackage #:cl-entity-system
  (:use #:cl :alexandria)
  (:nicknames #:cl-es)
  (:export ;;#:entity
	   ;;#:entity-id
	   ;;#:entity-group
	   #:defcomponent
	   #:component
	   #:system
	   ;;#:system-entity-manager
	   ;;#:dt-accumulator
	   ;;#:dt-max
	   ;;#:update
	   ;; Entity Manager
	   #:entity-manager
	   #:entities
	   ;;#:components-types
	   #:lowest-unassigned-id
	   #:make-entity
	   #:add-component
	   #:del-component
	   #:del-components
	   #:entity-component
	   #:entity-components
	   #:remove-entity
	   #:find-entities
	   #:find-entities-of
	   #:find-components
	   ;;#:find-entities-single-type
	   ;;#:find-entities-by-group
	   ;;#:find-entities-multiple-types
	   ))

(defpackage #:cl-entity-system-tests
  (:use #:cl #:lisp-unit #:cl-es)
  (:nicknames #:cl-es-tests)
  (:export #:do-tests))
