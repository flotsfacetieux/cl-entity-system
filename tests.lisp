(in-package #:cl-entity-system-tests)

(defclass flag (component)
  ((id :accessor flag-id
       :initarg :id)))

(defclass direction (component)
  ())

(defclass c1 (component)
  ())

(defclass c2 (component)
  ())

(defclass c3 (component)
  ())

(defclass geometry (component)
  ())

(defclass render-system (system)
  ())

(define-test test-make-entity
  (let ((em (make-instance 'entity-manager)))
    (assert-equal 0 (make-entity em))
    (assert-equal 1 (lowest-unassigned-id em))
    (assert-equal 1 (make-entity em))
    (assert-equal 2 (lowest-unassigned-id em))
    (assert-equal 2 (length (entities em)))))

(define-test test-add-component
  (let* ((em (make-instance 'entity-manager))
	 (entity1 (make-entity em))
	 (dir1 (make-instance 'direction))
	 (geom1 (make-instance 'geometry))
	 (entity2 (make-entity em))
	 (dir2 (make-instance 'direction))
	 (flag2 (make-instance 'flag)))
    
    (add-component em entity1 dir1)
    (add-component em entity1 geom1)
    (add-component em entity2 dir2)
    (add-component em entity2 flag2)
    
    (assert-equal dir1 (entity-component em entity1 'direction))
    (assert-true (entity-component em entity1 'geometry))
    (assert-false (eq dir2 (entity-component em entity1 'direction)))
    (assert-false (entity-component em entity2 'geometry))
    (assert-equal 3 (hash-table-count (cl-es::components em)))))


(define-test test-del-component
  (let* ((em (make-instance 'entity-manager))
	 (entity1 (make-entity em))
	 (dir1 (make-instance 'direction)))
    (add-component em entity1 dir1)
    (assert-equal dir1 (entity-component em entity1 'direction))
    (del-components em entity1 'direction)
    (assert-false (eq dir1 (entity-component em entity1 'direction)))))

(define-test test-remove-entity
  (let* ((em (make-instance 'entity-manager))
	 (entity (make-entity em))
	 (geom (make-instance 'geometry)))
    (add-component em entity geom)
    (assert-equal 1 (length (entities em)))
    (remove-entity em entity)
    (assert-equal 0 (length (entities em)))))

(define-test test-component-entity
  (let* ((em (make-instance 'entity-manager))
	 (entity (make-entity em))
	 (geom (make-instance 'geometry))
	 (flag (make-instance 'flag)))
    (add-component em entity geom)
    (add-component em entity flag)
    
    (assert-equal geom (entity-component em entity 'geometry))
    (assert-equal flag (entity-component em entity 'flag))))

(define-test test-find-entities
  (let* ((em (make-instance 'entity-manager))
	 (entity1 (make-entity em))
	 (dir1 (make-instance 'direction))
	 (geom1 (make-instance 'geometry))
	 (entity2 (make-entity em))
	 (dir2 (make-instance 'direction))
	 (flag2 (make-instance 'flag)))
    
    (add-component em entity1 dir1)
    (add-component em entity1 geom1)
    (add-component em entity2 dir2)
    (add-component em entity2 flag2)
    
    (assert-equal (list entity1 entity2)
		  (find-entities em 'direction))
    (assert-equal (list entity1) (find-entities em 'geometry))
    (assert-equal (list entity2) (find-entities em 'flag))))

(define-test test-find-entities-of
  (let* ((em (make-instance 'entity-manager))
	 (entity1 (make-entity em))
	 (dir1 (make-instance 'direction))
	 (geom1 (make-instance 'geometry))
	 (c11 (make-instance 'c1))
	 (c12 (make-instance 'c2))
	 (entity2 (make-entity em))
	 (dir2 (make-instance 'direction))
	 (flag2 (make-instance 'flag))
	 (c21 (make-instance 'c1))
	 (c22 (make-instance 'c2))
	 (entity3 (make-entity em))
	 (c31 (make-instance 'c1))
	 (c32 (make-instance 'c2))
	 (dir3 (make-instance 'direction))
	 (flag3 (make-instance 'flag)))
    
    (add-component em entity1 dir1)
    (add-component em entity1 geom1)
    (add-component em entity1 c11)
    (add-component em entity1 c12)
    
    (add-component em entity2 dir2)
    (add-component em entity2 flag2)
    (add-component em entity2 c21)
    (add-component em entity2 c22)
    
    (add-component em entity3 dir3)
    (add-component em entity3 flag3)
    (add-component em entity3 c31)
    (add-component em entity3 c32)
    
    (assert-false (find-entities-of em '(direction flag geometry)))
    (assert-equal (sort (list entity2 entity3) #'<)
		  (sort (find-entities-of em '(direction flag c1 c2))
			#'<))
    (assert-equal (sort (list entity1 entity2 entity3) #'<)
		  (sort (find-entities-of em '(direction c1 c2))
			#'<))))

(define-test test-find-components
  (let* ((em (make-instance 'entity-manager))
	 (entity1 (make-entity em))
	 (dir1 (make-instance 'direction))
	 (geom1 (make-instance 'geometry))
	 (entity2 (make-entity em))
	 (dir2 (make-instance 'direction))
	 (flag2 (make-instance 'flag)))
    
    (add-component em entity1 dir1)
    (add-component em entity1 geom1)
    (add-component em entity2 dir2)
    (add-component em entity2 flag2)
    
    (assert-equal (list dir1 dir2)
		  (find-components em 'direction))
    (assert-equal (list geom1) (find-components em 'geometry))
    (assert-equal (list flag2) (find-components em 'flag))))
    

(defun do-tests ()
  (setq *print-failures* t)
  (setq *print-errors* t)
  (run-tests :all :cl-es-tests))

