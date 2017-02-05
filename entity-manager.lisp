(in-package #:cl-entity-system)

(defclass entity-manager ()
  ((entities :accessor entities
	     :initform '()
	     :documentation "list of entities'id ")
   (lowest-unassigned-id :accessor lowest-unassigned-id
			 :initform 0)
   (components :accessor components
	       :initform (make-hash-table)
	       :documentation "Hash-table :
                             - key : Type of component 
                             - value : hash table of component 
                                 - key : id of entity
                                 - value : list of components ")))

(defmethod make-entity ((em entity-manager))
  ;; create an "entity" :
  ;; - store the id of the newest entity into entity manager
  ;; - inc 

  (let ((entity (lowest-unassigned-id em)))
    (pushnew entity (entities em))
    (incf (lowest-unassigned-id em))
    entity))

(defmethod component-type-entities ((em entity-manager) component-type)
  ;; Return the hashtable of entities with components of type component-type
  (gethash component-type (components em) nil))

(defmethod new-component-type ((em entity-manager) component-type)
  ;; add a new empty hash table for components of type "component-type"
  (setf (gethash component-type (components em) nil) (make-hash-table)))

(defun new-component (component-type-entities entity component)
  ;; Add a component in the list of the same type of components for entity
  (push component (gethash entity component-type-entities nil)))

(defmethod add-component ((em entity-manager) entity (component component))
  ;; Add a component for entity
  ;;
  (let* ((component-type (type-of component))
	 (cte (or (component-type-entities em component-type)
		  (new-component-type em component-type))))
    (new-component cte entity component)))

(defmethod del-components ((em entity-manager) entity component-type)
  (remhash entity (component-type-entities em component-type)))

(defmethod entity-components ((em entity-manager) entity component-type)
  ;; return the list of components of type component-type for entity
  (when-let ((components
	      (component-type-entities em component-type)))
    (gethash entity components)))

(defmethod entity-component ((em entity-manager) entity component-type)
  ;; return the first component of type component-type for entity
  (first (entity-components em entity component-type)))

(defmethod remove-entity ((em entity-manager) entity)
  ;; Remove entity, i.e. all of its components from the entity manager
  (loop for component-type being the hash-keys in (components em) using (hash-value v)
     do (remhash entity v))
  (setf (entities em) (remove entity (entities em))))

(defmethod find-entities ((em entity-manager) component-type)
  ;; return a list of entities with components of type component-type
  (when-let ((ct-ent (gethash component-type (components em))))
    (loop for entity being the hash-keys in ct-ent
       collect entity)))

(defmethod find-components ((em entity-manager) component-type)
  ;; return all components of type component-type from entity-manager
  (when-let ((ct-ent (gethash component-type (components em))))
    (loop for entity being the hash-keys in ct-ent
       using (hash-value component)
       append component)))

(defmethod find-entities-of ((em entity-manager) components-types)
  ;; return a list of entities with components of multiple types of component-type
  (let* ((result '())
	 (entities-of-components-types
	  (mapcar #'(lambda (component-type)
		      (find-entities em component-type))
		  components-types))
	 (1-ect (car entities-of-components-types))
	 (cdr-ect (cdr entities-of-components-types)))
    (dolist (entity 1-ect)
      (when (every #'(lambda (ent-list)
		       (find entity ent-list))
		   cdr-ect)
	(push entity result)))
    result))
