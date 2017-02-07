## cl-entity-system

*cl-entity-system* is a Common Lisp library that implements the architectural pattern [Entity-Component-System][ECS].

### Platform

Only tested under LINUX with SBCL and [Quicklisp][QL].

### Set-up

You'll need to install :
- [Steel Bank Common Lisp][2]
- [the Quicklisp package tool][3]

Clone the repository to your local-projects quicklisp directory.
```
cd ~/quicklisp/local-projects
git clone https://github.com/flotfacetieux/cl-entity-system
```
Load using [Quicklisp][QL] : `(ql:quickload :cl-entity-system)`.

### Exemple of use

```lisp
(defparameter *em* (make-instance 'entity-manager))

(defclass move (component)
  ((speed :accessor move-speed
          :initarg :speed)
   (action :accessor move-action
          :initarg :action)))

(defclass move-system (system) ())

(defmethod update ((system move-system) entity-manager)
  (dolist (component (find-components entity-manager 'move))
      (funcall (move-action component) entity-manager (move-speed component)))	
```

[ECS]: https://en.wikipedia.org/wiki/Entity%E2%80%93component%E2%80%93system "Entity Component System"
[QL]: https://www.quicklisp.org/ "Quicklisp"
