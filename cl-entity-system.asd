(defsystem "cl-entity-system"
    :name "Component Entity System"
    :version "0.0.1"
    :maintainer "Flot Facetieux"
    :author "Flot Facetieux"
    :licence ""
    :serial t
    :description "Component Entity System."
    :long-description
    ""
    :depends-on (:alexandria :lisp-unit)
    :components ((:file "packages")
		 (:file "base")
		 (:file "entity-manager")
		 (:file "tests")))
