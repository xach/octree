;;;; octree.asd

(asdf:defsystem #:octree
  :serial t
  :depends-on (#:vectometry
               #:skippy)
  :components ((:file "package")
               (:file "octree")))

