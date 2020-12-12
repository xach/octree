;;;; octree.asd

(asdf:defsystem #:octree
  :author "Zach Beane <xach@xach.com>"
  :license "MIT"
  :description "Create animated, colorful GIFs with Vecto and Skippy"
  :serial t
  :depends-on (#:vectometry
               #:skippy)
  :components ((:file "package")
               (:file "octree")
               (:file "hex-art")
               (:file "stringstack")
               (:file "text-art")))

