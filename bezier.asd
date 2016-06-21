;;;; bezier.asd

(asdf:defsystem #:bezier
  :description "Describe bezier here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cepl
               #:temporal-functions
               #:cepl.sdl2
               #:swank
               #:livesupport
               #:cepl.skitter.sdl2
               #:cepl.devil)
  :serial t
  :components ((:file "package")
               (:file "bezier")))

