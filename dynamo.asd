;;;; dynamo.asd

(asdf:defsystem #:dynamo
  :serial t
  :description "Describe dynamo here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:weft
               #:trivial-utf-8
               #:blackbird
               #:cl-mtgnet)
  :components ((:file "package")
               (:file "dynamo")
               (:file "server")))
