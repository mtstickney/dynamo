;;;; dynamo.asd

(asdf:defsystem #:dynamo
  :serial t
  :description "Describe dynamo here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:weft
               #:cl-rpc)
  :components ((:file "package")
               (:file "dynamo")
               (:file "server")))
