;;;; dynamo.asd

(asdf:defsystem #:dynamo
  :serial t
  :description "Describe dynamo here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:weft
               #:trivial-utf-8
               #:cl-mtgnet
               #:cl-netstring+)
  :components ((:file "package")
               (:file "dynamo")
               (:file "server")))
