;;;; markov.asd

(asdf:defsystem #:markov
  :description "Describe markov here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "Specify license here"
  :depends-on (#:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "markov")))

