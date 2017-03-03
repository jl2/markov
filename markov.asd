;;;; markov.asd

(asdf:defsystem #:markov
  :description "Markov text generator."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC"
  :depends-on ()
  :serial t
  :components ((:file "package")
               (:file "markov")))

