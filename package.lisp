;;;; package.lisp

(defpackage #:markov
  (:use #:cl)
  (:export #:make-markov
           #:generate-random-sentence
           #:to-dot))

