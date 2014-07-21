This is a script for simple Markov text generation.

It requires the cl-ppcre, which can be installed using (ql:quickload 'cl-ppcre).

To use it from the REPL:
    * (load "markov.lisp")
    To load "cl-ppcre":
      Load 1 ASDF system:
        cl-ppcre
    ; Loading "cl-ppcre"
    [package cl-ppcre]................................
    .....................
    T
    * (defparameter *bible* (make-markov "sample_text/king_james.txt"))
    * (generate-random-sentence *bible* :first "Behold")
    
    "Behold my voice: cause of the land of leprosy."
    * (generate-random-sentence *bible* :first "Behold")
    
    "Behold my right hand be destroyed."
    * 