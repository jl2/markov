This is a script for simple Markov text generation.

It requires the cl-ppcre, which can be installed using (ql:quickload 'cl-ppcre).

The general idea is to load a sample text file and create a word->word probability table using the make-markov function.  make-markov will return a markov-table structure that contains hash table and an array of words that can be used at the beginning of sentences.

The markov-table structure can be passed to the generate-random-sentence function to generate a random sentence using the probabilities from the table.  generate-random-sentence takes an optional keyword parameter, :first, which specifies the first word of the sentence.  If not specified, a random word is used.

Larger sample texts seem to work better.  The king james bible seems to work pretty well.

To use it from the REPL:
```commonlisp
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
```
