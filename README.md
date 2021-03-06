# markov
### _Jeremiah LaRocco <jeremiah_larocco@fastmail.com>_

## About

This is a Common Lisp library for simple Markov text generation.

## Usage

The easiest way to use the library is to clone the repo into Quicklisp's "local project" directory
and use ql:quickload:

```commonlisp
    * (ql:quickload :markov)
    * (defparameter *bible* (markov:make-markov (asdf:system-relative-pathname :markov  "samples/king_james.txt")))
    * (markov:generatve-random-sentence *bible* :first "Behold")
    * (markov:generate-random-sentence *bible* :first "Behold")
    * 
```

For more information, see [project.org](https://github.com/jl2/markov/blob/master/project.org)

## License

Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

The text samples in the samples directory are public domain and come from [[https://www.gutenberg.org/][Project Gutenberg]].

The library itself is released under the ISC license. See [LICENSE](https://github.com/jl2/markov/blob/masterLICENSE) for details.





