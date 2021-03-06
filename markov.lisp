;; markov.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


(in-package #:markov)

;; A markov-table is a table of probabilities and an array of words that can start a sentence.
(defclass markov-table ()
  ((prob-table :initarg :prob-table :type hash-table :accessor prob-table)
   (first-words :initarg :first-words :type array :accessor first-words)))


(declaim (inline create-markov-table
                 ends-with-end-sentence-punctuation-p
                 count-total
                 compute-probabilities
                 create-probabilities))

(defun count-total (htab)
  (loop for key fixnum being the hash-values of htab summing key))

(defun compute-probabilities (table)
  (let ((sum (count-total table)))
    (loop for word being the hash-keys of table
            using (hash-value count)
          do (setf (gethash word table) (/ count sum 1.0d0)))))

(defun create-probabilities (mtable)
  (loop for goesto being the hash-values of mtable
        do (compute-probabilities goesto))
  mtable)

(defun add-file-to-table (filename gt-table)
  (let ((file-content (alexandria:read-file-into-string filename :buffer-size (* 4096 2))))
    (loop for (prev-word  next-word) on (ju:split file-content) by #'cdr do
      (when (not (gethash prev-word gt-table))
        (setf (gethash prev-word gt-table) (make-hash-table :test 'equal)))
      (incf (gethash next-word (gethash prev-word gt-table) 0)))))

(defun create-markov-table (filenames)
  (let ((gt-table (make-hash-table :test 'equal)))
    (dolist (file-name filenames)
      (add-file-to-table file-name gt-table))
    (create-probabilities gt-table)))

(defun pick-random-word (mtable)
  (loop
    for last-word = nil then word
    with prob-value double-float = (random 1.0d0)
    for prob double-float being the hash-values of mtable
      using (hash-key word)
    for cumulative = 0.0d0 then (+ cumulative prob)
    when (>= cumulative prob-value) do
      (return-from pick-random-word word)
    finally
       (return-from pick-random-word last-word)))

(defun ends-with-end-sentence-punctuation-p (str)
  (declare (type simple-string str))
  (find (aref str (1- (length str))) "!?."))

(defun good-first-words (mtable)
  (declare (type hash-table mtable))
  (let ((firsts (loop
                  for word simple-string being the hash-keys of mtable
                  when (and (> (length word) 0)
                            (upper-case-p (aref word 0))
                            (not (ends-with-end-sentence-punctuation-p word)))
                    collect word)))
    (make-array (length firsts) :initial-contents firsts)))

(defun make-markov (file-names)
  (let ((prob-table (create-markov-table (ensure-list file-names))))
    (make-instance 'markov-table
                   :prob-table prob-table
                   :first-words (good-first-words prob-table))))

(defun generate-random-sentence (mtab &key (first (random-elt (first-words mtab))))
  (with-slots (prob-table) mtab
    (format nil "~{~a~^ ~}"
            (loop for next-word = first
                    then (pick-random-word (gethash next-word prob-table))
                  while next-word
                  collecting next-word
                  until (ends-with-end-sentence-punctuation-p next-word)))))
