;;;; markov.lisp

(in-package #:markov)

;; A markov-table is a table of probabilities and an array of words that can start a sentence.
(defstruct markov-table
  (mtable (make-hash-table) :type hash-table)
  (first-words (make-array 0) :type array))

(defun get-words (str)
  (loop
     for idx below (length str)
     for word-start = 0 then word-start
     for cc = (aref str idx) then (aref str idx)
     for old-in-word = nil then in-word
     for in-word = (not (sb-unicode:whitespace-p cc)) then (not (sb-unicode:whitespace-p cc))
     for words = nil then words
       
     when (and in-word (not old-in-word))
     do (setf word-start idx)

     when (and (not in-word) old-in-word)
     do (push (subseq str word-start idx) words)

     finally (when in-word (push (subseq str word-start idx) words)) (return (nreverse words))))


(defun create-markov-table (fname)
  (let ((mtable (make-hash-table :test 'equal))
        (previous nil))
    (with-open-file (stream fname)
      (loop for line = (read-line stream nil)
         while line do
           
           (dolist (curword (get-words line))
             (string-downcase curword)
             ;; Check for previous word
             (when previous
               ;; Get the hash table of transitions for previous
               ;; old-htab = mtable[previous]
               (let ((old-htab (gethash previous mtable)))
                 (if old-htab
                     ;; It exists, so check if curword exists
                     ;; If it exists get the counts
                     ;; old-cnts = old-htab[curword]
                     (if (gethash curword old-htab)
                         ;; It was previously transitioned to, so increment
                         ;; old-cnts[curword] += 1
                         (incf (gethash curword (gethash previous mtable)))
                         (setf (gethash curword (gethash previous mtable)) 1))
                     (let ((new-trans (make-hash-table :test 'equal)))
                       (setf (gethash curword new-trans) 1)
                       (setf (gethash previous mtable) new-trans)))))
             (setf previous curword))))
    mtable))

(defun count-total (htab)
  (reduce #'+ (loop for key being the hash-values of htab collect key)))

(defun compute-probabilities (table)
  (let ((sum (count-total table)))
  (loop for word being the hash-keys of table
      using (hash-value count)
      do (setf (gethash word table) (/ count sum 1.0d0)))))

(defun create-probabilities (mtable)
  (loop for initial being the hash-keys of mtable
        using (hash-value goesto)
        do (compute-probabilities goesto))
  mtable)

(defun pick-random-word (mtable)
  (let ((rs (random 1.0d0))
    (cumulative 0.0d0)
    (last-val nil))
  (loop for word being the hash-keys of mtable
      using (hash-value prob)
      do
      (setf cumulative (+ cumulative prob))
      (if (>= cumulative rs)
        (progn 
        (return-from pick-random-word word)))
      (setf last-val word))
  last-val))

(defun ends-with-end-sentence-punctuation-p (str)
  (find (aref str (1- (length str))) "!?."))

(defun ends-with-any-punctuation-p (str)
  (find (aref str (1- (length str))) "!?.;,:"))


(defun good-first-words (mtable)
  (let ((rval nil))
  (loop for word being the hash-keys of mtable
      do
      (if (and (> (length word) 0)
           (upper-case-p (aref word 0))
           (not (ends-with-any-punctuation-p word)))
        (push word rval)))
  (make-array (length rval) :initial-contents rval)))

(defun make-markov (fname)
  (let* ((htab (create-probabilities (create-markov-table fname)))
     (fwords (good-first-words htab))
     (rval (make-markov-table :mtable htab :first-words fwords)))
  rval))

(defun make-markov-no-regex (fname)
  (let* ((htab (create-probabilities (create-markov-table-no-regex fname)))
     (fwords (good-first-words htab))
     (rval (make-markov-table :mtable htab :first-words fwords)))
    rval))

(defun generate-random-sentence (mtab &key (first nil))
  (let* ((mtable (markov-table-mtable mtab))
    (fwords (markov-table-first-words mtab))
    (rval (list
         (if (and first (find first fwords :test 'equal))
           first
         (aref fwords (random (length fwords)))))))
  (do  ((no-trans nil))
    ((or (ends-with-end-sentence-punctuation-p (car rval)) no-trans))

    (let ((next-table (gethash (car rval) mtable)))
      (if next-table
        (push (pick-random-word next-table) rval)
      (progn 
        (setf no-trans t)
        (setf (car rval) (concatenate 'string (car rval) "."))))))
  (format nil "~{~A~^ ~}" (nreverse rval))))

(defun to-dot (mtab)
  (format t "Creating .dot file from Markov table.~%"))

