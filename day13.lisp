(uiop:define-package :aoc-22/day13
  (:use :cl :aoc-22/utils)
  (:import-from :cl-ppcre #:split)
  (:documentation "--- Day 13: Distress Signal ---"))

(in-package :aoc-22/day13)

(defun parse-packet (line)
  (let ((lisp-str (substitute #\Space #\,
                              (substitute #\) #\]
                                          (substitute #\( #\[
                                                      line)))))
    (read-from-string lisp-str)))

(defun parse-packets (line)
  (loop for pair-str in (split "\\n\\n" line)
        for pair  = (mapcar #'parse-packet (split "\\n" pair-str))
        collect pair))

(defmethod compare ((a list) (b list))
  (cond ((and (null a) (null b)) 0)
        ((null a) -1)
        ((null b) 1)
        (t (nif-compare (first a) (first b)
                        1
                        (compare (rest a) (rest b))
                        -1))))

(defmethod compare ((a integer) (b list))
  (compare (list a) b))

(defmethod compare ((a list) (b integer))
  (compare a (list b)))

;; => 4643
(defun part1 (&optional (pairs (parse-packets (read-input))))
  (loop for (left right) in pairs
        for idx from 1
        when (minusp (compare left right))
          sum idx))

;; => 21614
(defun part2 (&optional (pairs (parse-packets (read-input))))
  (let* ((div1 '((2)))
         (div2 '((6)))
         (packets (list* div1 div2 (loop for (l r) in pairs collect l collect r)))
         (sorted (sort packets (lambda (a b) (minusp (compare a b))))))
    (* (1+ (position div1 sorted :test #'equal))
       (1+ (position div2 sorted :test #'equal)))))
