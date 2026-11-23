(uiop:define-package :aoc-22/day06
  (:use :cl :aoc-22/utils)
  (:documentation "--- Day 6: Tuning Trouble ---"))

(in-package :aoc-22/day06)

(defun unique-chars-p (str)
  (= (length str) (length (remove-duplicates str))))

(defun no-duplicates-at-end-p (str cnt)
  (let ((len (length str)))
    (unique-chars-p (subseq str (- len cnt)))))

(defun find-marker (str cnt)
  (loop for i from cnt to (length str)
        when (no-duplicates-at-end-p (subseq str 0 i) cnt)
          return i))

;; => 1833
(defun part1 (&optional (message (read-input)))
  (find-marker message 4))

;; => 3425
(defun part2 (&optional (message (read-input)))
  (find-marker message 14))

(defparameter *examples* '("bvwbjplbgvbhsrlpgdmjqwftvncz"
                           "nppdvjthqldpwncqszvftbrmjlhg"
                           "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
                           "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))
