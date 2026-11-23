(uiop:define-package :aoc-22/day05
  (:use :cl :aoc-22/utils)
  (:import-from :alexandria #:hash-table-keys)
  (:import-from :cl-ppcre
                #:split
                #:all-matches-as-strings)
  (:documentation "--- Day 5: Supply Stacks ---"))

(in-package :aoc-22/day05)

(defun split-input (str)
  (split "\\n\\n" str))

(defun parse-stacks (str)
  (let* ((table (make-hash-table :test #'equal))
         (rows (butlast (split "\\n" str)))
         (stacks (transpose
                  (loop for row in rows
                        collect (loop for i from 1 below (length row) by 4
                                      collect (char row i))))))
    (loop for stack in stacks
          for i from 1
          do (setf (gethash i table)
                   (remove-if (lambda (ch)
                                (char= #\Space ch)) stack)))
    table))

(defun parse-moves (str)
  (mapcar (lambda (line)
            (mapcar #'parse-integer
                    (all-matches-as-strings "\\d+" line)))
          (split "\\n" str)))

(defun move-stacks (stacks move &key by-crate)
  (destructuring-bind (cnt source-i dest-i) move
    (let* ((source (gethash source-i stacks))
           (dest (gethash dest-i stacks))
           (moved (loop repeat cnt
                        collect (pop source))))
      (setf (gethash source-i stacks) source
            (gethash dest-i stacks) (append (if by-crate (reverse moved) moved) dest)))))

(defun tops-of-stack (stack)
  (let* ((keys (hash-table-keys stack))
         (tops (loop for i from 1 upto (length keys)
                     collect (car (gethash i stack)))))
    (coerce tops 'string)))


(defun parse-input (str)
  (let* ((parts (split-input str))
         (stacks (parse-stacks (first parts)))
         (moves (parse-moves (second parts))))
    (list stacks moves)))

;; => TLNGFGMFN
(defun part1 (&optional (input (parse-input (read-input))))
  (destructuring-bind (table moves) input
    (loop for move in moves
          do (move-stacks table move :by-crate t))
    (tops-of-stack table)))

;; => FGLQJCMBD
(defun part2 (&optional (input (parse-input (read-input))))
  (destructuring-bind (table moves) input
    (loop for move in moves
          do (move-stacks table move :by-crate nil))
    (tops-of-stack table)))

(defparameter *example*
  (format nil "~{~A~^~%~}"
          '("    [D]    "
            "[N] [C]    "
            "[Z] [M] [P]"
            " 1   2   3 "
            ""
            "move 1 from 2 to 1"
            "move 3 from 1 to 3"
            "move 2 from 2 to 1"
            "move 1 from 1 to 2")))
