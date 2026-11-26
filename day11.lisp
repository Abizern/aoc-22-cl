(uiop:define-package :aoc-22/day11
  (:use :cl :aoc-22/utils)
  (:import-from :cl-ppcre
                #:all-matches-as-strings
                #:register-groups-bind
                #:split)
  (:documentation "--- Day 11: Monkey in the Middle ---"))

(in-package :aoc-22/day11)

(defstruct monkey
  (items '() :type list)
  (op #'identity :type function)
  (divisor 1 :type integer)
  (if-true 0 :type integer)
  (if-false 0 :type integer)
  (inspections 0 :type integer))

(defun trim-leading (string)
  (string-left-trim " " string))

(defun get-numbers (line)
  (mapcar #'parse-integer (all-matches-as-strings "\\d+" line)))

(defun get-number (line)
  (last1 (get-numbers line)))

(defun get-op (line)
  (register-groups-bind (operator argument)
      ("Operation: new = old (\\*|\\+) (\\S+)" line)
    (let ((fn (if (string= operator "*") #'* #'+)))
      (if (string= argument "old")
          (lambda (old) (funcall fn old old))
          (let ((val (parse-integer argument)))
            (lambda (old) (funcall fn old val)))))))

(defun parse-monkey (line)
  (let* ((parts (mapcar #'trim-leading (split "\\n" line)))
         (items (get-numbers (second parts)))
         (op (get-op (third parts)))
         (divisor (get-number (fourth parts)))
         (if-true (get-number (fifth parts)))
         (if-false (get-number (sixth parts))))
    (make-monkey :items items
                 :op op
                 :divisor divisor
                 :if-true if-true
                 :if-false if-false)))

(defun parse-input (line)
  (coerce (mapcar #'parse-monkey (split "\\n\\n" line)) 'vector))

(defun test-fn-1 ()
  (lambda (worry) (floor worry 3)))

(defun test-fn-2 (monkey-vec)
  (let ((m (reduce #'* monkey-vec :key #'monkey-divisor)))
    (lambda (worry) (mod worry m))))

(defun simulate-turn (monkey-vec monkey test-fn)
  (dolist (item (monkey-items monkey))
    (incf (monkey-inspections monkey))
    (let* ((worry (funcall test-fn (funcall (monkey-op monkey) item)))
           (target (aref monkey-vec (if (zerop (mod worry (monkey-divisor monkey)))
                                        (monkey-if-true monkey)u
                                        (monkey-if-false monkey)))))
      (setf (monkey-items target)
            (nconc (monkey-items target) (list worry)))))
  (setf (monkey-items monkey) nil))

(defun simulate (monkey-vec rounds test-fn)
  (dotimes (r rounds)
    (loop for monkey across monkey-vec
          do (simulate-turn monkey-vec monkey test-fn)))
  (let ((counts (sort (loop for m across monkey-vec
                            collect (monkey-inspections m))
                      #'>)))
    (* (first counts) (second counts))))

;; => 120056
(defun part1 (&optional (monkey-vec (parse-input (read-input))))
  (simulate monkey-vec 20 (test-fn-1)))

;; => 21816744824
(defun part2 (&optional (monkey-vec (parse-input (read-input))))
  (simulate monkey-vec 10000 (test-fn-2 monkey-vec)))
