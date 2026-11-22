;;;; All shared utilities, re-exported from one package.
;;;;
;;;; General-purpose utilities come from the CHALLENGE-UTILITIES library.
;;;; Utilities specific to this year's solution  live in their own package under
;;;; utils/.

(uiop:define-package :aoc-22/utils
  (:use :cl)
  (:use-reexport :challenge-utilities)
  (:use-reexport :aoc-22/utils/input)
  (:use-reexport :aoc-22/utils/solutions-helper)
  (:import-from :cl-ppcre)
  (:import-from :alexandria)
  (:import-from :ironclad))
