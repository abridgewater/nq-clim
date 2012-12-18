;;;
;;; space-requirement-tests.lisp
;;;
;;; Tests for space-requirement behavior.
;;;

(cl:defpackage :game-stuff/space-requirement$tests
  (:use :cl :sb-rt :game-stuff/space-requirement))
(cl:in-package :game-stuff/space-requirement$tests)

;; Make sure that we can create a space-requirement and get values
;; from it.
(deftest value-holder
    (let ((space-req (make-space-requirement
		      :width 1 :max-width 2 :min-width 3
		      :height 4 :max-height 5 :min-height 6)))
      (assert (typep space-req 'space-requirement))
      (assert (= 1 (space-requirement-width space-req)))
      (assert (= 2 (space-requirement-max-width space-req)))
      (assert (= 3 (space-requirement-min-width space-req)))
      (assert (= 4 (space-requirement-height space-req)))
      (assert (= 5 (space-requirement-max-height space-req)))
      (assert (= 6 (space-requirement-min-height space-req)))
      (assert (equal '(1 3 2 4 6 5)
		     (multiple-value-list
		      (space-requirement-components space-req))))
      t)
  t)

;; Make sure that the default values for make-space-requirement are
;; correct.
(deftest (make-space-requirement default-parameters)
    (multiple-value-list
     (space-requirement-components
      (make-space-requirement)))
  (0 0 0 0 0 0))

;; Make sure that combination basically works, and that the values
;; line up correctly.
(deftest (combination *)
    (let ((sr1 (make-space-requirement :width 1 :max-width 2 :min-width 3
				       :height 4 :max-height 5 :min-height 6))
	  (sr2 (make-space-requirement
		:width 10 :max-width 100 :min-width 1000
		:height 10000 :max-height 100000 :min-height 1000000)))
      (multiple-value-list
       (space-requirement-components
	(space-requirement-combine #'* sr1 sr2))))
  (10 3000 200 40000 6000000 500000))

;; Make sure that combination is non-commutative in the correct direction.
(deftest (combination -)
    (let ((sr1 (make-space-requirement :width 1 :max-width 2 :min-width 3
				       :height 4 :max-height 5 :min-height 6))
	  (sr2 (make-space-requirement
		:width 10 :max-width 20 :min-width 30
		:height 40 :max-height 50 :min-height 60)))
      (multiple-value-list
       (space-requirement-components
	(space-requirement-combine #'- sr1 sr2))))
  (-9 -27 -18 -36 -54 -45))

;; Make sure that + (a special case of combination) works.
(deftest space-requirement-+
    (let ((sr1 (make-space-requirement :width 1 :max-width 2 :min-width 3
				       :height 4 :max-height 5 :min-height 6))
	  (sr2 (make-space-requirement :width 10 :max-width 20 :min-width 30
				       :height 40 :max-height 50 :min-height 60)))
      (multiple-value-list
       (space-requirement-components
	(space-requirement-+ sr1 sr2))))
  (11 33 22 44 66 55))

;; Make sure that +* (a "spread" version of +) works.
(deftest space-requirement-+*
    (let ((space-req (make-space-requirement
		      :width 1 :max-width 2 :min-width 3
		      :height 4 :max-height 5 :min-height 6)))
      (multiple-value-list
       (space-requirement-components
	(space-requirement-+* space-req
			      :width 10 :max-width 20 :min-width 30
			      :height 40 :max-height 50 :min-height 60))))
  (11 33 22 44 66 55))

;; Check the default values for +*.
(deftest (space-requirement-+* default-parameters)
    (let ((space-req (make-space-requirement
		      :width 1 :max-width 2 :min-width 3
		      :height 4 :max-height 5 :min-height 6)))
      (multiple-value-list
       (space-requirement-components
	(space-requirement-+* space-req))))
  (1 3 2 4 6 5))

;;; EOF
