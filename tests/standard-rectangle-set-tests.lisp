;;;
;;; standard-rectangle-set-tests.lisp
;;;
;;; Tests for regions composed of more than one rectangle.
;;;

(cl:defpackage :nq-clim/tests/standard-rectangle-set-tests
  (:use :cl :sb-rt)
  (:import-from :nq-clim/geometry/standard-rectangle-set
                "UNITE-X-SPAN-SETS"))
(cl:in-package :nq-clim/tests/standard-rectangle-set-tests)


(deftest (unite-x-span-sets same-regions)
    (unite-x-span-sets '((14 . 27))
                     '((14 . 27)))
  ((14 . 27)))

(deftest (unite-x-span-sets overlapping-regions set-1-first)
    (unite-x-span-sets '((14 . 27))
                     '((20 . 42)))
  ((14 . 42)))

(deftest (unite-x-span-sets overlapping-regions set-2-first)
    (unite-x-span-sets '((20 . 42))
                     '((14 . 27)))
  ((14 . 42)))

(deftest (unite-x-span-sets abutting-regions set-1-first)
    (unite-x-span-sets '((14 . 20))
                     '((20 . 42)))
  ((14 . 42)))

(deftest (unite-x-span-sets abutting-regions set-2-first)
    (unite-x-span-sets '((20 . 42))
                     '((14 . 20)))
  ((14 . 42)))

(deftest (unite-x-span-sets disjoint-regions set-1-first)
    (unite-x-span-sets '((14 . 27))
                     '((28 . 42)))
  ((14 . 27) (28 . 42)))

(deftest (unite-x-span-sets disjoint-regions set-2-first)
    (unite-x-span-sets '((28 . 42))
                     '((14 . 27)))
  ((14 . 27) (28 . 42)))

(deftest (unite-x-span-sets three-become-one)
    (unite-x-span-sets '((14 . 27) (40 . 61))
                     '((20 . 40)))
  ((14 . 61)))

;;; EOF
