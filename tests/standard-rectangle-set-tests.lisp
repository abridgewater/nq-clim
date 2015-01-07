;;;
;;; standard-rectangle-set-tests.lisp
;;;
;;; Tests for regions composed of more than one rectangle.
;;;

(cl:defpackage :nq-clim/tests/standard-rectangle-set-tests
  (:use :cl :sb-rt)
  (:import-from :nq-clim/geometry/standard-rectangle-set
                "DIFFERENCE-OPERATION"
                "DIFFER-Y-SPAN-SETS"
                "INTERSECTION-OPERATION"
                "OPERATE-ON-X-SPAN-SETS"
                "UNION-OPERATION"
                "UNITE-Y-SPAN-SETS"))
(cl:in-package :nq-clim/tests/standard-rectangle-set-tests)


(deftest (unite-x-span-sets same-regions)
    (operate-on-x-span-sets '((14 . 27))
                            '((14 . 27))
                            #'union-operation)
  ((14 . 27)))

(deftest (unite-x-span-sets overlapping-regions set-1-first)
    (operate-on-x-span-sets '((14 . 27))
                            '((20 . 42))
                            #'union-operation)
  ((14 . 42)))

(deftest (unite-x-span-sets overlapping-regions set-2-first)
    (operate-on-x-span-sets '((20 . 42))
                            '((14 . 27))
                            #'union-operation)
  ((14 . 42)))

(deftest (unite-x-span-sets abutting-regions set-1-first)
    (operate-on-x-span-sets '((14 . 20))
                            '((20 . 42))
                            #'union-operation)
  ((14 . 42)))

(deftest (unite-x-span-sets abutting-regions set-2-first)
    (operate-on-x-span-sets '((20 . 42))
                            '((14 . 20))
                            #'union-operation)
  ((14 . 42)))

(deftest (unite-x-span-sets disjoint-regions set-1-first)
    (operate-on-x-span-sets '((14 . 27))
                            '((28 . 42))
                            #'union-operation)
  ((14 . 27) (28 . 42)))

(deftest (unite-x-span-sets disjoint-regions set-2-first)
    (operate-on-x-span-sets '((28 . 42))
                            '((14 . 27))
                            #'union-operation)
  ((14 . 27) (28 . 42)))

(deftest (unite-x-span-sets three-become-one)
    (operate-on-x-span-sets '((14 . 27) (40 . 61))
                            '((20 . 40))
                            #'union-operation)
  ((14 . 61)))

(deftest (unite-x-span-sets set-1-empty)
    (operate-on-x-span-sets nil
                            '((20 . 40))
                            #'union-operation)
  ((20 . 40)))

(deftest (unite-x-span-sets set-2-empty)
    (operate-on-x-span-sets '((20 . 40))
                            nil
                            #'union-operation)
  ((20 . 40)))


(deftest (unite-y-span-sets same-regions)
    (unite-y-span-sets '(((25 . 50) (20 . 30)))
                       '(((25 . 50) (20 . 30))))
  (((25 . 50) (20 . 30))))

(deftest (unite-y-span-sets disjoint-horizontal-regions set-1-first)
    (unite-y-span-sets '(((25 . 50) (20 . 30)))
                       '(((25 . 50) (40 . 50))))
  (((25 . 50) (20 . 30) (40 . 50))))

(deftest (unite-y-span-sets disjoint-horizontal-regions set-2-first)
    (unite-y-span-sets '(((25 . 50) (40 . 50)))
                       '(((25 . 50) (20 . 30))))
  (((25 . 50) (20 . 30) (40 . 50))))

(deftest (unite-y-span-sets abutting-horizontal-regions)
    (unite-y-span-sets '(((25 . 50) (20 . 30)))
                       '(((25 . 50) (30 . 40))))
  (((25 . 50) (20 . 40))))

(deftest (unite-y-span-sets overlapping-horizontal-regions)
    (unite-y-span-sets '(((25 . 50) (20 . 30)))
                       '(((25 . 50) (25 . 35))))
  (((25 . 50) (20 . 35))))

(deftest (unite-y-span-sets enclosed-with-overlap set-1-first)
    (unite-y-span-sets '(((25 . 100) (25 . 100)))
                       '(((50 . 75) (50 . 75))))
  (((25 . 100) (25 . 100))))

(deftest (unite-y-span-sets enclosed-with-overlap set-2-first)
    (unite-y-span-sets '(((50 . 75) (50 . 75)))
                       '(((25 . 100) (25 . 100))))
  (((25 . 100) (25 . 100))))

(deftest (unite-y-span-sets overlapping-vertical-regions set-1-first)
    (unite-y-span-sets '(((25 . 50) (25 . 50)))
                       '(((45 . 70) (25 . 50))))
  (((25 . 70) (25 . 50))))

(deftest (unite-y-span-sets overlapping-vertical-regions set-2-first)
    (unite-y-span-sets '(((45 . 70) (25 . 50)))
                       '(((25 . 50) (25 . 50))))
  (((25 . 70) (25 . 50))))

(deftest (unite-y-span-sets abutting-vertical-regions set-1-first)
    (unite-y-span-sets '(((25 . 50) (25 . 50)))
                       '(((50 . 75) (25 . 50))))
  (((25 . 75) (25 . 50))))

(deftest (unite-y-span-sets abutting-vertical-regions set-2-first)
    (unite-y-span-sets '(((50 . 75) (25 . 50)))
                       '(((25 . 50) (25 . 50))))
  (((25 . 75) (25 . 50))))

(deftest (unite-y-span-sets disjoint-vertical-regions set-1-first)
    (unite-y-span-sets '(((25 . 50) (25 . 50)))
                       '(((74 . 99) (25 . 50))))
  (((25 . 50) (25 . 50))
   ((74 . 99) (25 . 50))))

(deftest (unite-y-span-sets disjoint-vertical-regions set-2-first)
    (unite-y-span-sets '(((74 . 99) (25 . 50)))
                       '(((25 . 50) (25 . 50))))
  (((25 . 50) (25 . 50))
   ((74 . 99) (25 . 50))))

(deftest (unite-y-span-sets overlapping-squares set-1-northwest)
    (unite-y-span-sets '(((25 . 75) (25 . 75)))
                       '(((50 . 100) (50 . 100))))
  (((25 . 50) (25 . 75))
   ((50 . 75) (25 . 100))
   ((75 . 100) (50 . 100))))

(deftest (unite-y-span-sets overlapping-squares set-1-northeast)
    (unite-y-span-sets '(((25 . 75) (50 . 100)))
                       '(((50 . 100) (25 . 75))))
  (((25 . 50) (50 . 100))
   ((50 . 75) (25 . 100))
   ((75 . 100) (25 . 75))))

(deftest (unite-y-span-sets overlapping-squares set-1-southeast)
    (unite-y-span-sets '(((50 . 100) (50 . 100)))
                       '(((25 . 75) (25 . 75))))
  (((25 . 50) (25 . 75))
   ((50 . 75) (25 . 100))
   ((75 . 100) (50 . 100))))

(deftest (unite-y-span-sets overlapping-squares set-1-southwest)
    (unite-y-span-sets '(((50 . 100) (25 . 75)))
                       '(((25 . 75) (50 . 100))))
  (((25 . 50) (50 . 100))
   ((50 . 75) (25 . 100))
   ((75 . 100) (25 . 75))))

(deftest (unite-y-span-sets cross-shape set-1-vertical)
    (unite-y-span-sets '(((25 . 100) (50 . 75)))
                       '(((50 . 75) (25 . 100))))
  (((25 . 50) (50 . 75))
   ((50 . 75) (25 . 100))
   ((75 . 100) (50 . 75))))

(deftest (unite-y-span-sets cross-shape set-2-vertical)
    (unite-y-span-sets '(((50 . 75) (25 . 100)))
                       '(((25 . 100) (50 . 75))))
  (((25 . 50) (50 . 75))
   ((50 . 75) (25 . 100))
   ((75 . 100) (50 . 75))))

(deftest (unite-y-span-sets checkerboard set-1-northwest)
    (unite-y-span-sets '(((25 . 50) (25 . 50))
                         ((50 . 75) (50 . 75)))
                       '(((25 . 50) (50 . 75))
                         ((50 . 75) (25 . 50))))
  (((25 . 75) (25 . 75))))

(deftest (unite-y-span-sets checkerboard set-2-northwest)
    (unite-y-span-sets '(((25 . 50) (50 . 75))
                         ((50 . 75) (25 . 50)))
                       '(((25 . 50) (25 . 50))
                         ((50 . 75) (50 . 75))))
  (((25 . 75) (25 . 75))))

(deftest (unite-y-span-sets cross-shape double-horizontal-with-covered-gaps)
    (unite-y-span-sets '(((25 . 100) (50 . 75)))
                       '(((50 . 60) (25 . 50) (60 . 100))
                         ((60 . 75) (25 . 60) (75 . 100))))
  (((25 . 50) (50 . 75))
   ((50 . 75) (25 . 100))
   ((75 . 100) (50 . 75))))


(deftest (intersect-x-span-sets same-regions)
    (operate-on-x-span-sets '((14 . 27))
                            '((14 . 27))
                            #'intersection-operation)
  ((14 . 27)))

(deftest (intersect-x-span-sets overlapping-regions set-1-first)
    (operate-on-x-span-sets '((14 . 27))
                            '((20 . 42))
                            #'intersection-operation)
  ((20 . 27)))

(deftest (intersect-x-span-sets overlapping-regions set-2-first)
    (operate-on-x-span-sets '((20 . 42))
                            '((14 . 27))
                            #'intersection-operation)
  ((20 . 27)))

(deftest (intersect-x-span-sets abutting-regions set-1-first)
    (operate-on-x-span-sets '((14 . 20))
                            '((20 . 42))
                            #'intersection-operation)
  nil)

(deftest (intersect-x-span-sets abutting-regions set-2-first)
    (operate-on-x-span-sets '((20 . 42))
                            '((14 . 20))
                            #'intersection-operation)
  nil)

(deftest (intersect-x-span-sets disjoint-regions set-1-first)
    (operate-on-x-span-sets '((14 . 27))
                            '((28 . 42))
                            #'intersection-operation)
  nil)

(deftest (intersect-x-span-sets disjoint-regions set-2-first)
    (operate-on-x-span-sets '((28 . 42))
                            '((14 . 27))
                            #'intersection-operation)
  nil)

(deftest (intersect-x-span-sets set-1-empty)
    (operate-on-x-span-sets nil
                            '((20 . 40))
                            #'intersection-operation)
  nil)

(deftest (intersect-x-span-sets set-2-empty)
    (operate-on-x-span-sets '((20 . 40))
                            nil
                            #'intersection-operation)
  nil)

(deftest (intersect-x-span-sets three-become-two)
    (operate-on-x-span-sets '((14 . 27) (40 . 61))
                            '((20 . 45))
                            #'intersection-operation)
  ((20 . 27) (40 . 45)))


(deftest (differ-x-span-sets same-regions)
    (operate-on-x-span-sets '((14 . 27))
                            '((14 . 27))
                            #'difference-operation)
  nil)

(deftest (differ-x-span-sets overlapping-regions set-1-first)
    (operate-on-x-span-sets '((14 . 27))
                            '((20 . 42))
                            #'difference-operation)
  ((14 . 20)))

(deftest (differ-x-span-sets overlapping-regions set-2-first)
    (operate-on-x-span-sets '((20 . 42))
                            '((14 . 27))
                            #'difference-operation)
  ((27 . 42)))

(deftest (differ-x-span-sets abutting-regions set-1-first)
    (operate-on-x-span-sets '((14 . 20))
                            '((20 . 42))
                            #'difference-operation)
  ((14 . 20)))

(deftest (differ-x-span-sets abutting-regions set-2-first)
    (operate-on-x-span-sets '((20 . 42))
                            '((14 . 20))
                            #'difference-operation)
  ((20 . 42)))

(deftest (differ-x-span-sets disjoint-regions set-1-first)
    (operate-on-x-span-sets '((14 . 27))
                            '((28 . 42))
                            #'difference-operation)
  ((14 . 27)))

(deftest (differ-x-span-sets disjoint-regions set-2-first)
    (operate-on-x-span-sets '((28 . 42))
                            '((14 . 27))
                            #'difference-operation)
  ((28 . 42)))

(deftest (differ-x-span-sets set-1-empty)
    (operate-on-x-span-sets nil
                            '((20 . 40))
                            #'difference-operation)
  nil)

(deftest (differ-x-span-sets set-2-empty)
    (operate-on-x-span-sets '((20 . 40))
                            nil
                            #'difference-operation)
  ((20 . 40)))

(deftest (differ-x-span-sets three-become-two)
    (operate-on-x-span-sets '((14 . 27) (40 . 61))
                            '((20 . 45))
                            #'difference-operation)
  ((14 . 20) (45 . 61)))


(deftest (differ-y-span-sets same-regions)
    (differ-y-span-sets '(((25 . 50) (20 . 30)))
                        '(((25 . 50) (20 . 30))))
  nil)

(deftest (differ-y-span-sets disjoint-horizontal-regions set-1-first)
    (differ-y-span-sets '(((25 . 50) (20 . 30)))
                        '(((25 . 50) (40 . 50))))
  (((25 . 50) (20 . 30))))

(deftest (differ-y-span-sets disjoint-horizontal-regions set-2-first)
    (differ-y-span-sets '(((25 . 50) (40 . 50)))
                        '(((25 . 50) (20 . 30))))
  (((25 . 50) (40 . 50))))

(deftest (differ-y-span-sets abutting-horizontal-regions)
    (differ-y-span-sets '(((25 . 50) (20 . 30)))
                        '(((25 . 50) (30 . 40))))
  (((25 . 50) (20 . 30))))

(deftest (differ-y-span-sets overlapping-horizontal-regions)
    (differ-y-span-sets '(((25 . 50) (20 . 30)))
                        '(((25 . 50) (25 . 35))))
  (((25 . 50) (20 . 25))))

(deftest (differ-y-span-sets enclosed-with-overlap set-1-first)
    (differ-y-span-sets '(((25 . 100) (25 . 100)))
                        '(((50 . 75) (50 . 75))))
  (((25 . 50) (25 . 100))
   ((50 . 75) (25 . 50) (75 . 100))
   ((75 . 100) (25 . 100))))

(deftest (differ-y-span-sets enclosed-with-overlap set-2-first)
    (differ-y-span-sets '(((50 . 75) (50 . 75)))
                        '(((25 . 100) (25 . 100))))
  nil)

(deftest (differ-y-span-sets overlapping-vertical-regions set-1-first)
    (differ-y-span-sets '(((25 . 50) (25 . 50)))
                        '(((45 . 70) (25 . 50))))
  (((25 . 45) (25 . 50))))

(deftest (differ-y-span-sets overlapping-vertical-regions set-2-first)
    (differ-y-span-sets '(((45 . 70) (25 . 50)))
                        '(((25 . 50) (25 . 50))))
  (((50 . 70) (25 . 50))))

(deftest (differ-y-span-sets abutting-vertical-regions set-1-first)
    (differ-y-span-sets '(((25 . 50) (25 . 50)))
                        '(((50 . 75) (25 . 50))))
  (((25 . 50) (25 . 50))))

(deftest (differ-y-span-sets abutting-vertical-regions set-2-first)
    (differ-y-span-sets '(((50 . 75) (25 . 50)))
                        '(((25 . 50) (25 . 50))))
  (((50 . 75) (25 . 50))))

(deftest (differ-y-span-sets disjoint-vertical-regions set-1-first)
    (differ-y-span-sets '(((25 . 50) (25 . 50)))
                        '(((74 . 99) (25 . 50))))
  (((25 . 50) (25 . 50))))

(deftest (differ-y-span-sets disjoint-vertical-regions set-2-first)
    (differ-y-span-sets '(((74 . 99) (25 . 50)))
                        '(((25 . 50) (25 . 50))))
  (((74 . 99) (25 . 50))))

(deftest (differ-y-span-sets overlapping-squares set-1-northwest)
    (differ-y-span-sets '(((25 . 75) (25 . 75)))
                        '(((50 . 100) (50 . 100))))
  (((25 . 50) (25 . 75))
   ((50 . 75) (25 . 50))))

(deftest (differ-y-span-sets overlapping-squares set-1-northeast)
    (differ-y-span-sets '(((25 . 75) (50 . 100)))
                        '(((50 . 100) (25 . 75))))
  (((25 . 50) (50 . 100))
   ((50 . 75) (75 . 100))))

(deftest (differ-y-span-sets overlapping-squares set-1-southeast)
    (differ-y-span-sets '(((50 . 100) (50 . 100)))
                        '(((25 . 75) (25 . 75))))
  (((50 . 75) (75 . 100))
   ((75 . 100) (50 . 100))))

(deftest (differ-y-span-sets overlapping-squares set-1-southwest)
    (differ-y-span-sets '(((50 . 100) (25 . 75)))
                        '(((25 . 75) (50 . 100))))
  (((50 . 75) (25 . 50))
   ((75 . 100) (25 . 75))))

(deftest (differ-y-span-sets cross-shape set-1-vertical)
    (differ-y-span-sets '(((25 . 100) (50 . 75)))
                        '(((50 . 75) (25 . 100))))
  (((25 . 50) (50 . 75))
   ((75 . 100) (50 . 75))))

(deftest (differ-y-span-sets cross-shape set-2-vertical)
    (differ-y-span-sets '(((50 . 75) (25 . 100)))
                        '(((25 . 100) (50 . 75))))
  (((50 . 75) (25 . 50) (75 . 100))))

(deftest (differ-y-span-sets checkerboard set-1-northwest)
    (differ-y-span-sets '(((25 . 50) (25 . 50))
                          ((50 . 75) (50 . 75)))
                        '(((25 . 50) (50 . 75))
                          ((50 . 75) (25 . 50))))
  (((25 . 50) (25 . 50))
   ((50 . 75) (50 . 75))))

(deftest (differ-y-span-sets checkerboard set-2-northwest)
    (differ-y-span-sets '(((25 . 50) (50 . 75))
                          ((50 . 75) (25 . 50)))
                        '(((25 . 50) (25 . 50))
                          ((50 . 75) (50 . 75))))
  (((25 . 50) (50 . 75))
   ((50 . 75) (25 . 50))))

(deftest (differ-y-span-sets cross-shape double-horizontal-with-covered-gaps)
    (differ-y-span-sets '(((25 . 100) (50 . 75)))
                        '(((50 . 60) (25 . 50) (60 . 100))
                          ((60 . 75) (25 . 60) (75 . 100))))
  (((25 . 50) (50 . 75))
   ((50 . 60) (50 . 60))
   ((60 . 75) (60 . 75))
   ((75 . 100) (50 . 75))))

;;; EOF
