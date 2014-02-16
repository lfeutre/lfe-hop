(defmodule hop-util_tests
  (export all)
  (import
    (from dict (store 3)
               (filter 2)
               (map 2)
               (fetch 2))
    (rename dict ((new 0) new-dict)
                 ((fetch_keys 1) keys))
    (from hop-util (car-bool 1)
                   (cdr-bool 1)
                   (has-task? 2)
                   (empty-tasks? 1))))

(include-lib "deps/lfeunit/include/lfeunit-macros.lfe")
(include-lib "include/hop-macros.lfe")

;; Set up data for tests

(defoperators operators
  '(#(walk walk)
    #(call-taxi call-taxi)
    #(ride-taxi ride-taxi)
    #(pay-cash pay-cash)))

(defmethods methods
  '(#(travel (travel-by-foot
              travel-by-taxi))))

;; Start of actual tests

(deftest car-bool
  (is-equal 1 (car-bool '(1 2 3)))
  (is-equal 1 (car-bool '(1)))
  (is-equal 'false (car-bool '()))
  (is-equal 'false (car-bool 'false)))

(deftest cdr-bool
  (is-equal '(2 3) (cdr-bool '(1 2 3)))
  (is-equal '() (cdr-bool '(1)))
  (is-equal 'false (cdr-bool '()))
  (is-equal 'false (cdr-bool 'false)))

(deftest has-task?
  (is-equal 'false (has-task? 'travel (operators)))
  (is-equal 'true (has-task? 'walk (operators)))
  (is-equal 'false (has-task? 'fly (operators)))
  (is-equal 'true (has-task? 'travel (methods)))
  (is-equal 'false (has-task? 'walk (methods)))
  (is-equal 'false (has-task? 'fly (methods))))

(deftest empty-tasks?
  (is (empty-tasks? '()))
  (is (empty-tasks? '(())))
  (is-not (empty-tasks? '((a 1 2))))
  (is-not (empty-tasks? '((a 1 2) (b 3 4)))))
