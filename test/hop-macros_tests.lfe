(defmodule hop-macros_tests
  (export all)
  (import
    (from dict (store 3)
               (filter 2)
               (map 2)
               (fetch 2)
               (to_list 1))
    (rename dict ((new 0) new-dict)
                 ((fetch_keys 1) keys))))

(include-lib "deps/lfeunit/include/lfeunit-macros.lfe")
(include-lib "include/hop-macros.lfe")

;; Set up data for tests

(defoperators ops-1
  (list
    (tuple 'walk '"walking function")
    (tuple 'run '"running function")))

(defoperators ops-2
  `(#(walk "walking function")
    #(run "running function")))

(defoperators ops-3
  `(#(walk ,(lambda () 'walking))
    #(run ,(lambda () 'running))))

(defun walk ()
  'walking)

(defun run ()
  'running)

(defoperators ops-4
  `(#(walk ,#'walk/0)
    #(run ,#'run/0)))

(deftasks tasks-empty
  '())

(deftasks tasks-no-matching
  '(stay-at-home "Bob" "home" "home"))

(deftasks tasks-one-method
  '(travel "Bob" "home" "park"))

(deftasks tasks-one-operator
  '(walk "Bob" "home" "park"))

(deftasks tasks-some
  '(travel "Bob" "home" "park")
  '(walk "Bob" "park" "home"))

(defmethods meths-1
  (list
    (tuple 'travel (list '"travel by foot"
                         '"travel by taxi"))))

(defmethods meths-2
  `(#(travel ("travel by foot"
              "travel by taxi"))))

(defmethods meths-3
  `(#(travel (,(lambda () 'walking)
              ,(lambda () 'riding)))))

(defun travel-by-foot ()
  'walking)

(defun travel-by-taxi ()
  'riding)

(defmethods meths-4
  `(#(travel (,#'travel-by-foot/0
              ,#'travel-by-taxi/0))))

;; Start of actual tests

(deftest tasks
  (is-equal '(()) (tasks-empty))
  (is-equal '((stay-at-home "Bob" "home" "home")) (tasks-no-matching))
  (is-equal '((travel "Bob" "home" "park")) (tasks-one-method))
  (is-equal '((walk "Bob" "home" "park")) (tasks-one-operator))
  (is-equal '((travel "Bob" "home" "park")
              (walk "Bob" "park" "home")) (tasks-some)))

(deftest subtasks
  (is-equal '((a 1 2)) (subtasks '(a 1 2)))
  (is-equal '((a 1 2) (b 3 4))
            (subtasks '(a 1 2)
                  '(b 3 4))))

(deftest defmethods
  (is-equal '(travel) (keys (meths-1)))
  (is-equal '(#(travel ("travel by foot"
                        "travel by taxi")))
            (to_list (meths-1)))
  (is-equal '(travel) (keys (meths-2)))
  (is-equal '(#(travel ("travel by foot"
                        "travel by taxi")))
            (to_list (meths-2)))
  (is-equal '(travel) (keys (meths-3)))
  (is-equal 'walking (funcall (car (fetch 'travel (meths-3)))))
  (is-equal '(travel) (keys (meths-4)))
  (is-equal 'riding (funcall (cadr (fetch 'travel (meths-4))))))

(deftest defoperators
  (is-equal '(walk run) (keys (ops-1)))
  (is-equal '(#(walk "walking function")
              #(run "running function"))
            (to_list (ops-1)))
  (is-equal '(walk run) (keys (ops-2)))
  (is-equal '(#(walk "walking function")
              #(run "running function"))
            (to_list (ops-2)))
  (is-equal '(walk run) (keys (ops-3)))
  (is-equal 'walking (funcall (fetch 'walk (ops-3))))
  (is-equal '(walk run) (keys (ops-4)))
  (is-equal 'running (funcall (fetch 'run (ops-4)))))


