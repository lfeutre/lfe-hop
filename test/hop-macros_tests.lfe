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

(defrecord entity
  name
  location
  (cash-total 0)
  (cash-owed 0))

(defun state ()
  (let* ((bob (make-entity name '"Bob" location '"home" cash-total 10))
         (state (store (entity-name bob) bob (new-dict)))
         (taxi (make-entity name '"Taxi" location '"downtown")))
    (store (entity-name taxi) taxi state)))

(defun prob-set-1 ()
  (make-problemset
    problems (list
               (make-problem
                 domain (make-domain
                          operators (ops-2)
                          methods (meths-2))
                 state (state))
               (make-problem
                 domain (make-domain
                          operators (ops-4)
                          methods (meths-4))
                 state (state)))))

;; Start of actual tests

(deftest problemset
  (let ((ps1 (make-problemset problems '()))
        (ps2 (make-problemset problems '(a b c))))
    (is-equal '() (problemset-problems ps1))
    (is-equal '(a b c) (problemset-problems ps2))))

(deftest problem
  (let ((p1 (make-problem domain '"my domain" state '"problem state"))
        (p2 (make-problem domain '"domain 2"
                          facts '"some facts"
                          state '"problem state data"
                          goals '"some goals")))
    (is-equal '"my domain" (problem-domain p1))
    (is-equal '() (problem-facts p1))
    (is-equal '"problem state" (problem-state p1))
    (is-equal '() (problem-goals p1))
    (is-equal '"domain 2" (problem-domain p2))
    (is-equal '"some facts" (problem-facts p2))
    (is-equal '"problem state data" (problem-state p2))
    (is-equal '"some goals" (problem-goals p2))))

(deftest domain
  (let ((d1 (make-domain operators '"domain operators"))
        (d2 (make-domain operators '"domain 2 operators"
                         methods '"some methods"
                         axioms '"domain axioms")))
    (is-equal '"domain operators" (domain-operators d1))
    (is-equal '"domain 2 operators" (domain-operators d2))
    (is-equal '"some methods" (domain-methods d2))
    (is-equal '"domain axioms" (domain-axioms d2))))

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

(deftest combined
  (let* ((p1 (car (problemset-problems (prob-set-1))))
         (p2 (cadr (problemset-problems (prob-set-1))))
         (d1 (problem-domain p1))
         (d2 (problem-domain p2))
         (s1 (problem-state p1))
         (s2 (problem-state p2))
         (o1 (domain-operators d1))
         (o2 (domain-operators d2))
         (m1 (domain-methods d1))
         (m2 (domain-methods d2)))
    (is-equal '("Taxi" "Bob") (keys s1))
    (is-equal '("Taxi" "Bob") (keys s2))
    (is-equal '("Taxi" "Bob") (keys s2))
    (is-equal '(walk run) (keys o1))
    (is-equal '(travel) (keys m1))
    (is-equal '(walk run) (keys o2))
    (is-equal '(travel) (keys m2))))


