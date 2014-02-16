(defmodule hop_tests
  (export all)
  (import
    (from dict (store 3)
               (filter 2)
               (map 2)
               (fetch 2))
    (rename dict ((new 0) new-dict)
                 ((fetch_keys 1) keys))
    (from lists (append 1)
                (append 2))
    (from hop-util (car-bool 1)
                   (cdr-bool 1)
                   (has-task? 2)
                   (empty-tasks? 1))))

(include-lib "deps/lfeunit/include/lfeunit-macros.lfe")
(include-lib "include/hop-macros.lfe")

;; Set up data for tests

(defrecord entity
  name
  location
  (cash-total 0)
  (cash-owed 0))

(defun get-min-walking-distance ()
  2)

(defun get-taxi-rate (distance)
  (+ 1.5 (* 0.5 distance)))

(defun get-distance
  (('"home" '"park")
   8)
  (('"park" '"home")
   8)
  (('"downtown" '"home")
   17)
  (('"home" '"downtown")
   17)
  (('"park" '"downtown")
   21)
  (('"downtown" '"park")
   21))

(defun get-location (state name)
  (entity-location (fetch name state)))

(defun move (state agent-name location)
  (let ((mover (fetch agent-name state)))
    (store agent-name
           (set-entity-location mover location)
           state)))

(defun owe (state agent-name amount)
  (let ((payer (fetch agent-name state)))
    (store agent-name
           (set-entity-cash-owed payer amount)
           state)))

(defun update-cash (state agent-name amount)
  (let* ((cash-holder (fetch agent-name state))
         (cash (entity-cash-total cash-holder)))
    (store agent-name
           (set-entity-cash-total cash-holder (+ cash amount))
           state)))

(defun pay-cash (state payer-name payee-name amount)
  (let* ((payer (fetch payer-name state))
         (payer-cash (entity-cash-total payer))
         (payer-owed (entity-cash-owed payer))
         (state (update-cash state payer-name (* -1 amount)))
         (state (owe state payer-name (- payer-owed amount)))
         (payee (fetch payee-name state)))
    (cond
      ((>= payer-cash payer-owed)
        (update-cash state payee-name amount))
      ('true 'false))))

(defun walk (state agent-name location destination)
  (cond
    ((== (get-location state agent-name) location)
      (move state agent-name destination))
    ('true 'false)))

(defun call-taxi (state location)
  (move state '"Taxi" location))

(defun ride-taxi (state agent-name pickup destination)
  (let ((taxi-name '"Taxi"))
    (cond
      ((and
         (== (get-location state taxi-name) pickup)
         (== (get-location state agent-name) pickup))
       (let* ((state (move state taxi-name destination))
              (state (move state agent-name destination))
              (fee (get-taxi-rate (get-distance pickup destination))))
         (owe state agent-name fee)))
      ('true 'false))))

(defun pay-taxi (state agent-name amount)
  (pay-cash state agent-name '"Taxi" amount))

(defun travel-by-foot (state agent-name location destination)
  (cond
    ((=< (get-min-walking-distance) (get-distance location destination))
      (subtasks
        `(walk ,agent-name ,location ,destination)))
    ('true 'false)))

(defun travel-by-taxi (state agent-name pickup destination)
  (let* ((traveler (fetch agent-name state))
         (traveler-cash (entity-cash-total traveler))
         (fee (get-taxi-rate (get-distance pickup destination))))
    (cond
      ((>= traveler-cash fee)
        (subtasks
          `(call-taxi ,pickup)
          `(ride-taxi ,agent-name ,pickup ,destination)
          `(pay-taxi ,agent-name ,fee)))
      ('true 'false))))

;; XXX operators should probably have is own record with "name" and "function"
;; keys
(defoperators operators
  `(#(walk ,#'walk/4)
    #(call-taxi ,#'call-taxi/2)
    #(ride-taxi ,#'ride-taxi/4)
    #(pay-cash ,#'pay-cash/4)))

;; XXX methods should probably have is own record with "name" and "function"
;; keys
(defmethods methods
  `(#(travel (,#'travel-by-foot/4
              ,#'travel-by-taxi/4))))

(defun state-1 ()
  (let* ((bob (make-entity name '"Bob" location '"home" cash-total 10))
         (state (store (entity-name bob) bob (new-dict)))
         (taxi (make-entity name '"Taxi" location '"downtown")))
    (store (entity-name taxi) taxi state)))

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

;; each of the following should have a macro for syntactic sugar
(defun domain-1 ()
  (make-domain
    operators (operators)
    methods (methods)))

(defun prob-1 ()
  (make-problem
    domain (domain-1)
    state (state-1)
    goals (tasks-one-method)))

(defun prob-set-1 ()
  (make-problemset
    problems (list (prob-1))))

;; Start of actual tests

(deftest get-first-task
  (let ((task (car-bool (problem-goals (prob-1)))))
    (is-equal '(travel "Bob" "home" "park") task)
    (is-equal (car (tasks-one-method)) task)
    (is-equal '("Bob" "home" "park") (cdr-bool task))))

(deftest search-operators-false-state
  (is-equal 'false (: hop search-operators
                   (domain-1)
                   'false
                   (tasks-one-operator)
                   '()
                   (car-bool (tasks-one-operator))
                   0)))

(deftest search-operators-state-change
  (is-equal '"home" (get-location (state-1) '"Bob"))
  (let* ((tasks (tasks-one-operator))
         (task (car-bool tasks))
         (operator-name (car task))
         (operator (fetch operator-name (operators)))
         (remaining (cdr task))
         (args (append `(,(state-1)) remaining))
         (new-state (apply operator args))
         )
    (is-not-equal '() tasks)
    (is-equal '(walk "Bob" "home" "park") task)
    (is-equal 'walk operator-name)
    (is-not-equal '() operator)
    (is-equal '("Bob" "home" "park") remaining)
    (is-not-equal '() args)
    (is-equal '"park" (get-location new-state '"Bob"))))

(deftest search-operators-no-solution
  (is-equal 1 1))

(deftest search-operators-empty-tasks
  (is-equal '((walk "Bob" "home" "park"))
            (: hop search-operators
               (domain-1)
               (state-1)
               (tasks-empty)
               '()
               (car (tasks-one-operator))
               0))
  )

(deftest get-subtasks
  ;; this tests some of the logic in process-subtasks
  ;; XXX this logic in process-subtasks should be put in its own function
  (let* ((goals (tasks-one-method))
         (method #'travel-by-taxi/4)
         (method-args (append `(,(state-1)) (cdr-bool (car-bool goals))))
         (subtasks (apply method method-args)))
    (is-equal '("Bob" "home" "park") (cdr-bool (car-bool goals)))
    (is-equal 4 (length method-args))
    (is-equal '((call-taxi "home")
                (ride-taxi "Bob" "home" "park")
                (pay-taxi "Bob" 5.5)) subtasks)))

(deftest check-expanded-goals
  ;; this tests some of the logic in process-subtasks
  ;; XXX this logic in process-subtasks should be put in its own function
  ;; test wth only one goal
  (let* ((goals (tasks-one-method))
         (method #'travel-by-taxi/4)
         (method-args (append `(,(state-1)) (cdr-bool (car-bool goals))))
         (subtasks (apply method method-args))
         (expanded-goals (append subtasks (cdr-bool goals)))
         )
    (is-equal '(travel "Bob" "home" "park") (car-bool goals))
    (is-equal '() (cdr-bool goals))
    (is-equal '((call-taxi "home")
                (ride-taxi "Bob" "home" "park")
                (pay-taxi "Bob" 5.5)) expanded-goals))
  ;; test with multiple goals
  (let* ((goals (tasks-some))
         (method #'travel-by-taxi/4)
         (method-args (append `(,(state-1)) (cdr-bool (car-bool goals))))
         (subtasks (apply method method-args))
         (expanded-goals (append subtasks (cdr-bool goals)))
         )
    (is-equal '(travel "Bob" "home" "park") (car-bool goals))
    (is-equal '((walk "Bob" "park" "home")) (cdr-bool goals))
    (is-equal '((call-taxi "home")
                (ride-taxi "Bob" "home" "park")
                (pay-taxi "Bob" 5.5)
                (walk "Bob" "park" "home")) expanded-goals)))

(deftest process-subtasks-false-state
  (is-equal 1 1))

(deftest process-subtasks-no-solution
  (is-equal 1 1))

(deftest process-subtasks
  (is-equal '()
            (: hop process-subtasks
               (domain-1)
               (state-1)
               (tasks-one-method)
               '()
               (car (tasks-one-method))
               0
               #'travel-by-taxi/4))
  )

(deftest search-methods-empty-tasks
  (is-equal '((walk "Bob" "home" "park"))
            (: hop search-methods
               (domain-1)
               (state-1)
               (tasks-empty)
               '()
               (car (tasks-one-method))
               0)))

(deftest find-plans
  (let ((result (: hop find-plans (prob-set-1))))
    (is-equal '() result)))

(deftest find-plan-empty-tasks
  (is-equal '() (: hop find-plan
                   (domain-1)
                   (state-1)
                   (tasks-empty)))
  (is-equal
    '(the-plan "do this" "then that")
    (: hop find-plan
       (domain-1)
       (state-1)
       (tasks-empty)
       '(the-plan "do this" "then that")
       0)))

(deftest find-plan-no-matching-task-key
  (is-equal 'false (: hop find-plan
                      (domain-1)
                      (state-1)
                      (tasks-no-matching))))

(deftest find-plan-operator-task
  (is-equal '() (: hop find-plan
                   (domain-1)
                   (state-1)
                   (tasks-one-operator))))

(deftest find-plan-method-task
  (is-equal '() (: hop find-plan
                   (domain-1)
                   (state-1)
                   (tasks-one-method))))
