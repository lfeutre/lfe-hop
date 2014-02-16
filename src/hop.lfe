(defmodule hop
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
    (rename lists ((member 2) in?))
    (from hop-util (car-bool 1)
                   (cdr-bool 1)
                   (has-task? 2)
                   (empty-tasks? 1))))

(include-lib "include/hop-macros.lfe")

(defun apply-operator (domain state task)
  "Used by the function search-operators, this function returns the new state
  after:
   * getting the operator for the given task
   * building the args for the operator (from the given task), and
   * applying the args to the operator."
  ;; XXX are the conditions under which this should return false?
  ;; if so, what are they?
  (let* ((operator-name (car task))
         (operators (domain-operators domain))
         (operator (fetch operator-name operators))
         (args (append `(,state) (cdr task))))
    (apply operator args)))

(defun update-plan (partial-plan successful-task)
  (append partial-plan `(,successful-task)))

(defun increment-depth (depth)
  (+ depth 1))

(defun search-operators (domain state goals partial-plan task depth)
  (let ((new-state (apply-operator domain state task)))
    (cond
      ((== new-state 'false)
       'false)
      ('true
        (let* ((remaining-goals (cdr-bool goals))
               (new-plan (update-plan partial-plan task))
               (new-depth (increment-depth depth))
               (solution (find-plan
                           domain new-state remaining-goals new-plan
                           new-depth)))
          (cond
           ((== solution 'false)
            'false)
           ('true solution)))))))

(defun process-subtasks (domain state goals partial-plan task depth method)
  "The argument 'method' is a function associated with a method key. Such
  functions are defined to return a list of subtasks. These subtasks are lists
  of lists where each sub-list is a list whose first element is a key and
  subsequent elements are arguments.

  The key in these sublists are operators. They are looked up to get the
  corresponding function. The sub-lists' remaining elements are later applied
  to the looked-up function to perform a given action."
  (let* ((method-args (append `(,state) (cdr-bool (car-bool task))))
         (subtasks (apply method method-args)))
    (cond
      ((== subtasks 'false)
        'false)
      ('true
        (let* ((expanded-goals (append subtasks (cdr-bool goals)))
               (depth (+ depth 1))
               (solution (find-plan
                           domain state expanded-goals partial-plan depth)))
          (cond
            ((== solution 'false)
             'false)
            ('true solution)))))))

(defun get-method-functions (domain task)
  (let* ((method-name (car task))
         (methods (domain-methods domain)))
    (fetch method-name methods)))

(defun search-methods (domain state goals partial-plan task depth)
  "Each function that is associated with a given method, when called, returns
  a list of tasks. Each of those tasks is associated with operations (and thus
  other functions).

  This search function iterates through the subtasks."
  (map (lambda (method)
         (process-subtasks domain state goals partial-plan task depth method))
       (get-method-functions domain task)))

(defun find-plans (problem-set)
  (map #'find-plan/1 (problemset-problems problem-set)))

(defun find-plan (problem)
  (let ((domain (problem-domain problem))
        (state (problem-state problem))
        (goals (problem-goals problem)))
    (find-plan domain state goals)))

(defun find-plan (domain state goals)
  (let ((partial-plan '())
        (depth 0))
    (find-plan domain state goals partial-plan depth)))

(defun find-plan (domain state goals partial-plan depth)
  ""
  (let* ((task (car-bool goals))
         (task-key (cdr-bool task)))
    (cond
      ((empty-tasks? goals)
        partial-plan)
      ((has-task? task-key (domain-operators domain))
        (search-operators domain state goals partial-plan task depth))
      ((has-task? task-key (domain-methods domain))
        (search-methods domain state goals partial-plan task depth))
      ('true 'false))))

