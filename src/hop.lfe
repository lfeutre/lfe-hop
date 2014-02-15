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
    (rename lists ((member 2) in?))))

(include-lib "include/hop-macros.lfe")

(defun car-bool
  ((tasks) (when (== tasks 'false))
   'false)
  ((tasks) (when (> (length tasks) 0))
    (car tasks))
  ((_) 'false))

(defun cdr-bool
  ((tasks) (when (== tasks 'false))
   'false)
  ((tasks) (when (> (length tasks) 0))
    (cdr tasks))
  ((_) 'false))

(defun has-task? (task-key tasks)
  "The argument 'tasks' should either be a a list of operators or a list of
  methods, both of which should be generated using their respective macros."
  (in? task-key (keys tasks)))

(defun search-operators (state tasks operators methods plan task depth)
  (let* ((operator-name (car task))
         (operator (fetch operator-name operators))
         (remaining (cdr task))
         (state (apply operator remaining)))
    (cond
      ((/= state 'false)
       (let* ((tasks (cdr-bool tasks))
              (plan (append plan '(task)))
              (depth (+ depth 1))
              (solution (find-plan
                          state tasks operators methods plan depth)))
         (cond
           ((/= solution 'false) solution)))))))

(defun process-subtasks (state tasks operators methods plan task depth method)
  (let ((subtasks (apply method (append '(state) (cdr task)))))
    (cond
      ((/= subtasks 'false)
       (let* ((tasks (append subtasks (cdr-bool tasks)))
              (depth (+ depth 1))
              (solution (find-plan
                          state tasks operators methods plan depth)))
         (cond
           ((/= solution 'false) solution)))))))

(defun search-methods (state tasks operators methods plan task depth)
  (let* ((method-name (car task))
         (relevant-methods (fetch method-name methods)))
    (map (lambda (method)
           (process-subtasks state tasks operators methods plan task depth
                             method))
         relevant-methods)))

(defun find-plan (state tasks operators methods)
  (let ((plan ()))
    (find-plan state tasks operators methods plan)))

(defun find-plan (state tasks operators methods plan)
  (let ((depth 0))
    (find-plan state tasks operators methods plan depth)))

(defun find-plan (state tasks operators methods plan depth)
  ""
  (let* ((task (car-bool tasks))
         (task-key (cdr-bool task)))
    (cond
      ((== tasks '())
        plan)
      ((has-task? task-key operators)
        (search-operators state tasks operators methods plan task depth))
      ((has-task? task-key methods)
        (search-methods state tasks operators methods plan task depth))
      ('true 'false))))

