(defmodule hop-util
  (export all)
  (import
    (from dict (store 3)
               (filter 2)
               (map 2)
               (fetch 2))
    (rename dict ((new 0) new-dict)
                 ((fetch_keys 1) keys))
    (rename lists ((member 2) in?))))

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

(defun empty-tasks? (tasks)
  (cond
    ((or (== tasks '()) (== tasks '(())))
      'true)
    ('true 'false)))
