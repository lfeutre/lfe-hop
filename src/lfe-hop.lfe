;;;;
;;;;
;;;;
;;;;
(defmodule lfe-hop
  (export all)
  (import
    (from dict (store 3)
               (filter 2)
               (map 2)
               (fetch 2))
    (rename dict ((new 0) new-dict)
                 ((fetch_keys 1) keys)
                 ((from_list 1) tuples->dict))
    (rename lists ((member 2) in?))
    ))


(defun get-first-task (tasks)
  (cond
    ((== tasks ()

(defun search-operators (state tasks operators methods plan task depth)
  (list 'searched-operators))

(defun search-methods (state tasks operators methods plan task depth)
  (list 'searched-methods))

(defun find-plan (state tasks operators methods)
  (let ((plan ())
        (depth 0))
    (find-plan state tasks operators methods plan depth)))

(defun find-plan (state tasks operators methods plan depth)
  (cond
    ((== tasks ())
      plan)
    ((in? (car tasks) operators)
      (search-operators state tasks operators methods plan (car tasks) depth))
    ((in? (car tasks) methods)
      (search-methods state tasks operators methods plan (car tasks) depth))
    ('true 'false)))

