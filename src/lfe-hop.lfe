;;
;;
;;
;;
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


(defun first
  ((tasks) (when (>= (length tasks) 1))
    (car tasks))
  ((_)
    'false))

(defun search-operators (state tasks operators methods plan task depth)
  (let ((operator-name (car task)))
    (list 'searched-operators operator-name)))

(defun search-methods (state tasks operators methods plan task depth)
  (let ((method-name (car task)))
    (list 'searched-methods method-name)))

(defun find-plan (state tasks operators methods)
  (let ((plan ())
        (depth 0))
    (find-plan state tasks operators methods plan depth)))

(defun find-plan (state tasks operators methods plan depth)
  ""
  (let* ((task (first tasks))
         (task-key (car task)))
    (cond
      ((== tasks ())
        plan)
      ((in? task-key (keys operators))
        (search-operators state tasks operators methods plan task depth))
      ((in? task-key (keys methods))
        (search-methods state tasks operators methods plan task depth))
      ('true 'false))))

