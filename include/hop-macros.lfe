(defmacro deftasks arg
  "A convenience wrapper for defining HOP (HTN) tasks.

  Tasks are a list of lists; the sub-lists are lists of values where:
    1) the first entry is a key for a method or an operator, and
    2) the remaining entries are parameters that will be passed to the function
       that is looked up with the key (the first entry)."
  (let ((name (car arg))
        (body (cdr arg)))
    `(defun ,name ()
       (list ,@body))))

(defmacro subtasks args
  "A subtask is a list of lists; the sub-lists are lists of values where:
    1) the first entry is a key for an operator, and
    2) the remaining entries are the parameters that will be passed to the
       function that is looked up with the key from the first entry."
  `(list ,@args))

(defmacro defmethods arg
  "A convenience wrapper for defining HOP (HTN) methods.

  Methods are key-value pairs where the key is an atom representing a task to be
  performed, and the value is a list of functions that represent different ways
  of accomplishing the task represented by the key."
  (let ((name (car arg))
        (body (cdr arg)))
    `(defun ,name ()
       (: dict from_list ,@body))))

(defmacro defoperators arg
  "A convenience wrapper for defining HOP (HTN) operators.

  Operators are key-value pairs where the key is an atom representing a subtask
  to be performed, and the value is a function that would accomplish the
  particular subtask."
  (let ((name (car arg))
        (body (cdr arg)))
    `(defun ,name ()
       (: dict from_list ,@body))))
