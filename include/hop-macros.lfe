(defmacro plan args
  `(list ,@args))

(defmacro defoperators arg
  "A convenience wrapper for defining HOP (HTN) operators."
  (let ((name (car arg))
        (body (cdr arg)))
    `(defun ,name ()
       (: dict from_list ,@body))))

(defmacro defmethods arg
  "A convenience wrapper for defining HOP (HTN) methods."
  (let ((name (car arg))
        (body (cdr arg)))
    `(defun ,name ()
       (: dict from_list ,@body))))
