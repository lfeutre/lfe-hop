(defmodule simple-travel
  (export all)
  (import
    (rename orddict ((new 0) new-dict)
                    ((store 3) store-dict)
                    ((filter 2) filter-dict)
                    ((map 2) map-dict)
                    ((fetch 2) fetch-dict))))


(defrecord entity
  name
  location
  cash-total
  (cash-owed 0))

(defun get-taxi-rate (distance)
  (+ 1.5 (* 0.5 distance)))

(defun get-distance
  (('"home" '"park")
   8)
  (('"downtown" '"home")
   17)
  (('"park" '"downtown")
   21))

(defun get-location (state name)
  (entity-location (fetch-dict name state)))

(defun move (state name location)
  (let ((mover (fetch-dict name state)))
    (store-dict name
                (set-entity-location mover location)
                state)))

(defun owe (state name amount)
  (let ((payer (fetch-dict name state)))
    (store-dict name
                (set-entity-cash-owed payer amount)
                state)))

(defun update-cash (state name amount)
  (let* ((cash-holder (fetch-dict name state))
         (cash (entity-cash-total cash-holder)))
    (store-dict name
                (set-entity-cash-total cash-holder (+ cash amount))
                state)))

(defun pay-cash (state payer-name payee-name amount)
  (let* ((payer (fetch-dict payer-name state))
         (payer-cash (entity-cash-total payer))
         (payer-owed (entity-cash-owed payer))
         (state (update-cash state payer (* -1 amount)))
         (state (owe state payer-name (- payer-owed amount)))
         (payee (fetch-dict payee-name state)))
    (update-cash state payee amount)))

(defun walk (state name location destination)
  (cond
    ((== (get-location state name) location)
      (move state name destination))
    ('true 'false)))

(defun call-taxi (state location)
  (move state '"Taxi" location))

(defun ride-taxi (state rider-name pickup destination)
  (let ((taxi-name '"Taxi"))
    (cond
      ((and
         (== (get-location state taxi-name) pickup)
         (== (get-location state rider-name) pickup))
       (let* ((state (move state taxi-name destination))
              (state (move state rider-name destination))
              (fee (get-taxi-rate (get-distance pickup destination))))
         (owe state rider-name fee)))
      ('true 'false))))

(defun pay-taxi (state rider-name amount)
  (pay-cash state rider-name '"Taxi" amount))

; XXX implement travel-by-foot

; XXX implement travel-by-taxi

(defun run ()
  (let* ((operators 'xxx)
         (methods 'yyy)
         (bob (make-entity name '"Bob" location '"home" cash-total 10))
         (state (store-dict (entity-name bob) bob (new-dict)))
         (taxi (make-entity name '"Taxi" location '"downtown"))
         (state (store-dict (entity-name taxi) taxi state))
         )
    (: lfe-hop plan
       state
       '(travel bob '"home" '"park")
       operators
       methods)))
