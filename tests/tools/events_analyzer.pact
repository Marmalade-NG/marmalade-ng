; Useful module to analyze events emitted by a command in REPL tests
;
; Usage:
; ------
;  (env-events true)
;  ....
;  (functions-under-test  ...)
;  ...
;  (ev-analyzer.store (env-events true))
;
;  At this point, all events are stored in the module and can be analyzed.
;
; To simplify further analysis, the events are not retrieved by their FQN,
; but by their first name: (i.e last part after the last dot)
;  For example, the event (coin.TRANSFER) can be retrieved with TRANSFER
;
; The module assumes that each event has been emitted once. If not, only the first
; generated event of a specific type is handled
;
; Some useful functions:
; ----------------------
;  (format-evs) => return a pretty formatted events list.
;                  Example: (print (ev-analyzerformat-evs))
;
;  (is-present ev) => return true if a specific event has been emitted
;                     Example: (expect "Event my-ns.my-mod.X emitted" true (ev-analyzer.is-present "X"))
;
;  (position ev) => return the position of an event in the list
;                   Example: (expect "Event X emitted before Y" true (< (ev-analyzer.position "X")
;                                                                       (ev-analyzer.position "Y")))
;
;  (params ev) => return the parameters of an emitted event
;                 Example: (expect "Event X emitted with parameters a, b and c" ["a", "b", "c"]
;                                   (ev-analyzer.params "X"))
;
;  (param ev idx) => return the idx parameter of an emitted event
;                    Example: (expect "Event X emitted with third parameter c" "c"
;                                   (ev-analyzer.param "X" 2))
;
(module ev-analyzer G
  (use free.util-lists)
  (use free.util-strings)

  (defcap G() true)

  (defschema temp-storage-sch
    evs:list
  )

  (deftable temp-storage-table:{temp-storage-sch})

  (defun store (evs:list)
    (write temp-storage-table "last" {'evs:evs}))

  (defun get:list ()
    (with-read temp-storage-table "last" {'evs:=x}
      x))

  (defun --extract-name (x)
    (compose (split ".") (last) (at 'name x)))

  (defun get-names ()
    (map (--extract-name) (get)))

  (defun format-evs:string ()
    (join "\n" (map (to-string) (get))))

  (defun is-present:bool (ev-name:string)
    (contains ev-name (get-names)))

  (defun position:integer (ev-name:string)
    (first (search (get-names) ev-name)))

  (defun params:list (ev-name:string)
    (at 'params (first (filter (compose (--extract-name) (= ev-name)) (get)))))

  (defun param (ev-name:string idx:integer)
    (at idx (params ev-name)))
)

(create-table temp-storage-table)
