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

  (defun extract-name (x)
    (compose (split ".") (last) (at 'name x)))

  (defun get-names ()
    (map (extract-name) (get)))

  (defun format-evs:string ()
    (join "\n" (map (to-string) (get))))

  (defun is-present:bool (ev-name:string)
    (contains ev-name (get-names)))

  (defun position:integer (ev-name:string)
    (first (search (get-names) ev-name)))

  (defun params:list (ev-name:string)
    (at "params" (first (filter (compose (extract-name) (= ev-name)) (get)))))

)

(create-table temp-storage-table)
