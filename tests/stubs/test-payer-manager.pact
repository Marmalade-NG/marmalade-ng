(module test-payer-manager GOVERNANCE
  (implements marmalade-ng.payer-manager-v1)
  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))
  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  (defschema test-sch
    enabled:bool
  )

  (deftable test-table:{test-sch})

  ; Enable a token
  (defun enable (token-id:string enabled:bool)
    (write test-table token-id {'enabled:enabled})
  )

  ;Enforce payer implementation
  (defun enforce-payer:bool (account:string)
    (bind (marmalade-ng.policy-fixed-sale.get-sale (pact-id))
          {'token-id:=token-id}
      (with-read test-table token-id {'enabled:=enabled}
        (enforce enabled "Paying this token is not allowed")))
  )
)
