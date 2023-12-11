(module null-payer-manager GOVERNANCE
  (implements payer-manager-v1)
  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defconst ADMIN-KEYSET:string (read-string "admin_keyset"))
  (defcap GOVERNANCE ()
    (enforce-keyset ADMIN-KEYSET))

  ;Enforce payer implementation
  (defun enforce-payer:bool (account:string)
    true)
)
