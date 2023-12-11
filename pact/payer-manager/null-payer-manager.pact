(module null-payer-manager GOVERNANCE
  (implements payer-manager-v1)
  ;-----------------------------------------------------------------------------
  ; Governance
  ;-----------------------------------------------------------------------------
  (defcap GOVERNANCE ()
    (governance.enforce-governance))

  ;Enforce payer implementation
  (defun enforce-payer:bool (account:string)
    true)
)
