(module fake-fungible GOV
  (implements fungible-v2)
  (use fungible-v2 [account-details])

  (defcap GOV ()
    true)

  (defconst OK:string "OK")

  (defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
    @managed amount TRANSFER-mgr
    true)

  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    0.0)

  (defun transfer:string (sender:string receiver:string amount:decimal)
    OK)

  (defun transfer-create:string (sender:string receiver:string receiver-guard:guard amount:decimal)
    OK)

  (defpact transfer-crosschain:string (sender:string receiver:string receiver-guard:guard target-chain:string amount:decimal)
    (step
      OK))

  (defun get-balance:decimal (account:string)
    0.0)

  (defun details:object{account-details} (account:string)
    {'account:"", 'balance:0.0, 'guard:(create-module-guard "")})

  (defun precision:integer ()
    0)

  (defun enforce-unit:bool (amount:decimal)
    true)

  (defun create-account:string (account:string guard:guard)
    OK)

  (defun rotate:string (account:string new-guard:guard)
    OK)
)
