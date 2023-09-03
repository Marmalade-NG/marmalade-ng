(module policy-events-A G
  (implements token-policy-ng-v1)
  (use token-policy-ng-v1 [token-info])

  (defcap G() true)

  (defcap EVENT-A:bool (func:string)
    @event
    true)

  (defun rank:integer ()
    20)

  (defun enforce-init:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-INIT token policy-events-A))
    (emit-event (EVENT-A "init"))
  )

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-MINT token policy-events-A))
    (emit-event (EVENT-A "mint"))
  )

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-BURN token policy-events-A))
    (emit-event (EVENT-A "burn"))
  )

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    (require-capability (ledger.POLICY-ENFORCE-TRANSFER token policy-events-A))
    (emit-event (EVENT-A "transfer"))
  )

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    (require-capability (ledger.POLICY-ENFORCE-OFFER token (pact-id) policy-events-A))
    (emit-event (EVENT-A "sale-offer"))
    true
  )

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-WITHDRAW token (pact-id) policy-events-A))
    (emit-event (EVENT-A "sale-withdraw"))
  )

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    (require-capability (ledger.POLICY-ENFORCE-BUY token (pact-id) policy-events-A))
    (emit-event (EVENT-A "sale-buy"))
  )

  (defun enforce-sale-settle:bool (token:object{token-info})
    (require-capability (ledger.POLICY-ENFORCE-SETTLE token (pact-id) policy-events-A))
    (emit-event (EVENT-A "sale-settle"))
  )

)
