(module policy-events-extra-C G
  (implements token-policy-ng-v1)
  (use token-policy-ng-v1 [token-info])
  (use marmalade-ng.policy-extra-policies)

  (defcap G() true)

  (defcap EVENT-C:bool (func:string)
    @event
    true)

  (defcap EVENT-SETTLE-C:bool (func:string)
    @event
    true)

  (defun rank:integer ()
    20)

  (defun enforce-init:bool (token:object{token-info})
    (emit-event (EVENT-C "init"))
  )

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (POLICY-ENFORCE-MINT token policy-events-extra-C))
    (emit-event (EVENT-C "mint"))
  )

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (POLICY-ENFORCE-BURN token policy-events-extra-C))
    (emit-event (EVENT-C "burn"))
  )

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    (require-capability (POLICY-ENFORCE-TRANSFER token policy-events-extra-C))
    (emit-event (EVENT-C "transfer"))
  )

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    (require-capability (POLICY-ENFORCE-OFFER token (pact-id) policy-events-extra-C))
    (emit-event (EVENT-C "sale-offer"))
    false
  )

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    (require-capability (POLICY-ENFORCE-WITHDRAW token (pact-id) policy-events-extra-C))
    (emit-event (EVENT-C "sale-withdraw"))
  )

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    (require-capability (POLICY-ENFORCE-BUY token (pact-id) policy-events-extra-C))
    (emit-event (EVENT-C "sale-buy"))
  )

  (defun enforce-sale-settle:bool (token:object{token-info})
    (require-capability (POLICY-ENFORCE-SETTLE token (pact-id) policy-events-extra-C))
    (emit-event (EVENT-SETTLE-C "sale-settle"))
  )

)
