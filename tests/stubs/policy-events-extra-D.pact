(module policy-events-extra-D G
  (implements token-policy-ng-v1)
  (use token-policy-ng-v1 [token-info])
  (use marmalade-ng.policy-extra-policies)

  (defcap G() true)

  (defcap EVENT-D:bool (func:string)
    @event
    true)

  (defcap EVENT-SETTLE-D:bool (func:string)
    @event
    true)

  (defun rank:integer ()
    30)

  (defun enforce-init:bool (token:object{token-info})
    (emit-event (EVENT-D "init"))
  )

  (defun enforce-mint:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (POLICY-ENFORCE-MINT token policy-events-extra-D))
    (emit-event (EVENT-D "mint"))
  )

  (defun enforce-burn:bool (token:object{token-info} account:string amount:decimal)
    (require-capability (POLICY-ENFORCE-BURN token policy-events-extra-D))
    (emit-event (EVENT-D "burn"))
  )

  (defun enforce-transfer:bool (token:object{token-info} sender:string receiver:string amount:decimal)
    (require-capability (POLICY-ENFORCE-TRANSFER token policy-events-extra-D))
    (emit-event (EVENT-D "transfer"))
  )

  (defun enforce-sale-offer:bool (token:object{token-info} seller:string amount:decimal timeout:time)
    (require-capability (POLICY-ENFORCE-OFFER token (pact-id) policy-events-extra-D))
    (emit-event (EVENT-D "sale-offer"))
    false
  )

  (defun enforce-sale-withdraw:bool (token:object{token-info})
    (require-capability (POLICY-ENFORCE-WITHDRAW token (pact-id) policy-events-extra-D))
    (emit-event (EVENT-D "sale-withdraw"))
  )

  (defun enforce-sale-buy:bool (token:object{token-info} buyer:string)
    (require-capability (POLICY-ENFORCE-BUY token (pact-id) policy-events-extra-D))
    (emit-event (EVENT-D "sale-buy"))
  )

  (defun enforce-sale-settle:bool (token:object{token-info})
    (require-capability (POLICY-ENFORCE-SETTLE token (pact-id) policy-events-extra-D))
    (emit-event (EVENT-SETTLE-D "sale-settle"))
  )

)
