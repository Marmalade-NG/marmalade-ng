(module collection-bid-payer GOV
  (implements payer-manager-v1)

  (use policy-fixed-sale [get-sale])
  (use ledger [total-supply])
  (use policy-collection [get-token-collection])

  (defcap GOV ()
    (enforce false "No upgrade")
  )

  (defschema bid-payer-spec
    collection:string
    max-price:decimal
    amount:decimal
  )

  (defun get-collection:string (token-id:string)
    (try  "" (at 'name (get-token-collection token-id)))
  )

  (defun enforce-collection:bool (token-id:string req-collection:string)
    (let ((token-collection (get-collection token-id)))
      (enforce (= token-collection req-collection) "Bad collection"))
  )

  (defun enforce-buyer:bool (req-buyer:string)
    (enforce (= (read-string 'buyer) req-buyer) "Bad buyer")
  )

  (defun --enforce-payer:bool (spec:object{bid-payer-spec})
    (bind spec {'collection:=collection,
                'max-price:=max-price,
                'amount:=req-amount}
      (bind (get-sale (pact-id)) {'token-id:=token-id,
                                  'price:=price,
                                  'amount:=amount}
        (enforce (<= price max-price) "Price too high")
        (enforce (>= amount req-amount) "Amount too low")
        (enforce-collection token-id collection)))
  )

  (defun enforce-payer:bool (spec:object)
    (--enforce-payer spec))


  (defun fct:bool (buyer:string buyer-guard:guard spec:object{bid-payer-spec})
    (pact-id)
    (enforce-buyer buyer)
    (enforce-one "Account not granted"[(require-capability (policy-fixed-sale.FIXED-SALE-PAYMENT collection-bid-payer spec))
                                       (enforce-guard buyer-guard)])
  )

  (defun create-guard:guard (buyer:string buyer-guard:guard max-price:decimal collection:string)
    (create-user-guard (fct buyer buyer-guard
                                  {'max-price:max-price, 'amount:1.0, 'collection:collection}))
  )

  (defun create-account (fungible:module{fungible-v2} buyer:string max-price:decimal collection:string)
    (let* ((buyer-g (at 'guard (fungible::details buyer)))
           (g (create-guard buyer buyer-g max-price collection)))
      (fungible::create-account (create-principal g) g))
  )
)
