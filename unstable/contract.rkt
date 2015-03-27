#lang racket/base
(require racket/contract/base
         racket/contract/combinator
         (only-in racket/sequence sequence/c)
         (only-in racket/string non-empty-string?)
         (only-in racket/tcp port-number? listen-port-number?))

(define path-piece?
  (or/c path-string? (symbols 'up 'same)))

;; Added by asumu
;; option/c : contract -> contract
(define (option/c ctc-arg)
  (define ctc (coerce-contract 'option/c ctc-arg))
  (cond [(flat-contract? ctc) (flat-option/c ctc)]
        [(chaperone-contract? ctc) (chaperone-option/c ctc)]
        [else (impersonator-option/c ctc)]))

(define (option/c-name ctc)
  (build-compound-type-name 'option/c (base-option/c-ctc ctc)))

(define (option/c-projection ctc)
  (define ho-proj (contract-projection (base-option/c-ctc ctc)))
  (λ (blame)
    (define partial (ho-proj blame))
    (λ (val)
      (if (not val) val (partial val)))))

(define ((option/c-first-order ctc) v)
  (or (not v) (contract-first-order-passes? (base-option/c-ctc ctc) v)))

(define (option/c-stronger? this that)
  (and (base-option/c? that)
       (contract-stronger? (base-option/c-ctc this)
                           (base-option/c-ctc that))))

(struct base-option/c (ctc))

(struct flat-option/c base-option/c ()
        #:property prop:flat-contract
        (build-flat-contract-property
          #:name option/c-name
          #:first-order option/c-first-order
          #:stronger option/c-stronger?))

(struct chaperone-option/c base-option/c ()
        #:property prop:chaperone-contract
        (build-chaperone-contract-property
          #:name option/c-name
          #:first-order option/c-first-order
          #:projection option/c-projection
          #:stronger option/c-stronger?))

(struct impersonator-option/c base-option/c ()
        #:property prop:contract
        (build-contract-property
          #:name option/c-name
          #:first-order option/c-first-order
          #:projection option/c-projection
          #:stronger option/c-stronger?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Flat Contracts
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define truth/c
  (flat-named-contract '|truth value| (lambda (x) #t)))

;; Added by ntoronto

(define (treeof elem-contract)
  (define tree-contract
    (or/c elem-contract
          (listof
            (cond
              [(flat-contract? elem-contract) (recursive-contract tree-contract #:flat)]
              [(chaperone-contract? elem-contract) (recursive-contract tree-contract #:chaperone)]
              [else (recursive-contract tree-contract)]))))
  tree-contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Exports
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract
 [path-piece? contract?]

 [rename option/c maybe/c (-> contract? contract?)]

 [truth/c flat-contract?]
 
 [treeof (contract? . -> . contract?)])
(provide sequence/c
         non-empty-string?
         port-number?
         (rename-out [listen-port-number? tcp-listen-port?])
         rename-contract
         if/c
         failure-result/c)
