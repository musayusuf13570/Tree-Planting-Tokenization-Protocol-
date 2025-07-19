
(define-non-fungible-token tree-nft uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-coordinates (err u102))
(define-constant err-already-verified (err u103))

(define-map tree-data uint 
  {
    owner: principal,
    latitude: int,
    longitude: int,
    planting-date: uint,
    height: uint,
    health-score: uint,
    last-verification: uint,
    carbon-credits: uint
  }
)

(define-map verifier-status principal bool)

(define-data-var next-token-id uint u1)
(define-data-var verification-period uint u144) ;; ~24 hours in blocks

(define-public (mint-tree-nft (latitude int) (longitude int))
    (let 
        ((token-id (var-get next-token-id)))
        (try! (validate-coordinates latitude longitude))
        (try! (nft-mint? tree-nft token-id tx-sender))
        (map-set tree-data token-id {
            owner: tx-sender,
            latitude: latitude,
            longitude: longitude,
            planting-date: stacks-block-height,
            height: u0,
            health-score: u100,
            last-verification: stacks-block-height,
            carbon-credits: u0
        })
        (var-set next-token-id (+ token-id u1))
        (ok token-id)
    )
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (try! (nft-transfer? tree-nft token-id sender recipient))
        (let ((tree (unwrap! (map-get? tree-data token-id) (err u404))))
            (map-set tree-data token-id (merge tree {owner: recipient}))
        )
        (ok true)
    )
)

(define-public (update-tree-verification (token-id uint) (new-height uint) (health-score uint))
    (let ((tree (unwrap! (map-get? tree-data token-id) err-not-found)))
        (asserts! (is-verifier tx-sender) err-owner-only)
        (asserts! (unwrap! (can-verify token-id) err-not-found) err-already-verified)
        (map-set tree-data token-id (merge tree {
            height: new-height,
            health-score: health-score,
            last-verification: stacks-block-height,
            carbon-credits: (calculate-carbon-credits new-height)
        }))
        (ok true)
    )
)

(define-public (add-verifier (address principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set verifier-status address true)
        (ok true)
    )
)

(define-public (remove-verifier (address principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set verifier-status address false)
        (ok true)
    )
)

(define-read-only (get-tree-details (token-id uint))
    (ok (unwrap! (map-get? tree-data token-id) err-not-found))
)

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? tree-nft token-id))
)

(define-read-only (is-verifier (address principal))
    (default-to false (map-get? verifier-status address))
)

(define-read-only (can-verify (token-id uint))
    (ok (let ((tree (unwrap! (map-get? tree-data token-id) err-not-found)))
        (>= (- stacks-block-height (get last-verification tree)) (var-get verification-period))
    ))
)

(define-private (validate-coordinates (latitude int) (longitude int))
    (if (and 
            (and (>= latitude (* -90 1000000)) (<= latitude (* 90 1000000)))
            (and (>= longitude (* -180 1000000)) (<= longitude (* 180 1000000)))
        )
        (ok true)
        err-invalid-coordinates
    )
)

(define-private (calculate-carbon-credits (height uint))
    ;; Simple calculation: 1 credit per 10 units of height
    (/ height u10)
)
