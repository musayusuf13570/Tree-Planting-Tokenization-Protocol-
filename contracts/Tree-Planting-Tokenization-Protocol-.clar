
(define-non-fungible-token tree-nft uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-coordinates (err u102))
(define-constant err-already-verified (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-not-for-sale (err u105))
(define-constant err-invalid-price (err u106))
(define-constant err-insufficient-credits (err u107))
(define-constant err-milestone-already-claimed (err u108))
(define-constant err-milestone-not-reached (err u109))
(define-constant err-insufficient-reward-pool (err u110))

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

(define-map marketplace-listings uint {
    price: uint,
    seller: principal,
    listed: bool
})

(define-map carbon-credit-orders uint {
    credits-amount: uint,
    price-per-credit: uint,
    seller: principal,
    active: bool
})

(define-data-var next-token-id uint u1)
(define-data-var verification-period uint u144)
(define-data-var next-order-id uint u1)
(define-data-var reward-pool uint u0)

(define-map milestone-rewards uint uint)

(define-map tree-milestone-claims uint {
    milestone-1: bool,
    milestone-2: bool,
    milestone-3: bool,
    milestone-4: bool,
    milestone-5: bool
})

(map-set milestone-rewards u1 u50000)
(map-set milestone-rewards u2 u100000)
(map-set milestone-rewards u3 u200000)
(map-set milestone-rewards u4 u400000)
(map-set milestone-rewards u5 u800000)

(define-public (fund-reward-pool (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set reward-pool (+ (var-get reward-pool) amount))
        (ok true)
    )
)

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
        (map-set tree-milestone-claims token-id {
            milestone-1: false,
            milestone-2: false,
            milestone-3: false,
            milestone-4: false,
            milestone-5: false
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

(define-public (list-tree-for-sale (token-id uint) (price uint))
    (let ((tree (unwrap! (map-get? tree-data token-id) err-not-found)))
        (asserts! (is-eq (get owner tree) tx-sender) err-owner-only)
        (asserts! (> price u0) err-invalid-price)
        (map-set marketplace-listings token-id {
            price: price,
            seller: tx-sender,
            listed: true
        })
        (ok true)
    )
)

(define-public (delist-tree (token-id uint))
    (let ((listing (unwrap! (map-get? marketplace-listings token-id) err-not-found)))
        (asserts! (is-eq (get seller listing) tx-sender) err-owner-only)
        (map-delete marketplace-listings token-id)
        (ok true)
    )
)

(define-public (buy-tree (token-id uint))
    (let (
        (listing (unwrap! (map-get? marketplace-listings token-id) err-not-for-sale))
        (tree (unwrap! (map-get? tree-data token-id) err-not-found))
        (price (get price listing))
        (seller (get seller listing))
    )
        (asserts! (get listed listing) err-not-for-sale)
        (try! (stx-transfer? price tx-sender seller))
        (try! (nft-transfer? tree-nft token-id seller tx-sender))
        (map-set tree-data token-id (merge tree {owner: tx-sender}))
        (map-delete marketplace-listings token-id)
        (ok true)
    )
)

(define-public (create-carbon-credit-order (token-id uint) (price-per-credit uint))
    (let (
        (order-id (var-get next-order-id))
        (tree (unwrap! (map-get? tree-data token-id) err-not-found))
        (credits-amount (get carbon-credits tree))
    )
        (asserts! (is-eq (get owner tree) tx-sender) err-owner-only)
        (asserts! (> credits-amount u0) err-insufficient-credits)
        (asserts! (> price-per-credit u0) err-invalid-price)
        (map-set carbon-credit-orders order-id {
            credits-amount: credits-amount,
            price-per-credit: price-per-credit,
            seller: tx-sender,
            active: true
        })
        (var-set next-order-id (+ order-id u1))
        (ok order-id)
    )
)

(define-public (buy-carbon-credits (order-id uint) (credits-to-buy uint))
    (let (
        (order (unwrap! (map-get? carbon-credit-orders order-id) err-not-found))
        (total-price (* (get price-per-credit order) credits-to-buy))
        (seller (get seller order))
    )
        (asserts! (get active order) err-not-for-sale)
        (asserts! (<= credits-to-buy (get credits-amount order)) err-insufficient-credits)
        (try! (stx-transfer? total-price tx-sender seller))
        (if (is-eq credits-to-buy (get credits-amount order))
            (map-delete carbon-credit-orders order-id)
            (map-set carbon-credit-orders order-id (merge order {
                credits-amount: (- (get credits-amount order) credits-to-buy)
            }))
        )
        (ok credits-to-buy)
    )
)

(define-read-only (get-marketplace-listing (token-id uint))
    (ok (map-get? marketplace-listings token-id))
)

(define-read-only (get-carbon-credit-order (order-id uint))
    (ok (map-get? carbon-credit-orders order-id))
)

(define-public (claim-milestone-reward (token-id uint) (milestone uint))
    (let (
        (tree (unwrap! (map-get? tree-data token-id) err-not-found))
        (claims (unwrap! (map-get? tree-milestone-claims token-id) err-not-found))
        (reward-amount (unwrap! (map-get? milestone-rewards milestone) err-not-found))
        (tree-height (get height tree))
    )
        (asserts! (is-eq (get owner tree) tx-sender) err-owner-only)
        (asserts! (<= reward-amount (var-get reward-pool)) err-insufficient-reward-pool)
        (asserts! (>= tree-height (get-milestone-height-requirement milestone)) err-milestone-not-reached)
        (asserts! (not (get-milestone-claim-status claims milestone)) err-milestone-already-claimed)
        (try! (as-contract (stx-transfer? reward-amount tx-sender (get owner tree))))
        (var-set reward-pool (- (var-get reward-pool) reward-amount))
        (map-set tree-milestone-claims token-id (set-milestone-claim-status claims milestone))
        (ok reward-amount)
    )
)

(define-read-only (get-milestone-height-requirement (milestone uint))
    (if (is-eq milestone u1) u50
        (if (is-eq milestone u2) u150
            (if (is-eq milestone u3) u300
                (if (is-eq milestone u4) u500
                    (if (is-eq milestone u5) u750
                        u0
                    )
                )
            )
        )
    )
)

(define-read-only (get-milestone-claim-status (claims {milestone-1: bool, milestone-2: bool, milestone-3: bool, milestone-4: bool, milestone-5: bool}) (milestone uint))
    (if (is-eq milestone u1) (get milestone-1 claims)
        (if (is-eq milestone u2) (get milestone-2 claims)
            (if (is-eq milestone u3) (get milestone-3 claims)
                (if (is-eq milestone u4) (get milestone-4 claims)
                    (if (is-eq milestone u5) (get milestone-5 claims)
                        false
                    )
                )
            )
        )
    )
)

(define-private (set-milestone-claim-status (claims {milestone-1: bool, milestone-2: bool, milestone-3: bool, milestone-4: bool, milestone-5: bool}) (milestone uint))
    (if (is-eq milestone u1) (merge claims {milestone-1: true})
        (if (is-eq milestone u2) (merge claims {milestone-2: true})
            (if (is-eq milestone u3) (merge claims {milestone-3: true})
                (if (is-eq milestone u4) (merge claims {milestone-4: true})
                    (if (is-eq milestone u5) (merge claims {milestone-5: true})
                        claims
                    )
                )
            )
        )
    )
)

(define-read-only (get-tree-milestone-claims (token-id uint))
    (ok (map-get? tree-milestone-claims token-id))
)

(define-read-only (get-reward-pool-balance)
    (ok (var-get reward-pool))
)

(define-read-only (get-milestone-reward-amount (milestone uint))
    (ok (map-get? milestone-rewards milestone))
)

(define-private (calculate-carbon-credits (height uint))
    (/ height u10)
)
