
(define-non-fungible-token rental-nft uint)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-rented (err u102))
(define-constant err-rental-expired (err u103))
(define-constant err-not-renter (err u104))
(define-constant err-invalid-duration (err u105))

;; Data structures
(define-map nft-rentals
  uint
  {
    owner: principal,
    renter: principal,
    rental-start: uint,
    rental-end: uint,
    rental-price: uint
  })

(define-data-var next-nft-id uint u1)

;; Function 1: Rent NFT - Allows users to rent an NFT for a specified duration
(define-public (rent-nft (nft-id uint) (duration-blocks uint) (rental-price uint))
  (let (
    (current-block stacks-block-height)
    (rental-end (+ current-block duration-blocks))
    (nft-owner (unwrap! (nft-get-owner? rental-nft nft-id) err-not-found))
  )
    (begin
     
      (asserts! (> duration-blocks u0) err-invalid-duration)
      
      
      (asserts! (is-none (map-get? nft-rentals nft-id)) err-already-rented)
      
      
      (try! (stx-transfer? rental-price tx-sender nft-owner))
      
      
      (map-set nft-rentals nft-id {
        owner: nft-owner,
        renter: tx-sender,
        rental-start: current-block,
        rental-end: rental-end,
        rental-price: rental-price,
      })
      
      (ok {
        nft-id: nft-id,
        renter: tx-sender,
        rental-start: current-block,
        rental-end: rental-end,
        message: "NFT rented successfully"
      })
    )
  )
)

;; Function 2: Return NFT - Automatically returns NFT after rental period or allows manual return
(define-public (return-nft (nft-id uint))
  (let (
    (rental-info (unwrap! (map-get? nft-rentals nft-id) err-not-found))
    (current-block stacks-block-height)
  )
    (begin
      ;; Check if caller is the renter or rental has expired
      (asserts! 
        (or 
          (is-eq tx-sender (get renter rental-info))
          (>= current-block (get rental-end rental-info))
        ) 
        err-not-renter
      )
      
      ;; Remove rental record (effectively returning NFT to owner)
      (map-delete nft-rentals nft-id)
      
      (ok {
        nft-id: nft-id,
        returned-to: (get owner rental-info),
        returned-at: current-block,
        message: "NFT returned successfully"
      })
    )
  )
)

;; Helper function: Get rental info
(define-read-only (get-rental-info (nft-id uint))
  (ok (map-get? nft-rentals nft-id))
)

;; Helper function: Check if rental is active
(define-read-only (is-rental-active (nft-id uint))
  (match (map-get? nft-rentals nft-id)
    rental-info (ok (< stacks-block-height (get rental-end rental-info)))
    (ok false)
  )
)

;; Helper function: Get current renter
(define-read-only (get-current-renter (nft-id uint))
  (match (map-get? nft-rentals nft-id)
    rental-info 
      (if (< stacks-block-height (get rental-end rental-info))
        (ok (some (get renter rental-info)))
        (ok none))
    (ok none)
  )
)