
;; =================================
;; CONTRACT 2: Auction House
;; =================================
;; contracts/auction-house.clar

;; Digital Asset Auction House Contract
;; Handles auctions for NFTs with automatic winner selection and payment processing

;; Define NFT trait locally
(define-trait nft-trait
  (
    (get-last-token-id () (response uint uint))
    (get-token-uri (uint) (response (optional (string-ascii 256)) uint))
    (get-owner (uint) (response (optional principal) uint))
    (transfer (uint principal principal) (response bool uint))
  )
)

;; Error constants
(define-constant ERR-AUCTION-NOT-AUTHORIZED (err u401))
(define-constant ERR-AUCTION-NOT-FOUND (err u404))
(define-constant ERR-AUCTION-ENDED (err u405))
(define-constant ERR-AUCTION-ACTIVE (err u406))
(define-constant ERR-BID-TOO-LOW (err u407))
(define-constant ERR-TRANSFER-FAILED (err u408))
(define-constant ERR-INVALID-DURATION (err u409))
(define-constant ERR-INVALID-ASSET (err u410))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u411))

;; Constants
(define-constant MIN-BID-INCREMENT u1000000) ;; 1 STX minimum increment
(define-constant MAX-AUCTION-DURATION u1008) ;; ~1 week in blocks
(define-constant MIN-AUCTION-DURATION u144)  ;; ~1 day in blocks
(define-constant PLATFORM-FEE-BASIS-POINTS u250) ;; 2.5% platform fee

;; Data structures
(define-map auctions uint {
  asset-contract: principal,
  asset-id: uint,
  seller: principal,
  start-block: uint,
  end-block: uint,
  starting-bid: uint,
  current-bid: uint,
  current-bidder: (optional principal),
  ended: bool
})

(define-map bids {auction-id: uint, bidder: principal} {
  amount: uint,
  block-height: uint
})

(define-data-var next-auction-id uint u1)
(define-data-var platform-wallet principal tx-sender)

;; Read-only functions
(define-read-only (get-auction (auction-id uint))
  (map-get? auctions auction-id)
)

(define-read-only (get-bid (auction-id uint) (bidder principal))
  (map-get? bids {auction-id: auction-id, bidder: bidder})
)

(define-read-only (get-next-auction-id)
  (var-get next-auction-id)
)

(define-read-only (is-auction-active (auction-id uint))
  (match (map-get? auctions auction-id)
    auction
    (and
      (not (get ended auction))
      (<= stacks-block-height (get end-block auction))
      (>= stacks-block-height (get start-block auction))
    )
    false
  )
)

(define-read-only (calculate-platform-fee (amount uint))
  (/ (* amount PLATFORM-FEE-BASIS-POINTS) u10000)
)

;; Create auction
(define-public (create-auction
  (asset-contract <nft-trait>)
  (asset-id uint)
  (starting-bid uint)
  (duration uint))
  (let
    (
      (auction-id (var-get next-auction-id))
      (start-block stacks-block-height)
      (end-block (+ stacks-block-height duration))
    )
    ;; Validate inputs
    (asserts! (and (>= duration MIN-AUCTION-DURATION) (<= duration MAX-AUCTION-DURATION)) ERR-INVALID-DURATION)
    (asserts! (> starting-bid u0) ERR-BID-TOO-LOW)

    ;; Transfer NFT to contract (escrow)
    (try! (contract-call? asset-contract transfer asset-id tx-sender (as-contract tx-sender)))

    ;; Create auction record
    (map-set auctions auction-id {
      asset-contract: (contract-of asset-contract),
      asset-id: asset-id,
      seller: tx-sender,
      start-block: start-block,
      end-block: end-block,
      starting-bid: starting-bid,
      current-bid: starting-bid,
      current-bidder: none,
      ended: false
    })

    ;; Increment auction ID
    (var-set next-auction-id (+ auction-id u1))

    (ok auction-id)
  )
)

;; Place bid
(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let
    (
      (auction (unwrap! (map-get? auctions auction-id) ERR-AUCTION-NOT-FOUND))
      (current-bid (get current-bid auction))
      (current-bidder (get current-bidder auction))
      (min-bid (+ current-bid MIN-BID-INCREMENT))
    )
    ;; Validate auction is active
    (asserts! (is-auction-active auction-id) ERR-AUCTION-ENDED)

    ;; Validate bid amount
    (asserts! (>= bid-amount min-bid) ERR-BID-TOO-LOW)

    ;; Transfer STX from bidder to contract
    (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))

    ;; Refund previous bidder if exists
    (match current-bidder
      prev-bidder
      (try! (as-contract (stx-transfer? current-bid (as-contract tx-sender) prev-bidder)))
      true
    )

    ;; Update auction with new bid
    (map-set auctions auction-id
      (merge auction {
        current-bid: bid-amount,
        current-bidder: (some tx-sender)
      })
    )

    ;; Record bid
    (map-set bids {auction-id: auction-id, bidder: tx-sender} {
      amount: bid-amount,
      block-height: stacks-block-height
    })

    (ok true)
  )
)

;; End auction and process payment
(define-public (end-auction (auction-id uint) (asset-contract <nft-trait>))
  (let
    (
      (auction (unwrap! (map-get? auctions auction-id) ERR-AUCTION-NOT-FOUND))
      (seller (get seller auction))
      (current-bid (get current-bid auction))
      (current-bidder (get current-bidder auction))
      (stored-contract (get asset-contract auction))
      (asset-id (get asset-id auction))
      (platform-fee (calculate-platform-fee current-bid))
      (seller-amount (- current-bid platform-fee))
    )
    ;; Validate auction can be ended
    (asserts! (not (get ended auction)) ERR-AUCTION-ACTIVE)
    (asserts! (> stacks-block-height (get end-block auction)) ERR-AUCTION-ACTIVE)

    ;; Validate contract matches stored contract
    (asserts! (is-eq (contract-of asset-contract) stored-contract) ERR-AUCTION-NOT-AUTHORIZED)

    ;; Mark auction as ended
    (map-set auctions auction-id
      (merge auction { ended: true })
    )

    ;; Process based on whether there was a winning bid
    (match current-bidder
      winner
      (begin
        ;; Transfer NFT to winner
        (try! (as-contract (contract-call? asset-contract transfer asset-id (as-contract tx-sender) winner)))

        ;; Transfer payment to seller (minus platform fee)
        (try! (as-contract (stx-transfer? seller-amount (as-contract tx-sender) seller)))

        ;; Transfer platform fee
        (try! (as-contract (stx-transfer? platform-fee (as-contract tx-sender) (var-get platform-wallet))))

        (ok {winner: (some winner), final-price: current-bid})
      )
      ;; No bids - return NFT to seller
      (begin
        (try! (as-contract (contract-call? asset-contract transfer asset-id (as-contract tx-sender) seller)))
        (ok {winner: none, final-price: u0})
      )
    )
  )
)

;; Emergency functions (admin only)
(define-public (set-platform-wallet (new-wallet principal))
  (begin
    (asserts! (is-eq tx-sender (var-get platform-wallet)) ERR-AUCTION-NOT-AUTHORIZED)
    (var-set platform-wallet new-wallet)
    (ok true)
  )
)

;; Cancel auction (seller only, before any bids)
(define-public (cancel-auction (auction-id uint) (asset-contract <nft-trait>))
  (let
    (
      (auction (unwrap! (map-get? auctions auction-id) ERR-AUCTION-NOT-FOUND))
      (seller (get seller auction))
      (current-bidder (get current-bidder auction))
      (stored-contract (get asset-contract auction))
      (asset-id (get asset-id auction))
    )
    ;; Only seller can cancel
    (asserts! (is-eq tx-sender seller) ERR-AUCTION-NOT-AUTHORIZED)

    ;; Validate contract matches stored contract
    (asserts! (is-eq (contract-of asset-contract) stored-contract) ERR-AUCTION-NOT-AUTHORIZED)

    ;; Can only cancel if no bids placed
    (asserts! (is-none current-bidder) ERR-AUCTION-ACTIVE)

    ;; Mark as ended
    (map-set auctions auction-id
      (merge auction { ended: true })
    )

    ;; Return NFT to seller
    (try! (as-contract (contract-call? asset-contract transfer asset-id (as-contract tx-sender) seller)))

    (ok true)
  )
)
