;; title: BlindAuction
;; version: 1.0
;; summary: Minimal blind auction in Clarity 3.6.1.
;; description: A simplified blind auction where bidders submit blinded bids with deposits. Reveal phase validates and places bids. Highest bid wins, beneficiary collects after end. Uses block heights for timing, STX for payments.

;; traits
;;

;; token definitions
;;

;; constants
(define-constant err-already-initialized u100)
(define-constant err-bidding-active u101)
(define-constant err-reveal-active u102)
(define-constant err-reveal-ended u103)
(define-constant err-invalid-length u104)
(define-constant err-auction-ended u105)
(define-constant err-auction-not-ended u106)
(define-constant err-too-many-bids u107)
(define-constant max-bids-per-bidder u100)

;; data vars
(define-data-var beneficiary principal tx-sender)
(define-data-var bidding-end uint u0)
(define-data-var reveal-end uint u0)
(define-data-var ended bool false)
(define-data-var highest-bidder (optional principal) none)
(define-data-var highest-bid uint u0)

;; data maps
(define-map bidder-bid-count principal uint)
(define-map bids {bidder: principal, index: uint} {blinded-bid: (buff 32), deposit: uint})
(define-map pending-returns principal uint)

;; private functions

(define-private (reveal-bids-rec (index uint) (refund uint) (values (list 100 uint)) (fakes (list 100 bool)) (secrets (list 100 (buff 32))) (length uint))
  (if (>= index length)
    refund
    (let (
      (value (unwrap-panic (element-at values index)))
      (fake (unwrap-panic (element-at fakes index)))
      (secret (unwrap-panic (element-at secrets index)))
      (bid-entry (unwrap-panic (map-get? bids {bidder: tx-sender, index: index})))
      (blinded-bid (get blinded-bid bid-entry))
      (deposit (get deposit bid-entry))
      (computed-hash (keccak256 (unwrap-panic (to-consensus-buff? (tuple (value value) (fake fake) (secret secret))))))
    )
      (map-set bids {bidder: tx-sender, index: index} {blinded-bid: 0x0000000000000000000000000000000000000000000000000000000000000000, deposit: deposit})
      (if (is-eq blinded-bid computed-hash)
        (let ((new-refund (+ refund deposit)))
          (if (and (not fake) (>= deposit value))
            (if (place-bid tx-sender value)
              (reveal-bids-rec (+ index u1) (- new-refund value) values fakes secrets length)
              (reveal-bids-rec (+ index u1) new-refund values fakes secrets length)
            )
            (reveal-bids-rec (+ index u1) new-refund values fakes secrets length)
          )
        )
        (reveal-bids-rec (+ index u1) refund values fakes secrets length)
      )
    )
  )
)

(define-private (place-bid (bidder principal) (value uint))
  (if (<= value (var-get highest-bid))
    false
    (begin
      (match (var-get highest-bidder) prev-bidder
        (map-set pending-returns prev-bidder (+ (default-to u0 (map-get? pending-returns prev-bidder)) (var-get highest-bid)))
        true
      )
      (var-set highest-bid value)
      (var-set highest-bidder (some bidder))
      true
    )
  )
)


;; public functions
(define-public (init (bidding-time uint) (reveal-time uint) (new-beneficiary principal))
  (begin
    (asserts! (is-eq (var-get bidding-end) u0) (err err-already-initialized))
    (var-set beneficiary new-beneficiary)
    (var-set bidding-end (+ (block-height) bidding-time))
    (var-set reveal-end (+ (var-get bidding-end) reveal-time))
    (ok true)
  )
)

(define-public (bid (blinded-bid (buff 32)) (deposit uint))
  (let ((count (default-to u0 (map-get? bidder-bid-count tx-sender))))
    (asserts! (< (block-height) (var-get bidding-end)) (err err-bidding-active))
    (asserts! (< count max-bids-per-bidder) (err err-too-many-bids))
    (try! (stx-transfer? deposit tx-sender (as-contract tx-sender)))
    (map-set bids {bidder: tx-sender, index: count} {blinded-bid: blinded-bid, deposit: deposit})
    (map-set bidder-bid-count tx-sender (+ count u1))
    (ok true)
  )
)

(define-public (reveal (values (list 100 uint)) (fakes (list 100 bool)) (secrets (list 100 (buff 32))))
  (let ((length (default-to u0 (map-get? bidder-bid-count tx-sender))))
    (asserts! (> (block-height) (var-get bidding-end)) (err err-reveal-active))
    (asserts! (< (block-height) (var-get reveal-end)) (err err-reveal-ended))
    (asserts! (is-eq length (len values)) (err err-invalid-length))
    (asserts! (is-eq length (len fakes)) (err err-invalid-length))
    (asserts! (is-eq length (len secrets)) (err err-invalid-length))
    (let ((refund (reveal-bids-rec u0 u0 values fakes secrets length)))
      (try! (as-contract (stx-transfer? refund tx-sender tx-sender)))
      (ok true)
    )
  )
)

(define-public (withdraw)
  (let ((amount (default-to u0 (map-get? pending-returns tx-sender))))
    (if (> amount u0)
      (begin
        (map-set pending-returns tx-sender u0)
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        (ok amount)
      )
      (ok u0)
    )
  )
)

(define-public (auction-end)
  (begin
    (asserts! (> (block-height) (var-get reveal-end)) (err err-reveal-ended))
    (asserts! (not (var-get ended)) (err err-auction-ended))
    (var-set ended true)
    (print {event: "AuctionEnded", winner: (var-get highest-bidder), highest-bid: (var-get highest-bid)})
    (try! (as-contract (stx-transfer? (var-get highest-bid) tx-sender (var-get beneficiary))))
    (ok true)
  )
)

;; read only functions
;;