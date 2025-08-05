;; Basic Crowdfunding Contract
;; A simple fundraising platform with goal tracking, automatic refunds, and milestone releases

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-CAMPAIGN-NOT-FOUND (err u101))
(define-constant ERR-CAMPAIGN-ENDED (err u102))
(define-constant ERR-CAMPAIGN-ACTIVE (err u103))
(define-constant ERR-GOAL-ALREADY-MET (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))
(define-constant ERR-NO-CONTRIBUTION (err u106))
(define-constant ERR-MILESTONE-NOT-FOUND (err u107))
(define-constant ERR-MILESTONE-ALREADY-RELEASED (err u108))
(define-constant ERR-GOAL-NOT-MET (err u109))
(define-constant ERR-INVALID-MILESTONE (err u110))
(define-constant ERR-CAMPAIGN-NOT-ENDED (err u111))

;; Data structures
(define-map campaigns
  { campaign-id: uint }
  {
    creator: principal,
    title: (string-ascii 50),
    description: (string-ascii 200),
    goal: uint,
    deadline: uint,
    total-raised: uint,
    milestone-count: uint,
    funds-released: uint,
    is-active: bool
  }
)

(define-map contributions
  { campaign-id: uint, contributor: principal }
  { amount: uint }
)

(define-map milestones
  { campaign-id: uint, milestone-id: uint }
  {
    description: (string-ascii 100),
    amount: uint,
    is-released: bool
  }
)

;; Campaign counter
(define-data-var next-campaign-id uint u1)

;; Events
(define-data-var contract-owner principal tx-sender)

;; Read-only functions

(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns { campaign-id: campaign-id })
)

(define-read-only (get-contribution (campaign-id uint) (contributor principal))
  (map-get? contributions { campaign-id: campaign-id, contributor: contributor })
)

(define-read-only (get-milestone (campaign-id uint) (milestone-id uint))
  (map-get? milestones { campaign-id: campaign-id, milestone-id: milestone-id })
)

(define-read-only (get-next-campaign-id)
  (var-get next-campaign-id)
)

(define-read-only (is-campaign-successful (campaign-id uint))
  (match (get-campaign campaign-id)
    campaign (>= (get total-raised campaign) (get goal campaign))
    false
  )
)

(define-read-only (is-campaign-ended (campaign-id uint))
  (match (get-campaign campaign-id)
    campaign (>= stacks-block-height (get deadline campaign))
    false
  )
)

(define-read-only (get-refundable-amount (campaign-id uint) (contributor principal))
  (let ((contribution (get-contribution campaign-id contributor)))
    (if (and
          (is-some contribution)
          (is-campaign-ended campaign-id)
          (not (is-campaign-successful campaign-id)))
      (some (get amount (unwrap-panic contribution)))
      none
    )
  )
)

;; Public functions

(define-public (create-campaign
  (title (string-ascii 50))
  (description (string-ascii 200))
  (goal uint)
  (duration-blocks uint))

  (let ((campaign-id (var-get next-campaign-id))
        (deadline (+ stacks-block-height duration-blocks)))

    ;; Create the campaign
    (map-set campaigns
      { campaign-id: campaign-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        goal: goal,
        deadline: deadline,
        total-raised: u0,
        milestone-count: u0,
        funds-released: u0,
        is-active: true
      }
    )

    ;; Increment campaign counter
    (var-set next-campaign-id (+ campaign-id u1))

    ;; Print event
    (print {
      event: "campaign-created",
      campaign-id: campaign-id,
      creator: tx-sender,
      goal: goal,
      deadline: deadline
    })

    (ok campaign-id)
  )
)

(define-public (contribute (campaign-id uint) (amount uint))
  (let ((campaign (unwrap! (get-campaign campaign-id) ERR-CAMPAIGN-NOT-FOUND))
        (existing-contribution (default-to { amount: u0 }
          (get-contribution campaign-id tx-sender))))

    ;; Validate campaign is active and not ended
    (asserts! (get is-active campaign) ERR-CAMPAIGN-ENDED)
    (asserts! (< stacks-block-height (get deadline campaign)) ERR-CAMPAIGN-ENDED)
    (asserts! (< (get total-raised campaign) (get goal campaign)) ERR-GOAL-ALREADY-MET)

    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; Update contribution
    (map-set contributions
      { campaign-id: campaign-id, contributor: tx-sender }
      { amount: (+ (get amount existing-contribution) amount) }
    )

    ;; Update campaign total
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { total-raised: (+ (get total-raised campaign) amount) })
    )

    ;; Print event
    (print {
      event: "contribution-made",
      campaign-id: campaign-id,
      contributor: tx-sender,
      amount: amount,
      total-raised: (+ (get total-raised campaign) amount)
    })

    (ok true)
  )
)

(define-public (add-milestone
  (campaign-id uint)
  (description (string-ascii 100))
  (amount uint))

  (let ((campaign (unwrap! (get-campaign campaign-id) ERR-CAMPAIGN-NOT-FOUND)))

    ;; Only creator can add milestones
    (asserts! (is-eq tx-sender (get creator campaign)) ERR-NOT-AUTHORIZED)

    ;; Campaign must be active
    (asserts! (get is-active campaign) ERR-CAMPAIGN-ENDED)

    ;; Add milestone
    (map-set milestones
      { campaign-id: campaign-id, milestone-id: (get milestone-count campaign) }
      {
        description: description,
        amount: amount,
        is-released: false
      }
    )

    ;; Update milestone count
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { milestone-count: (+ (get milestone-count campaign) u1) })
    )

    ;; Print event
    (print {
      event: "milestone-added",
      campaign-id: campaign-id,
      milestone-id: (get milestone-count campaign),
      amount: amount
    })

    (ok (get milestone-count campaign))
  )
)

(define-public (release-milestone (campaign-id uint) (milestone-id uint))
  (let ((campaign (unwrap! (get-campaign campaign-id) ERR-CAMPAIGN-NOT-FOUND))
        (milestone (unwrap! (get-milestone campaign-id milestone-id) ERR-MILESTONE-NOT-FOUND)))

    ;; Only creator can release milestones
    (asserts! (is-eq tx-sender (get creator campaign)) ERR-NOT-AUTHORIZED)

    ;; Campaign must have met its goal
    (asserts! (is-campaign-successful campaign-id) ERR-GOAL-NOT-MET)

    ;; Milestone must not be already released
    (asserts! (not (get is-released milestone)) ERR-MILESTONE-ALREADY-RELEASED)

    ;; Check if enough funds are available for release
    (let ((available-funds (- (get total-raised campaign) (get funds-released campaign))))
      (asserts! (>= available-funds (get amount milestone)) ERR-INSUFFICIENT-FUNDS)

      ;; Transfer funds to creator
      (try! (as-contract (stx-transfer? (get amount milestone) tx-sender (get creator campaign))))

      ;; Mark milestone as released
      (map-set milestones
        { campaign-id: campaign-id, milestone-id: milestone-id }
        (merge milestone { is-released: true })
      )

      ;; Update funds released
      (map-set campaigns
        { campaign-id: campaign-id }
        (merge campaign { funds-released: (+ (get funds-released campaign) (get amount milestone)) })
      )

      ;; Print event
      (print {
        event: "milestone-released",
        campaign-id: campaign-id,
        milestone-id: milestone-id,
        amount: (get amount milestone),
        recipient: (get creator campaign)
      })

      (ok true)
    )
  )
)

(define-public (claim-refund (campaign-id uint))
  (let ((campaign (unwrap! (get-campaign campaign-id) ERR-CAMPAIGN-NOT-FOUND))
        (contribution (unwrap! (get-contribution campaign-id tx-sender) ERR-NO-CONTRIBUTION)))

    ;; Campaign must be ended and unsuccessful
    (asserts! (is-campaign-ended campaign-id) ERR-CAMPAIGN-NOT-ENDED)
    (asserts! (not (is-campaign-successful campaign-id)) ERR-GOAL-ALREADY-MET)

    ;; Get refund amount
    (let ((refund-amount (get amount contribution)))

      ;; Remove contribution record
      (map-delete contributions { campaign-id: campaign-id, contributor: tx-sender })

      ;; Transfer refund
      (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))

      ;; Print event
      (print {
        event: "refund-claimed",
        campaign-id: campaign-id,
        contributor: tx-sender,
        amount: refund-amount
      })

      (ok refund-amount)
    )
  )
)

(define-public (end-campaign (campaign-id uint))
  (let ((campaign (unwrap! (get-campaign campaign-id) ERR-CAMPAIGN-NOT-FOUND)))

    ;; Only creator or after deadline
    (asserts! (or
      (is-eq tx-sender (get creator campaign))
      (>= stacks-block-height (get deadline campaign))
    ) ERR-NOT-AUTHORIZED)

    ;; Campaign must be active
    (asserts! (get is-active campaign) ERR-CAMPAIGN-ENDED)

    ;; Mark campaign as inactive
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign { is-active: false })
    )

    ;; If goal was met and no milestones, release all funds to creator
    (if (and
          (is-campaign-successful campaign-id)
          (is-eq (get milestone-count campaign) u0))
      (begin
        (try! (as-contract (stx-transfer? (get total-raised campaign) tx-sender (get creator campaign))))
        (map-set campaigns
          { campaign-id: campaign-id }
          (merge campaign { funds-released: (get total-raised campaign) })
        )
      )
      true
    )

    ;; Print event
    (print {
      event: "campaign-ended",
      campaign-id: campaign-id,
      successful: (is-campaign-successful campaign-id),
      total-raised: (get total-raised campaign)
    })

    (ok true)
  )
)
