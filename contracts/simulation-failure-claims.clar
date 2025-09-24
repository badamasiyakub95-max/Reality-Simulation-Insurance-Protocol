;; Automated Compensation for Simulation Breakdowns and Consciousness Extraction Failures
;; This contract processes insurance claims and manages compensation distribution for covered simulation incidents

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-INVALID-CLAIM (err u301))
(define-constant ERR-CLAIM-NOT-FOUND (err u302))
(define-constant ERR-CLAIM-ALREADY-PROCESSED (err u303))
(define-constant ERR-INSUFFICIENT-COVERAGE (err u304))
(define-constant ERR-POLICY-NOT-ACTIVE (err u305))
(define-constant ERR-INSUFFICIENT-FUNDS (err u306))
(define-constant ERR-FRAUDULENT-CLAIM (err u307))

;; Coverage types and compensation rates (in micro-STX)
(define-constant IMMEDIATE-RESPONSE-RATE u5000000) ;; 5 STX per hour
(define-constant RECOVERY-SUPPORT-RATE u10000000) ;; 10 STX per session
(define-constant LOST-TIME-COMPENSATION-RATE u2000000) ;; 2 STX per hour lost
(define-constant CONSCIOUSNESS-RESTORATION-RATE u50000000) ;; 50 STX per incident
(define-constant LEGAL-PROTECTION-RATE u25000000) ;; 25 STX per legal case

;; Claim severity multipliers
(define-constant MINOR-INCIDENT-MULTIPLIER u100) ;; 1x
(define-constant MODERATE-INCIDENT-MULTIPLIER u200) ;; 2x
(define-constant SEVERE-INCIDENT-MULTIPLIER u500) ;; 5x
(define-constant CRITICAL-INCIDENT-MULTIPLIER u1000) ;; 10x

;; Premium rates and policy limits
(define-constant BASE-PREMIUM u1000000) ;; 1 STX base premium
(define-constant MAX-COVERAGE-PER-INCIDENT u100000000) ;; 100 STX max per incident
(define-constant POLICY-DURATION u52560) ;; 1 year in blocks (10 sec blocks)

;; Data variables
(define-data-var claim-counter uint u0)
(define-data-var policy-counter uint u0)
(define-data-var insurance-pool uint u0)
(define-data-var total-claims-paid uint u0)

;; Data maps
(define-map insurance-policies uint {
    id: uint,
    policyholder: principal,
    coverage-type: (string-ascii 32),
    premium-paid: uint,
    coverage-limit: uint,
    policy-start: uint,
    policy-end: uint,
    claims-made: uint,
    status: (string-ascii 20),
    risk-score: uint
})

(define-map insurance-claims uint {
    id: uint,
    policy-id: uint,
    claimant: principal,
    incident-type: (string-ascii 32),
    severity: uint,
    simulation-id: uint,
    session-id: uint,
    time-lost: uint,
    medical-costs: uint,
    claim-amount: uint,
    status: (string-ascii 20),
    filed-at: uint,
    processed-at: (optional uint),
    approved-amount: uint
})

(define-map claim-evidence uint {
    claim-id: uint,
    evidence-type: (string-ascii 32),
    evidence-hash: (buff 32),
    submitted-at: uint,
    verified: bool
})

(define-map fraud-detection uint {
    claim-id: uint,
    suspicious-patterns: (list 5 (string-ascii 32)),
    risk-indicators: uint,
    investigation-status: (string-ascii 20),
    verified-legitimate: bool
})

(define-map authorized-adjusters principal bool)
(define-map rehabilitation-providers principal {
    name: (string-ascii 64),
    services: (list 10 (string-ascii 32)),
    rate-per-hour: uint,
    authorized: bool
})

;; Public functions

;; Purchase insurance policy
(define-public (purchase-policy (coverage-type (string-ascii 32)) (coverage-limit uint))
    (let (
        (policy-id (+ (var-get policy-counter) u1))
        (risk-premium (calculate-risk-premium tx-sender coverage-limit))
        (policy-end (+ burn-block-height POLICY-DURATION))
    )
        (asserts! (<= coverage-limit MAX-COVERAGE-PER-INCIDENT) ERR-INSUFFICIENT-COVERAGE)
        (try! (stx-transfer? risk-premium tx-sender CONTRACT-OWNER))
        
        ;; Add premium to insurance pool
        (var-set insurance-pool (+ (var-get insurance-pool) risk-premium))
        
        ;; Create policy
        (map-set insurance-policies policy-id {
            id: policy-id,
            policyholder: tx-sender,
            coverage-type: coverage-type,
            premium-paid: risk-premium,
            coverage-limit: coverage-limit,
            policy-start: burn-block-height,
            policy-end: policy-end,
            claims-made: u0,
            status: "active",
            risk-score: u100
        })
        
        (var-set policy-counter policy-id)
        (ok policy-id)
    )
)

;; File insurance claim
(define-public (file-claim 
    (policy-id uint)
    (incident-type (string-ascii 32))
    (severity uint)
    (simulation-id uint)
    (session-id uint)
    (time-lost uint)
    (medical-costs uint)
    (evidence-hash (buff 32))
)
    (let (
        (claim-id (+ (var-get claim-counter) u1))
        (policy (unwrap! (map-get? insurance-policies policy-id) ERR-INVALID-CLAIM))
        (claim-amount (calculate-claim-amount incident-type severity time-lost medical-costs))
    )
        (asserts! (is-eq tx-sender (get policyholder policy)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status policy) "active") ERR-POLICY-NOT-ACTIVE)
        (asserts! (< burn-block-height (get policy-end policy)) ERR-POLICY-NOT-ACTIVE)
        (asserts! (<= claim-amount (get coverage-limit policy)) ERR-INSUFFICIENT-COVERAGE)
        (asserts! (<= severity u4) ERR-INVALID-CLAIM) ;; Severity 1-4
        
        ;; Create claim record
        (map-set insurance-claims claim-id {
            id: claim-id,
            policy-id: policy-id,
            claimant: tx-sender,
            incident-type: incident-type,
            severity: severity,
            simulation-id: simulation-id,
            session-id: session-id,
            time-lost: time-lost,
            medical-costs: medical-costs,
            claim-amount: claim-amount,
            status: "pending",
            filed-at: burn-block-height,
            processed-at: none,
            approved-amount: u0
        })
        
        ;; Store evidence
        (map-set claim-evidence claim-id {
            claim-id: claim-id,
            evidence-type: "primary",
            evidence-hash: evidence-hash,
            submitted-at: burn-block-height,
            verified: false
        })
        
        ;; Update policy claims count
        (map-set insurance-policies policy-id
            (merge policy { claims-made: (+ (get claims-made policy) u1) })
        )
        
        ;; Run initial fraud detection
        (try! (run-fraud-detection claim-id))
        
        (var-set claim-counter claim-id)
        (ok claim-id)
    )
)

;; Process insurance claim (adjusters only)
(define-public (process-claim (claim-id uint) (approved bool) (approved-amount uint))
    (let (
        (claim (unwrap! (map-get? insurance-claims claim-id) ERR-CLAIM-NOT-FOUND))
        (policy-id (get policy-id claim))
        (policy (unwrap! (map-get? insurance-policies policy-id) ERR-INVALID-CLAIM))
    )
        (asserts! (default-to false (map-get? authorized-adjusters tx-sender)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status claim) "pending") ERR-CLAIM-ALREADY-PROCESSED)
        
        (if approved
            (begin
                (asserts! (<= approved-amount (get coverage-limit policy)) ERR-INSUFFICIENT-COVERAGE)
                (asserts! (<= approved-amount (var-get insurance-pool)) ERR-INSUFFICIENT-FUNDS)
                
                ;; Transfer payment to claimant
                (try! (as-contract (stx-transfer? approved-amount tx-sender (get claimant claim))))
                
                ;; Update insurance pool
                (var-set insurance-pool (- (var-get insurance-pool) approved-amount))
                (var-set total-claims-paid (+ (var-get total-claims-paid) approved-amount))
                
                ;; Update claim status
                (map-set insurance-claims claim-id
                    (merge claim {
                        status: "approved",
                        processed-at: (some burn-block-height),
                        approved-amount: approved-amount
                    })
                )
            )
            (begin
                ;; Claim denied
                (map-set insurance-claims claim-id
                    (merge claim {
                        status: "denied",
                        processed-at: (some burn-block-height),
                        approved-amount: u0
                    })
                )
            )
        )
        
        (ok approved)
    )
)

;; Calculate claim amount based on incident details
(define-private (calculate-claim-amount 
    (incident-type (string-ascii 32))
    (severity uint)
    (time-lost uint)
    (medical-costs uint)
)
    (let (
        (base-amount (if (is-eq incident-type "consciousness-entrapment")
                        CONSCIOUSNESS-RESTORATION-RATE
                        (if (is-eq incident-type "simulation-breakdown")
                            IMMEDIATE-RESPONSE-RATE
                            (if (is-eq incident-type "time-lost")
                                (* time-lost LOST-TIME-COMPENSATION-RATE)
                                RECOVERY-SUPPORT-RATE
                            )
                        )
                     ))
        (severity-multiplier (if (is-eq severity u1) MINOR-INCIDENT-MULTIPLIER
                                (if (is-eq severity u2) MODERATE-INCIDENT-MULTIPLIER
                                    (if (is-eq severity u3) SEVERE-INCIDENT-MULTIPLIER
                                        CRITICAL-INCIDENT-MULTIPLIER
                                    )
                                )
                             ))
        (multiplied-amount (/ (* base-amount severity-multiplier) u100))
    )
        (+ multiplied-amount medical-costs)
    )
)

;; Calculate risk-based premium
(define-private (calculate-risk-premium (user principal) (coverage-limit uint))
    (let (
        (base-premium BASE-PREMIUM)
        (coverage-factor (/ (* coverage-limit u100) MAX-COVERAGE-PER-INCIDENT))
        (user-risk-factor u100) ;; Could be enhanced with user history
    )
        (/ (* base-premium (+ coverage-factor user-risk-factor)) u100)
    )
)

;; Run fraud detection algorithms
(define-private (run-fraud-detection (claim-id uint))
    (let (
        (claim (unwrap! (map-get? insurance-claims claim-id) ERR-CLAIM-NOT-FOUND))
        (suspicious-patterns (list))
        (risk-score u0)
    )
        ;; Basic fraud detection - could be enhanced
        (map-set fraud-detection claim-id {
            claim-id: claim-id,
            suspicious-patterns: suspicious-patterns,
            risk-indicators: risk-score,
            investigation-status: "pending",
            verified-legitimate: false
        })
        (ok true)
    )
)

;; Renew insurance policy
(define-public (renew-policy (policy-id uint))
    (let (
        (policy (unwrap! (map-get? insurance-policies policy-id) ERR-INVALID-CLAIM))
        (new-premium (calculate-risk-premium tx-sender (get coverage-limit policy)))
    )
        (asserts! (is-eq tx-sender (get policyholder policy)) ERR-NOT-AUTHORIZED)
        (try! (stx-transfer? new-premium tx-sender CONTRACT-OWNER))
        
        ;; Add premium to pool
        (var-set insurance-pool (+ (var-get insurance-pool) new-premium))
        
        ;; Update policy
        (map-set insurance-policies policy-id
            (merge policy {
                policy-end: (+ burn-block-height POLICY-DURATION),
                premium-paid: (+ (get premium-paid policy) new-premium),
                status: "active"
            })
        )
        
        (ok true)
    )
)

;; Emergency claim processing for critical incidents
(define-public (process-emergency-claim (claim-id uint))
    (let (
        (claim (unwrap! (map-get? insurance-claims claim-id) ERR-CLAIM-NOT-FOUND))
        (policy-id (get policy-id claim))
        (policy (unwrap! (map-get? insurance-policies policy-id) ERR-INVALID-CLAIM))
        (emergency-amount (/ (get claim-amount claim) u2)) ;; 50% immediate payment
    )
        (asserts! (default-to false (map-get? authorized-adjusters tx-sender)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status claim) "pending") ERR-CLAIM-ALREADY-PROCESSED)
        (asserts! (>= (get severity claim) u3) ERR-INVALID-CLAIM) ;; Only severe/critical
        
        ;; Emergency payment
        (try! (as-contract (stx-transfer? emergency-amount tx-sender (get claimant claim))))
        
        ;; Update pools and status
        (var-set insurance-pool (- (var-get insurance-pool) emergency-amount))
        (var-set total-claims-paid (+ (var-get total-claims-paid) emergency-amount))
        
        (map-set insurance-claims claim-id
            (merge claim {
                status: "emergency-processed",
                approved-amount: emergency-amount
            })
        )
        
        (ok emergency-amount)
    )
)

;; Administrative functions

;; Authorize claims adjuster
(define-public (authorize-adjuster (adjuster principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set authorized-adjusters adjuster true)
        (ok true)
    )
)

;; Add rehabilitation provider
(define-public (add-rehabilitation-provider 
    (provider principal)
    (name (string-ascii 64))
    (services (list 10 (string-ascii 32)))
    (rate-per-hour uint)
)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set rehabilitation-providers provider {
            name: name,
            services: services,
            rate-per-hour: rate-per-hour,
            authorized: true
        })
        (ok true)
    )
)

;; Fund insurance pool
(define-public (fund-pool (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
        (var-set insurance-pool (+ (var-get insurance-pool) amount))
        (ok true)
    )
)

;; Read-only functions

;; Get policy information
(define-read-only (get-policy-info (policy-id uint))
    (map-get? insurance-policies policy-id)
)

;; Get claim information
(define-read-only (get-claim-info (claim-id uint))
    (map-get? insurance-claims claim-id)
)

;; Get claim evidence
(define-read-only (get-claim-evidence (claim-id uint))
    (map-get? claim-evidence claim-id)
)

;; Get fraud detection info
(define-read-only (get-fraud-detection (claim-id uint))
    (map-get? fraud-detection claim-id)
)

;; Check if adjuster is authorized
(define-read-only (is-authorized-adjuster (adjuster principal))
    (default-to false (map-get? authorized-adjusters adjuster))
)

;; Get rehabilitation provider info
(define-read-only (get-rehabilitation-provider (provider principal))
    (map-get? rehabilitation-providers provider)
)

;; Get insurance pool balance
(define-read-only (get-insurance-pool-balance)
    (var-get insurance-pool)
)

;; Get total claims paid
(define-read-only (get-total-claims-paid)
    (var-get total-claims-paid)
)

;; Get policy counter
(define-read-only (get-policy-counter)
    (var-get policy-counter)
)

;; Get claim counter
(define-read-only (get-claim-counter)
    (var-get claim-counter)
)

;; Check if policy is active
(define-read-only (is-policy-active (policy-id uint))
    (match (map-get? insurance-policies policy-id)
        policy
        (and (is-eq (get status policy) "active")
             (< burn-block-height (get policy-end policy)))
        false
    )
)

;; Calculate coverage remaining
(define-read-only (get-coverage-remaining (policy-id uint))
    (match (map-get? insurance-policies policy-id)
        policy
        (let (
            (total-claims-amount (get-total-claims-for-policy policy-id))
        )
            (if (> (get coverage-limit policy) total-claims-amount)
                (- (get coverage-limit policy) total-claims-amount)
                u0
            )
        )
        u0
    )
)

;; Helper function to get total claims amount for a policy
(define-private (get-total-claims-for-policy (policy-id uint))
    ;; This would need to iterate through claims - simplified for now
    u0
)
