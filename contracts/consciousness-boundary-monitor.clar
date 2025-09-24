;; Virtual-Reality Consciousness Boundary Monitoring and Entrapment Prevention
;; This contract prevents and responds to consciousness entrapment scenarios in simulation environments

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-INVALID-USER (err u201))
(define-constant ERR-INVALID-SESSION (err u202))
(define-constant ERR-SESSION-NOT-FOUND (err u203))
(define-constant ERR-ENTRAPMENT-DETECTED (err u204))
(define-constant ERR-EMERGENCY-PROTOCOL-FAILED (err u205))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u206))

;; Safety parameters and thresholds
(define-constant MAX-EXIT-ATTEMPTS u3) ;; Maximum failed exit attempts before emergency
(define-constant CRITICAL-ENGAGEMENT-THRESHOLD u9500) ;; 95% engagement = critical
(define-constant IDENTITY-COHERENCE-MIN u7500) ;; 75% minimum identity coherence
(define-constant MAX-SESSION-DURATION u14400) ;; 4 hours in blocks (assuming 10 second blocks)
(define-constant EMERGENCY-COOLDOWN u144) ;; 24 minutes cooldown between emergency extractions

;; Data variables
(define-data-var session-counter uint u0)
(define-data-var monitoring-fee uint u500000) ;; 0.5 STX in micro-STX
(define-data-var emergency-fund uint u0)

;; Data maps
(define-map consciousness-sessions uint {
    id: uint,
    user: principal,
    simulation-id: uint,
    status: (string-ascii 20),
    engagement-level: uint,
    identity-coherence: uint,
    exit-attempts: uint,
    session-start: uint,
    last-heartbeat: uint,
    emergency-extractions: uint
})

(define-map user-profiles principal {
    total-sessions: uint,
    successful-exits: uint,
    failed-exits: uint,
    emergency-extractions: uint,
    consciousness-stability: uint,
    last-session-id: uint,
    risk-score: uint
})

(define-map exit-attempt-log uint {
    session-id: uint,
    attempt-number: uint,
    timestamp: uint,
    method-used: (string-ascii 32),
    success: bool,
    consciousness-state: uint
})

(define-map emergency-protocols uint {
    session-id: uint,
    trigger-reason: (string-ascii 64),
    initiated-at: uint,
    completed-at: (optional uint),
    success: bool,
    recovery-time: uint
})

(define-map authorized-extractors principal bool)
(define-map simulation-operators principal bool)

;; Public functions

;; Initialize a new consciousness monitoring session
(define-public (start-session (simulation-id uint) (user principal))
    (let (
        (session-id (+ (var-get session-counter) u1))
        (current-block burn-block-height)
        (user-profile (default-to { 
            total-sessions: u0, 
            successful-exits: u0, 
            failed-exits: u0, 
            emergency-extractions: u0, 
            consciousness-stability: u10000, 
            last-session-id: u0, 
            risk-score: u0 
        } (map-get? user-profiles user)))
    )
        (asserts! (or (is-eq tx-sender user) 
                     (default-to false (map-get? simulation-operators tx-sender)))
                 ERR-NOT-AUTHORIZED)
        (try! (stx-transfer? (var-get monitoring-fee) tx-sender CONTRACT-OWNER))
        
        ;; Create session record
        (map-set consciousness-sessions session-id {
            id: session-id,
            user: user,
            simulation-id: simulation-id,
            status: "active",
            engagement-level: u5000, ;; Start at 50% engagement
            identity-coherence: u10000, ;; Start at 100% coherence
            exit-attempts: u0,
            session-start: current-block,
            last-heartbeat: current-block,
            emergency-extractions: u0
        })
        
        ;; Update user profile
        (map-set user-profiles user 
            (merge user-profile {
                total-sessions: (+ (get total-sessions user-profile) u1),
                last-session-id: session-id
            })
        )
        
        (var-set session-counter session-id)
        (ok session-id)
    )
)

;; Update consciousness metrics during simulation session
(define-public (update-consciousness-state
    (session-id uint)
    (engagement-level uint)
    (identity-coherence uint)
)
    (let (
        (session (unwrap! (map-get? consciousness-sessions session-id) ERR-SESSION-NOT-FOUND))
        (is-user (is-eq tx-sender (get user session)))
        (is-operator (default-to false (map-get? simulation-operators tx-sender)))
    )
        (asserts! (or is-user is-operator) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status session) "active") ERR-INVALID-SESSION)
        (asserts! (<= engagement-level u10000) ERR-INVALID-SESSION)
        (asserts! (<= identity-coherence u10000) ERR-INVALID-SESSION)
        
        ;; Check for dangerous conditions
        (if (>= engagement-level CRITICAL-ENGAGEMENT-THRESHOLD)
            (begin
                (try! (trigger-entrapment-alert session-id "high-engagement"))
                true
            )
            true
        )
        
        (if (< identity-coherence IDENTITY-COHERENCE-MIN)
            (begin
                (try! (trigger-entrapment-alert session-id "identity-degradation"))
                true
            )
            true
        )
        
        ;; Check session duration
        (if (> (- burn-block-height (get session-start session)) MAX-SESSION-DURATION)
            (begin
                (try! (trigger-entrapment-alert session-id "duration-exceeded"))
                true
            )
            true
        )
        
        ;; Update session data
        (map-set consciousness-sessions session-id
            (merge session {
                engagement-level: engagement-level,
                identity-coherence: identity-coherence,
                last-heartbeat: burn-block-height
            })
        )
        
        (ok true)
    )
)

;; Attempt to exit simulation
(define-public (attempt-exit (session-id uint) (exit-method (string-ascii 32)))
    (let (
        (session (unwrap! (map-get? consciousness-sessions session-id) ERR-SESSION-NOT-FOUND))
        (current-attempts (get exit-attempts session))
        (log-id (+ (* session-id u1000) current-attempts u1))
    )
        (asserts! (is-eq tx-sender (get user session)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status session) "active") ERR-INVALID-SESSION)
        
        ;; Log the exit attempt
        (map-set exit-attempt-log log-id {
            session-id: session-id,
            attempt-number: (+ current-attempts u1),
            timestamp: burn-block-height,
            method-used: exit-method,
            success: false, ;; Will be updated if successful
            consciousness-state: (get engagement-level session)
        })
        
        ;; Update session with new attempt count
        (map-set consciousness-sessions session-id
            (merge session { exit-attempts: (+ current-attempts u1) })
        )
        
        ;; Check if max attempts exceeded
        (if (>= (+ current-attempts u1) MAX-EXIT-ATTEMPTS)
            (begin
                (try! (initiate-emergency-extraction session-id "max-attempts-exceeded"))
                (ok false)
            )
            (ok true)
        )
    )
)

;; Successful exit from simulation
(define-public (complete-exit (session-id uint))
    (let (
        (session (unwrap! (map-get? consciousness-sessions session-id) ERR-SESSION-NOT-FOUND))
        (user (get user session))
        (user-profile (unwrap-panic (map-get? user-profiles user)))
        (exit-attempts (get exit-attempts session))
        (log-id (+ (* session-id u1000) exit-attempts))
    )
        (asserts! (or (is-eq tx-sender user)
                     (default-to false (map-get? simulation-operators tx-sender)))
                 ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status session) "active") ERR-INVALID-SESSION)
        
        ;; Update exit attempt log if there were attempts
        (if (> exit-attempts u0)
            (map-set exit-attempt-log log-id
                (merge (unwrap-panic (map-get? exit-attempt-log log-id)) { success: true })
            )
            true
        )
        
        ;; Close session
        (map-set consciousness-sessions session-id
            (merge session { status: "completed" })
        )
        
        ;; Update user profile
        (map-set user-profiles user
            (merge user-profile {
                successful-exits: (+ (get successful-exits user-profile) u1)
            })
        )
        
        (ok true)
    )
)

;; Emergency extraction protocol (can be triggered automatically or manually)
(define-private (initiate-emergency-extraction (session-id uint) (reason (string-ascii 64)))
    (let (
        (session (unwrap! (map-get? consciousness-sessions session-id) ERR-SESSION-NOT-FOUND))
        (protocol-id (+ (* session-id u10000) burn-block-height))
    )
        ;; Record emergency protocol
        (map-set emergency-protocols protocol-id {
            session-id: session-id,
            trigger-reason: reason,
            initiated-at: burn-block-height,
            completed-at: none,
            success: false,
            recovery-time: u0
        })
        
        ;; Mark session as emergency
        (map-set consciousness-sessions session-id
            (merge session {
                status: "emergency-extraction",
                emergency-extractions: (+ (get emergency-extractions session) u1)
            })
        )
        
        (ok protocol-id)
    )
)

;; Complete emergency extraction (called by authorized extractors)
(define-public (complete-emergency-extraction (protocol-id uint) (success bool))
    (let (
        (protocol (unwrap! (map-get? emergency-protocols protocol-id) ERR-SESSION-NOT-FOUND))
        (session-id (get session-id protocol))
        (session (unwrap! (map-get? consciousness-sessions session-id) ERR-SESSION-NOT-FOUND))
        (user (get user session))
        (user-profile (unwrap-panic (map-get? user-profiles user)))
        (recovery-time (- burn-block-height (get initiated-at protocol)))
    )
        (asserts! (default-to false (map-get? authorized-extractors tx-sender)) ERR-NOT-AUTHORIZED)
        
        ;; Update protocol record
        (map-set emergency-protocols protocol-id
            (merge protocol {
                completed-at: (some burn-block-height),
                success: success,
                recovery-time: recovery-time
            })
        )
        
        ;; Update session status
        (map-set consciousness-sessions session-id
            (merge session { 
                status: (if success "emergency-completed" "emergency-failed")
            })
        )
        
        ;; Update user profile
        (map-set user-profiles user
            (merge user-profile {
                emergency-extractions: (+ (get emergency-extractions user-profile) u1),
                successful-exits: (if success (+ (get successful-exits user-profile) u1) (get successful-exits user-profile)),
                failed-exits: (if (not success) (+ (get failed-exits user-profile) u1) (get failed-exits user-profile)),
                risk-score: (if success 
                              (get risk-score user-profile)
                              (+ (get risk-score user-profile) u100))
            })
        )
        
        (ok true)
    )
)

;; Trigger entrapment alert (internal function)
(define-private (trigger-entrapment-alert (session-id uint) (alert-reason (string-ascii 64)))
    (let (
        (session (unwrap! (map-get? consciousness-sessions session-id) ERR-SESSION-NOT-FOUND))
    )
        ;; If engagement is critical or identity coherence is low, initiate emergency extraction
        (if (or (>= (get engagement-level session) CRITICAL-ENGAGEMENT-THRESHOLD)
                (< (get identity-coherence session) IDENTITY-COHERENCE-MIN))
            (begin
                (try! (initiate-emergency-extraction session-id alert-reason))
                true
            )
            true
        )
        (ok true)
    )
)

;; Administrative functions

;; Authorize emergency extractor
(define-public (authorize-extractor (extractor principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set authorized-extractors extractor true)
        (ok true)
    )
)

;; Authorize simulation operator
(define-public (authorize-simulation-operator (operator principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set simulation-operators operator true)
        (ok true)
    )
)

;; Update monitoring fee
(define-public (set-monitoring-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set monitoring-fee new-fee)
        (ok true)
    )
)

;; Read-only functions

;; Get session information
(define-read-only (get-session-info (session-id uint))
    (map-get? consciousness-sessions session-id)
)

;; Get user profile
(define-read-only (get-user-profile (user principal))
    (map-get? user-profiles user)
)

;; Get exit attempt log
(define-read-only (get-exit-attempt (log-id uint))
    (map-get? exit-attempt-log log-id)
)

;; Get emergency protocol
(define-read-only (get-emergency-protocol (protocol-id uint))
    (map-get? emergency-protocols protocol-id)
)

;; Check if session is at risk of entrapment
(define-read-only (is-session-at-risk (session-id uint))
    (match (map-get? consciousness-sessions session-id)
        session
        (let (
            (high-engagement (>= (get engagement-level session) CRITICAL-ENGAGEMENT-THRESHOLD))
            (low-coherence (< (get identity-coherence session) IDENTITY-COHERENCE-MIN))
            (too-many-attempts (>= (get exit-attempts session) MAX-EXIT-ATTEMPTS))
            (session-too-long (> (- burn-block-height (get session-start session)) MAX-SESSION-DURATION))
        )
            (or high-engagement low-coherence too-many-attempts session-too-long)
        )
        false
    )
)

;; Get session counter
(define-read-only (get-session-counter)
    (var-get session-counter)
)

;; Get monitoring fee
(define-read-only (get-monitoring-fee)
    (var-get monitoring-fee)
)

;; Check authorization status
(define-read-only (is-authorized-extractor (extractor principal))
    (default-to false (map-get? authorized-extractors extractor))
)

(define-read-only (is-authorized-operator (operator principal))
    (default-to false (map-get? simulation-operators operator))
)
