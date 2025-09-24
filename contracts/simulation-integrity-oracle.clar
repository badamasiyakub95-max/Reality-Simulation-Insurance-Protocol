;; Reality Simulation Stability Monitoring and Virtual Environment Integrity Tracking
;; This contract monitors simulation environment stability and triggers alerts for integrity violations

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-SIMULATION (err u101))
(define-constant ERR-INVALID-METRICS (err u102))
(define-constant ERR-SIMULATION-NOT-FOUND (err u103))
(define-constant ERR-THRESHOLD-EXCEEDED (err u104))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u105))

;; Safety thresholds (as percentages multiplied by 100)
(define-constant PHYSICS-DEVIATION-THRESHOLD u10) ;; 0.1% = 10/10000
(define-constant TEMPORAL-FLOW-THRESHOLD u500) ;; 5% = 500/10000
(define-constant REALITY-COHERENCE-MIN u8000) ;; 80% = 8000/10000
(define-constant MEMORY-CORRUPTION-MAX u5) ;; Maximum 5 corruption events

;; Data variables
(define-data-var simulation-counter uint u0)
(define-data-var monitoring-fee uint u1000000) ;; 1 STX in micro-STX

;; Data maps
(define-map simulation-registry uint {
    id: uint,
    owner: principal,
    name: (string-ascii 64),
    status: (string-ascii 20),
    created-at: uint,
    last-updated: uint
})

(define-map simulation-metrics uint {
    physics-deviation: uint,
    temporal-flow-rate: uint,
    reality-coherence: uint,
    memory-corruption-count: uint,
    session-count: uint,
    total-runtime: uint
})

(define-map baseline-parameters uint {
    gravity-constant: uint,
    time-flow-rate: uint,
    physics-engine-version: uint,
    reality-baseline: uint
})

(define-map alert-history uint {
    simulation-id: uint,
    alert-type: (string-ascii 32),
    severity: uint,
    timestamp: uint,
    resolved: bool
})

(define-map authorized-monitors principal bool)

;; Public functions

;; Register a new simulation environment
(define-public (register-simulation (name (string-ascii 64)))
    (let (
        (simulation-id (+ (var-get simulation-counter) u1))
        (current-block-height burn-block-height)
    )
        (asserts! (> (len name) u0) ERR-INVALID-SIMULATION)
        (try! (stx-transfer? (var-get monitoring-fee) tx-sender CONTRACT-OWNER))
        (map-set simulation-registry simulation-id {
            id: simulation-id,
            owner: tx-sender,
            name: name,
            status: "active",
            created-at: current-block-height,
            last-updated: current-block-height
        })
        (map-set simulation-metrics simulation-id {
            physics-deviation: u0,
            temporal-flow-rate: u10000, ;; 100% normal rate
            reality-coherence: u10000, ;; 100% coherence
            memory-corruption-count: u0,
            session-count: u0,
            total-runtime: u0
        })
        (map-set baseline-parameters simulation-id {
            gravity-constant: u982, ;; 9.82 m/s^2 * 100
            time-flow-rate: u10000, ;; 100% normal time
            physics-engine-version: u100,
            reality-baseline: u10000
        })
        (var-set simulation-counter simulation-id)
        (ok simulation-id)
    )
)

;; Update simulation metrics (only by authorized monitors or owner)
(define-public (update-simulation-metrics 
    (simulation-id uint)
    (physics-deviation uint)
    (temporal-flow-rate uint)
    (reality-coherence uint)
    (memory-corruption-count uint)
)
    (let (
        (simulation (unwrap! (map-get? simulation-registry simulation-id) ERR-SIMULATION-NOT-FOUND))
        (is-owner (is-eq tx-sender (get owner simulation)))
        (is-authorized (default-to false (map-get? authorized-monitors tx-sender)))
    )
        (asserts! (or is-owner is-authorized) ERR-NOT-AUTHORIZED)
        (asserts! (<= physics-deviation u10000) ERR-INVALID-METRICS)
        (asserts! (<= temporal-flow-rate u50000) ERR-INVALID-METRICS)
        (asserts! (<= reality-coherence u10000) ERR-INVALID-METRICS)
        
        ;; Update metrics
        (map-set simulation-metrics simulation-id {
            physics-deviation: physics-deviation,
            temporal-flow-rate: temporal-flow-rate,
            reality-coherence: reality-coherence,
            memory-corruption-count: memory-corruption-count,
            session-count: (+ (get session-count (unwrap-panic (map-get? simulation-metrics simulation-id))) u1),
            total-runtime: (+ (get total-runtime (unwrap-panic (map-get? simulation-metrics simulation-id))) u1)
        })
        
        ;; Update simulation registry
        (map-set simulation-registry simulation-id 
            (merge simulation { last-updated: burn-block-height })
        )
        
        ;; Check for threshold violations
        (unwrap-panic (check-safety-thresholds simulation-id physics-deviation temporal-flow-rate reality-coherence memory-corruption-count))
        
        (ok true)
    )
)

;; Check safety thresholds and create alerts if needed
(define-private (check-safety-thresholds 
    (simulation-id uint)
    (physics-deviation uint)
    (temporal-flow-rate uint)
    (reality-coherence uint)
    (memory-corruption-count uint)
)
    (begin
        ;; Physics deviation check
        (if (> physics-deviation PHYSICS-DEVIATION-THRESHOLD)
            (begin
                (unwrap-panic (create-alert simulation-id "physics-violation" u3 burn-block-height))
                true
            )
            true
        )
        
        ;; Temporal flow rate check (deviation from normal 100%)
        (if (or (< temporal-flow-rate (- u10000 TEMPORAL-FLOW-THRESHOLD))
                (> temporal-flow-rate (+ u10000 TEMPORAL-FLOW-THRESHOLD)))
            (begin
                (unwrap-panic (create-alert simulation-id "temporal-anomaly" u2 burn-block-height))
                true
            )
            true
        )
        
        ;; Reality coherence check
        (if (< reality-coherence REALITY-COHERENCE-MIN)
            (begin
                (unwrap-panic (create-alert simulation-id "coherence-failure" u4 burn-block-height))
                true
            )
            true
        )
        
        ;; Memory corruption check
        (if (> memory-corruption-count MEMORY-CORRUPTION-MAX)
            (begin
                (unwrap-panic (create-alert simulation-id "memory-corruption" u5 burn-block-height))
                true
            )
            true
        )
        
        (ok true)
    )
)

;; Create an alert for threshold violations
(define-private (create-alert (simulation-id uint) (alert-type (string-ascii 32)) (severity uint) (timestamp uint))
    (let (
        (alert-id (+ (* simulation-id u1000) severity timestamp))
    )
        (map-set alert-history alert-id {
            simulation-id: simulation-id,
            alert-type: alert-type,
            severity: severity,
            timestamp: timestamp,
            resolved: false
        })
        (ok alert-id)
    )
)

;; Resolve an alert (mark as resolved)
(define-public (resolve-alert (alert-id uint))
    (let (
        (alert (unwrap! (map-get? alert-history alert-id) ERR-SIMULATION-NOT-FOUND))
        (simulation-id (get simulation-id alert))
        (simulation (unwrap! (map-get? simulation-registry simulation-id) ERR-SIMULATION-NOT-FOUND))
    )
        (asserts! (or (is-eq tx-sender (get owner simulation))
                     (default-to false (map-get? authorized-monitors tx-sender)))
                 ERR-NOT-AUTHORIZED)
        (map-set alert-history alert-id (merge alert { resolved: true }))
        (ok true)
    )
)

;; Deactivate a simulation
(define-public (deactivate-simulation (simulation-id uint))
    (let (
        (simulation (unwrap! (map-get? simulation-registry simulation-id) ERR-SIMULATION-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get owner simulation)) ERR-NOT-AUTHORIZED)
        (map-set simulation-registry simulation-id 
            (merge simulation { 
                status: "inactive",
                last-updated: burn-block-height
            })
        )
        (ok true)
    )
)

;; Authorize a monitor (admin only)
(define-public (authorize-monitor (monitor principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set authorized-monitors monitor true)
        (ok true)
    )
)

;; Remove monitor authorization (admin only)
(define-public (revoke-monitor (monitor principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set authorized-monitors monitor false)
        (ok true)
    )
)

;; Update monitoring fee (admin only)
(define-public (set-monitoring-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set monitoring-fee new-fee)
        (ok true)
    )
)

;; Read-only functions

;; Get simulation info
(define-read-only (get-simulation-info (simulation-id uint))
    (map-get? simulation-registry simulation-id)
)

;; Get simulation metrics
(define-read-only (get-simulation-metrics (simulation-id uint))
    (map-get? simulation-metrics simulation-id)
)

;; Get baseline parameters
(define-read-only (get-baseline-parameters (simulation-id uint))
    (map-get? baseline-parameters simulation-id)
)

;; Get alert information
(define-read-only (get-alert-info (alert-id uint))
    (map-get? alert-history alert-id)
)

;; Check if monitor is authorized
(define-read-only (is-authorized-monitor (monitor principal))
    (default-to false (map-get? authorized-monitors monitor))
)

;; Get current monitoring fee
(define-read-only (get-monitoring-fee)
    (var-get monitoring-fee)
)

;; Get simulation counter
(define-read-only (get-simulation-counter)
    (var-get simulation-counter)
)

;; Check if simulation is within safety parameters
(define-read-only (is-simulation-safe (simulation-id uint))
    (match (map-get? simulation-metrics simulation-id)
        metrics
        (let (
            (physics-ok (<= (get physics-deviation metrics) PHYSICS-DEVIATION-THRESHOLD))
            (temporal-ok (and (>= (get temporal-flow-rate metrics) (- u10000 TEMPORAL-FLOW-THRESHOLD))
                             (<= (get temporal-flow-rate metrics) (+ u10000 TEMPORAL-FLOW-THRESHOLD))))
            (coherence-ok (>= (get reality-coherence metrics) REALITY-COHERENCE-MIN))
            (memory-ok (<= (get memory-corruption-count metrics) MEMORY-CORRUPTION-MAX))
        )
            (and physics-ok temporal-ok coherence-ok memory-ok)
        )
        false
    )
)
