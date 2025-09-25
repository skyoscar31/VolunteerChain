;; VolunteerChain: Community Volunteer Service Tracking and Reward System
;; Version: 1.0.0

;; Constants
(define-constant VOLUNTEER_INCENTIVE_CAPACITY u2800000)
(define-constant BASE_VOLUNTEER_REWARD u20)
(define-constant COMMUNITY_BONUS u7)
(define-constant MAX_VOLUNTEER_LEVEL u10)
(define-constant ERR_INVALID_SERVICE_USAGE u1)
(define-constant ERR_NO_VOLUNTEER_POINTS u2)
(define-constant ERR_INCENTIVE_EXCEEDED u3)
(define-constant BLOCKS_PER_SERVICE_CYCLE u1440)
(define-constant PROJECT_OPTIMIZATION_MULTIPLIER u3)
(define-constant MIN_OPTIMIZATION_PERIOD u720)
(define-constant EARLY_OPTIMIZATION_PENALTY u12)

;; Data Variables
(define-data-var total-volunteer-points-awarded uint u0)
(define-data-var total-service-hours uint u0)
(define-data-var community-coordinator principal tx-sender)

;; Data Maps
(define-map volunteer-services principal uint)
(define-map volunteer-points principal uint)
(define-map service-start-time principal uint)
(define-map volunteer-level principal uint)
(define-map volunteer-last-service principal uint)
(define-map volunteer-optimized-projects principal uint)
(define-map volunteer-optimization-start-block principal uint)

;; Public Functions
(define-public (start-volunteer-service (service-hours uint))
  (let
    (
      (volunteer tx-sender)
    )
    (asserts! (> service-hours u0) (err ERR_INVALID_SERVICE_USAGE))
    (map-set service-start-time volunteer burn-block-height)
    (ok true)
  ))

(define-public (complete-volunteer-service (service-hours uint))
  (let
    (
      (volunteer tx-sender)
      (start-block (default-to u0 (map-get? service-start-time volunteer)))
      (blocks-serving (- burn-block-height start-block))
      (last-service-block (default-to u0 (map-get? volunteer-last-service volunteer)))
      (volunteer-tier (default-to u0 (map-get? volunteer-level volunteer)))
      (capped-tier (if (<= volunteer-tier MAX_VOLUNTEER_LEVEL) volunteer-tier MAX_VOLUNTEER_LEVEL))
      (volunteer-reward (+ BASE_VOLUNTEER_REWARD (* capped-tier COMMUNITY_BONUS)))
    )
    (asserts! (and (> start-block u0) (>= blocks-serving service-hours)) (err ERR_INVALID_SERVICE_USAGE))
    
    (map-set volunteer-services volunteer (+ (default-to u0 (map-get? volunteer-services volunteer)) u1))
    (map-set volunteer-points volunteer (+ (default-to u0 (map-get? volunteer-points volunteer)) volunteer-reward))
    
    (if (< (- burn-block-height last-service-block) BLOCKS_PER_SERVICE_CYCLE)
      (map-set volunteer-level volunteer (+ volunteer-tier u1))
      (map-set volunteer-level volunteer u1)
    )
    
    (map-set volunteer-last-service volunteer burn-block-height)
    (var-set total-service-hours (+ (var-get total-service-hours) u1))
    (var-set total-volunteer-points-awarded (+ (var-get total-volunteer-points-awarded) volunteer-reward))
    
    (asserts! (<= (var-get total-volunteer-points-awarded) VOLUNTEER_INCENTIVE_CAPACITY) (err ERR_INCENTIVE_EXCEEDED))
    (ok volunteer-reward)
  ))

(define-public (claim-volunteer-rewards)
  (let
    (
      (volunteer tx-sender)
      (point-balance (default-to u0 (map-get? volunteer-points volunteer)))
    )
    (asserts! (> point-balance u0) (err ERR_NO_VOLUNTEER_POINTS))
    (map-set volunteer-points volunteer u0)
    (ok point-balance)
  ))

;; Project Optimization Features
(define-public (optimize-community-projects (amount uint))
  (let
    (
      (volunteer tx-sender)
    )
    (asserts! (> amount u0) (err ERR_INVALID_SERVICE_USAGE))
    (asserts! (>= (var-get total-volunteer-points-awarded) amount) (err ERR_INCENTIVE_EXCEEDED))
    
    (map-set volunteer-optimized-projects volunteer amount)
    (map-set volunteer-optimization-start-block volunteer burn-block-height)
    (var-set total-volunteer-points-awarded (- (var-get total-volunteer-points-awarded) amount))
    (ok amount)
  ))

(define-public (complete-project-optimization)
  (let
    (
      (volunteer tx-sender)
      (optimized-amount (default-to u0 (map-get? volunteer-optimized-projects volunteer)))
      (optimization-start-block (default-to u0 (map-get? volunteer-optimization-start-block volunteer)))
      (blocks-optimized (- burn-block-height optimization-start-block))
      (penalty (if (< blocks-optimized MIN_OPTIMIZATION_PERIOD) (/ (* optimized-amount EARLY_OPTIMIZATION_PENALTY) u100) u0))
      (final-amount (- optimized-amount penalty))
    )
    (asserts! (> optimized-amount u0) (err ERR_NO_VOLUNTEER_POINTS))
    
    (map-set volunteer-optimized-projects volunteer u0)
    (map-set volunteer-optimization-start-block volunteer u0)
    (var-set total-volunteer-points-awarded (+ (var-get total-volunteer-points-awarded) final-amount))
    (ok final-amount)
  ))

;; Read-Only Functions
(define-read-only (get-service-count (user principal))
  (default-to u0 (map-get? volunteer-services user)))

(define-read-only (get-volunteer-point-balance (user principal))
  (default-to u0 (map-get? volunteer-points user)))

(define-read-only (get-volunteer-level (user principal))
  (default-to u0 (map-get? volunteer-level user)))

(define-read-only (get-volunteer-program-stats)
  {
    total-service-hours: (var-get total-service-hours),
    total-volunteer-points-awarded: (var-get total-volunteer-points-awarded)
  })

;; Private Functions
(define-private (is-community-coordinator)
  (is-eq tx-sender (var-get community-coordinator)))