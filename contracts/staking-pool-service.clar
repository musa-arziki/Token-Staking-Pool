;; Token Staking Pool Contract
;; A production-ready staking contract with fixed rewards and extensible architecture

;; ===========================================
;; CONSTANTS AND ERROR CODES
;; ===========================================

(define-constant CONTRACT_OWNER tx-sender)
(define-constant STAKING_TOKEN 'SP1H1733V5MZ3SZ9XRW9FKYGEZT0JDGEB8Y634C7R.miamicoin-token)

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_NO_STAKE_FOUND (err u103))
(define-constant ERR_STAKING_PERIOD_NOT_COMPLETE (err u104))
(define-constant ERR_POOL_NOT_ACTIVE (err u105))
(define-constant ERR_INSUFFICIENT_POOL_BALANCE (err u106))
(define-constant ERR_INVALID_PERIOD (err u107))

;; Staking parameters
(define-constant MIN_STAKE_AMOUNT u1000000) ;; 1 token (assuming 6 decimals)
(define-constant REWARD_RATE u5) ;; 5% annual reward rate (500 basis points)
(define-constant BASIS_POINTS u10000)
(define-constant BLOCKS_PER_YEAR u52560) ;; Approximate blocks per year (10 min blocks)
(define-constant MIN_STAKING_PERIOD u1440) ;; Minimum 1 day (144 blocks * 10)

;; ===========================================
;; DATA STRUCTURES
;; ===========================================

;; Pool configuration
(define-data-var pool-active bool true)
(define-data-var total-staked uint u0)
(define-data-var total-rewards-distributed uint u0)
(define-data-var pool-balance uint u0)

;; User stakes mapping
(define-map stakes
    { staker: principal }
    {
        amount: uint,
        start-block: uint,
        last-claim-block: uint,
        total-claimed: uint
    }
)

;; Staking statistics for users
(define-map user-stats
    { user: principal }
    {
        total-staked: uint,
        total-claimed: uint,
        stakes-count: uint
    }
)

;; ===========================================
;; READ-ONLY FUNCTIONS
;; ===========================================

;; Get stake information for a user
(define-read-only (get-stake (staker principal))
    (map-get? stakes { staker: staker })
)

;; Get user statistics
(define-read-only (get-user-stats (user principal))
    (default-to
        { total-staked: u0, total-claimed: u0, stakes-count: u0 }
        (map-get? user-stats { user: user })
    )
)

;; Calculate pending rewards for a staker
(define-read-only (get-pending-rewards (staker principal))
    (match (map-get? stakes { staker: staker })
        stake-data
        (let (
            (current-block (stacks-block-height))
            (blocks-since-last-claim (- current-block (get last-claim-block stake-data)))
            (stake-amount (get amount stake-data))
        )
            (/ (* (* stake-amount REWARD_RATE) blocks-since-last-claim)
               (* BASIS_POINTS BLOCKS_PER_YEAR))
        )
        u0
    )
)

;; Get total rewards earned (claimed + pending)
(define-read-only (get-total-rewards (staker principal))
    (+ (get-pending-rewards staker)
       (default-to u0
           (get total-claimed (map-get? stakes { staker: staker }))
       )
    )
)

;; Check if user can unstake (minimum period completed)
(define-read-only (can-unstake (staker principal))
    (match (map-get? stakes { staker: staker })
        stake-data
        (>= (- (stacks-block-height) (get start-block stake-data)) MIN_STAKING_PERIOD)
        false
    )
)

;; Get pool information
(define-read-only (get-pool-info)
    {
        active: (var-get pool-active),
        total-staked: (var-get total-staked),
        total-rewards-distributed: (var-get total-rewards-distributed),
        pool-balance: (var-get pool-balance),
        reward-rate: REWARD_RATE,
        min-stake: MIN_STAKE_AMOUNT,
        min-period: MIN_STAKING_PERIOD
    }
)

;; ===========================================
;; PRIVATE FUNCTIONS
;; ===========================================

;; Update user statistics
(define-private (update-user-stats (user principal) (staked-amount uint) (claimed-amount uint))
    (let (
        (current-stats (get-user-stats user))
        (new-total-staked (+ (get total-staked current-stats) staked-amount))
        (new-total-claimed (+ (get total-claimed current-stats) claimed-amount))
        (new-stakes-count (+ (get stakes-count current-stats) u1))
    )
        (map-set user-stats
            { user: user }
            {
                total-staked: new-total-staked,
                total-claimed: new-total-claimed,
                stakes-count: new-stakes-count
            }
        )
    )
)

;; Transfer tokens with error handling
(define-private (safe-token-transfer (amount uint) (sender principal) (recipient principal))
    (contract-call? STAKING_TOKEN transfer amount sender recipient none)
)

;; ===========================================
;; PUBLIC FUNCTIONS
;; ===========================================

;; Stake tokens
(define-public (stake (amount uint))
    (let (
        (staker tx-sender)
        (current-block (stacks-block-height))
    )
        ;; Validation checks
        (asserts! (var-get pool-active) ERR_POOL_NOT_ACTIVE)
        (asserts! (>= amount MIN_STAKE_AMOUNT) ERR_INVALID_AMOUNT)
        (asserts! (is-none (map-get? stakes { staker: staker })) ERR_INVALID_AMOUNT) ;; No existing stake

        ;; Transfer tokens to contract
        (try! (safe-token-transfer amount staker (as-contract tx-sender)))

        ;; Create stake record
        (map-set stakes
            { staker: staker }
            {
                amount: amount,
                start-block: current-block,
                last-claim-block: current-block,
                total-claimed: u0
            }
        )

        ;; Update pool state
        (var-set total-staked (+ (var-get total-staked) amount))

        ;; Update user statistics
        (update-user-stats staker amount u0)

        (ok {
            staker: staker,
            amount: amount,
            start-block: current-block
        })
    )
)

;; Claim rewards without unstaking
(define-public (claim-rewards)
    (let (
        (staker tx-sender)
        (current-block (stacks-block-height))
    )
        (match (map-get? stakes { staker: staker })
            stake-data
            (let (
                (pending-rewards (get-pending-rewards staker))
                (current-claimed (get total-claimed stake-data))
            )
                ;; Check if there are rewards to claim
                (asserts! (> pending-rewards u0) ERR_INVALID_AMOUNT)
                (asserts! (>= (var-get pool-balance) pending-rewards) ERR_INSUFFICIENT_POOL_BALANCE)

                ;; Transfer rewards to staker
                (try! (as-contract (safe-token-transfer pending-rewards tx-sender staker)))

                ;; Update stake record
                (map-set stakes
                    { staker: staker }
                    (merge stake-data {
                        last-claim-block: current-block,
                        total-claimed: (+ current-claimed pending-rewards)
                    })
                )

                ;; Update pool state
                (var-set pool-balance (- (var-get pool-balance) pending-rewards))
                (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) pending-rewards))

                ;; Update user statistics
                (update-user-stats staker u0 pending-rewards)

                (ok {
                    staker: staker,
                    rewards-claimed: pending-rewards,
                    total-claimed: (+ current-claimed pending-rewards)
                })
            )
            ERR_NO_STAKE_FOUND
        )
    )
)

;; Unstake tokens (claims rewards automatically)
(define-public (unstake)
    (let (
        (staker tx-sender)
        (current-block (stacks-block-height))
    )
        (match (map-get? stakes { staker: staker })
            stake-data
            (let (
                (stake-amount (get amount stake-data))
                (pending-rewards (get-pending-rewards staker))
                (total-payout (+ stake-amount pending-rewards))
            )
                ;; Check minimum staking period
                (asserts! (can-unstake staker) ERR_STAKING_PERIOD_NOT_COMPLETE)
                (asserts! (>= (var-get pool-balance) pending-rewards) ERR_INSUFFICIENT_POOL_BALANCE)

                ;; Transfer stake + rewards back to staker
                (try! (as-contract (safe-token-transfer stake-amount tx-sender staker)))
                (if (> pending-rewards u0)
                    (try! (as-contract (safe-token-transfer pending-rewards tx-sender staker)))
                    true
                )

                ;; Remove stake record
                (map-delete stakes { staker: staker })

                ;; Update pool state
                (var-set total-staked (- (var-get total-staked) stake-amount))
                (var-set pool-balance (- (var-get pool-balance) pending-rewards))
                (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) pending-rewards))

                (ok {
                    staker: staker,
                    stake-returned: stake-amount,
                    rewards-claimed: pending-rewards,
                    total-payout: total-payout
                })
            )
            ERR_NO_STAKE_FOUND
        )
    )
)

;; ===========================================
;; ADMIN FUNCTIONS
;; ===========================================

;; Add rewards to the pool (only contract owner)
(define-public (fund-pool (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (try! (safe-token-transfer amount tx-sender (as-contract tx-sender)))
        (var-set pool-balance (+ (var-get pool-balance) amount))
        (ok { amount-added: amount, new-balance: (var-get pool-balance) })
    )
)

;; Toggle pool active status (only contract owner)
(define-public (toggle-pool-status)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set pool-active (not (var-get pool-active)))
        (ok { active: (var-get pool-active) })
    )
)

;; Emergency withdraw (only contract owner, when pool is inactive)
(define-public (emergency-withdraw (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (not (var-get pool-active)) ERR_POOL_NOT_ACTIVE)
        (asserts! (<= amount (var-get pool-balance)) ERR_INSUFFICIENT_POOL_BALANCE)

        (try! (as-contract (safe-token-transfer amount tx-sender CONTRACT_OWNER)))
        (var-set pool-balance (- (var-get pool-balance) amount))
        (ok { withdrawn: amount })
    )
)
