;; Task Bounty Board
;; A decentralized job posting system with token rewards, skill requirements, and reputation tracking

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_TASK_NOT_FOUND (err u101))
(define-constant ERR_TASK_NOT_ACTIVE (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_ALREADY_ASSIGNED (err u104))
(define-constant ERR_NOT_ASSIGNED (err u105))
(define-constant ERR_TASK_EXPIRED (err u106))
(define-constant ERR_INVALID_SKILL_LEVEL (err u107))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u108))
(define-constant ERR_INVALID_DURATION (err u109))
(define-constant ERR_ALREADY_COMPLETED (err u110))

;; Data Variables
(define-data-var next-task-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% in basis points (250/10000)

;; Task Status Constants
(define-constant TASK_STATUS_ACTIVE u1)
(define-constant TASK_STATUS_ASSIGNED u2)
(define-constant TASK_STATUS_COMPLETED u3)
(define-constant TASK_STATUS_CANCELLED u4)

;; Data Maps
(define-map tasks uint {
    creator: principal,
    assignee: (optional principal),
    title: (string-utf8 100),
    description: (string-utf8 500),
    reward: uint,
    required-skill: (string-utf8 50),
    min-skill-level: uint,
    min-reputation: uint,
    deadline: uint,
    status: uint,
    created-at: uint,
    completed-at: (optional uint)
})

(define-map user-profiles principal {
    reputation: uint,
    completed-tasks: uint,
    skills: (list 10 {skill: (string-utf8 50), level: uint}),
    total-earned: uint,
    last-active: uint
})

(define-map task-applications uint (list 20 principal))
(define-map user-task-history principal (list 50 uint))
(define-map escrow uint uint) ;; task-id -> locked amount

;; Read-only functions
(define-read-only (get-task (task-id uint))
    (map-get? tasks task-id)
)

(define-read-only (get-user-profile (user principal))
    (default-to
        {reputation: u0, completed-tasks: u0, skills: (list), total-earned: u0, last-active: u0}
        (map-get? user-profiles user)
    )
)

(define-read-only (get-task-applications (task-id uint))
    (default-to (list) (map-get? task-applications task-id))
)

(define-read-only (get-user-task-history (user principal))
    (default-to (list) (map-get? user-task-history user))
)

(define-read-only (get-next-task-id)
    (var-get next-task-id)
)

(define-read-only (get-platform-fee-rate)
    (var-get platform-fee-rate)
)

(define-read-only (calculate-platform-fee (amount uint))
    (/ (* amount (var-get platform-fee-rate)) u10000)
)

(define-read-only (get-escrow-amount (task-id uint))
    (default-to u0 (map-get? escrow task-id))
)

(define-read-only (check-skill-requirement (user principal) (required-skill (string-utf8 50)) (min-level uint))
    (let ((profile (get-user-profile user)))
        (let ((user-skills (get skills profile)))
            (fold check-skill-match user-skills {found: false, required: required-skill, min-level: min-level})
        )
    )
)

(define-private (check-skill-match
    (skill-entry {skill: (string-utf8 50), level: uint})
    (acc {found: bool, required: (string-utf8 50), min-level: uint})
)
    (if (get found acc)
        acc
        (if (and
                (is-eq (get skill skill-entry) (get required acc))
                (>= (get level skill-entry) (get min-level acc))
            )
            (merge acc {found: true})
            acc
        )
    )
)

;; Public functions
(define-public (create-task
    (title (string-utf8 100))
    (description (string-utf8 500))
    (reward uint)
    (required-skill (string-utf8 50))
    (min-skill-level uint)
    (min-reputation uint)
    (duration-blocks uint)
)
    (let (
        (task-id (var-get next-task-id))
        (deadline (+ stacks-block-height duration-blocks))
        (platform-fee (calculate-platform-fee reward))
        (total-required (+ reward platform-fee))
    )
        (asserts! (> duration-blocks u0) ERR_INVALID_DURATION)
        (asserts! (>= (stx-get-balance tx-sender) total-required) ERR_INSUFFICIENT_BALANCE)
        (asserts! (<= min-skill-level u10) ERR_INVALID_SKILL_LEVEL)

        ;; Transfer STX to escrow
        (try! (stx-transfer? total-required tx-sender (as-contract tx-sender)))

        ;; Create task
        (map-set tasks task-id {
            creator: tx-sender,
            assignee: none,
            title: title,
            description: description,
            reward: reward,
            required-skill: required-skill,
            min-skill-level: min-skill-level,
            min-reputation: min-reputation,
            deadline: deadline,
            status: TASK_STATUS_ACTIVE,
            created-at: stacks-block-height,
            completed-at: none
        })

        ;; Set escrow amount
        (map-set escrow task-id total-required)

        ;; Update task ID counter
        (var-set next-task-id (+ task-id u1))

        (ok task-id)
    )
)

(define-public (apply-for-task (task-id uint))
    (let ((task (unwrap! (get-task task-id) ERR_TASK_NOT_FOUND)))
        (asserts! (is-eq (get status task) TASK_STATUS_ACTIVE) ERR_TASK_NOT_ACTIVE)
        (asserts! (< stacks-block-height (get deadline task)) ERR_TASK_EXPIRED)

        ;; Check reputation requirement
        (let ((user-profile (get-user-profile tx-sender)))
            (asserts! (>= (get reputation user-profile) (get min-reputation task)) ERR_INSUFFICIENT_REPUTATION)
        )

        ;; Add to applications list
        (let ((current-applications (get-task-applications task-id)))
            (map-set task-applications task-id
                (unwrap! (as-max-len? (append current-applications tx-sender) u20) (err u999))
            )
        )

        ;; Update user's last active time
        (update-user-activity tx-sender)

        (ok true)
    )
)

(define-public (assign-task (task-id uint) (assignee principal))
    (let ((task (unwrap! (get-task task-id) ERR_TASK_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get creator task)) ERR_NOT_AUTHORIZED)
        (asserts! (is-eq (get status task) TASK_STATUS_ACTIVE) ERR_TASK_NOT_ACTIVE)
        (asserts! (< stacks-block-height (get deadline task)) ERR_TASK_EXPIRED)

        ;; Verify assignee applied for the task
        (let ((applications (get-task-applications task-id)))
            (asserts! (is-some (index-of applications assignee)) ERR_NOT_AUTHORIZED)
        )

        ;; Update task status and assignee
        (map-set tasks task-id (merge task {
            assignee: (some assignee),
            status: TASK_STATUS_ASSIGNED
        }))

        ;; Add to user's task history
        (add-to-user-history assignee task-id)

        (ok true)
    )
)

(define-public (complete-task (task-id uint))
    (let ((task (unwrap! (get-task task-id) ERR_TASK_NOT_FOUND)))
        (asserts! (is-eq (get status task) TASK_STATUS_ASSIGNED) ERR_TASK_NOT_ACTIVE)
        (asserts! (is-eq tx-sender (get creator task)) ERR_NOT_AUTHORIZED)
        (asserts! (< stacks-block-height (get deadline task)) ERR_TASK_EXPIRED)

        (let (
            (assignee (unwrap! (get assignee task) ERR_NOT_ASSIGNED))
            (reward (get reward task))
            (platform-fee (calculate-platform-fee reward))
            (escrow-amount (get-escrow-amount task-id))
        )
            ;; Transfer reward to assignee
            (try! (as-contract (stx-transfer? reward tx-sender assignee)))

            ;; Transfer platform fee to contract owner
            (try! (as-contract (stx-transfer? platform-fee tx-sender CONTRACT_OWNER)))

            ;; Update task status
            (map-set tasks task-id (merge task {
                status: TASK_STATUS_COMPLETED,
                completed-at: (some stacks-block-height)
            }))

            ;; Update assignee profile
            (update-user-completion assignee reward)

            ;; Clear escrow
            (map-delete escrow task-id)

            (ok true)
        )
    )
)

(define-public (cancel-task (task-id uint))
    (let ((task (unwrap! (get-task task-id) ERR_TASK_NOT_FOUND)))
        (asserts! (is-eq tx-sender (get creator task)) ERR_NOT_AUTHORIZED)
        (asserts! (not (is-eq (get status task) TASK_STATUS_COMPLETED)) ERR_ALREADY_COMPLETED)

        (let ((escrow-amount (get-escrow-amount task-id)))
            ;; Return escrowed funds to creator
            (try! (as-contract (stx-transfer? escrow-amount tx-sender (get creator task))))

            ;; Update task status
            (map-set tasks task-id (merge task {
                status: TASK_STATUS_CANCELLED
            }))

            ;; Clear escrow
            (map-delete escrow task-id)

            (ok true)
        )
    )
)

(define-public (add-skill (skill-name (string-utf8 50)) (level uint))
    (begin
        (asserts! (<= level u10) ERR_INVALID_SKILL_LEVEL)

        (let ((current-profile (get-user-profile tx-sender)))
            (let ((current-skills (get skills current-profile)))
                (map-set user-profiles tx-sender (merge current-profile {
                    skills: (unwrap! (as-max-len?
                        (append current-skills {skill: skill-name, level: level}) u10
                    ) (err u999)),
                    last-active: stacks-block-height
                }))
            )
        )

        (ok true)
    )
)

(define-public (update-platform-fee (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (asserts! (<= new-rate u1000) (err u200)) ;; Max 10%
        (var-set platform-fee-rate new-rate)
        (ok true)
    )
)

;; Private functions
(define-private (update-user-activity (user principal))
    (let ((current-profile (get-user-profile user)))
        (map-set user-profiles user (merge current-profile {
            last-active: stacks-block-height
        }))
    )
)

(define-private (update-user-completion (user principal) (reward uint))
    (let ((current-profile (get-user-profile user)))
        (map-set user-profiles user (merge current-profile {
            reputation: (+ (get reputation current-profile) u10), ;; +10 reputation per completion
            completed-tasks: (+ (get completed-tasks current-profile) u1),
            total-earned: (+ (get total-earned current-profile) reward),
            last-active: stacks-block-height
        }))
    )
)

(define-private (add-to-user-history (user principal) (task-id uint))
    (let ((current-history (get-user-task-history user)))
        (match (as-max-len? (append current-history task-id) u50)
            new-history (map-set user-task-history user new-history)
            (map-set user-task-history user (list task-id))
        )
    )
)

;; Initialize contract
(begin
    (map-set user-profiles CONTRACT_OWNER {
        reputation: u100,
        completed-tasks: u0,
        skills: (list),
        total-earned: u0,
        last-active: stacks-block-height
    })
)
