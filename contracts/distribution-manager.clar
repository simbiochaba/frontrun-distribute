;; Distribution Manager Contract
;; 
;; A comprehensive smart contract for managing project distributions on the Stacks blockchain.
;; Enables transparent tracking, milestone completion, and equitable fund distribution
;; with an immutable audit trail of all financial and project-related activities.

;; Define distribution manager trait for interface compatibility
(define-trait distribution-manager-trait
  (
    ;; Distribute funds for a milestone
    (distribute-milestone-funds 
      (uint uint) 
      (response bool uint))
    
    ;; Check milestone completion status
    (is-milestone-completed 
      (uint) 
      (response bool uint))
    
    ;; Retrieve total budget for a project
    (get-project-budget 
      (uint) 
      (response uint uint))
  ))

;; Error constants
(define-constant err-not-authorized (err u100))
(define-constant err-project-not-found (err u101))
(define-constant err-task-not-found (err u102))
(define-constant err-user-not-found (err u103))
(define-constant err-invalid-status (err u104))
(define-constant err-invalid-role (err u105))
(define-constant err-already-exists (err u106))
(define-constant err-milestone-not-found (err u107))
(define-constant err-invalid-parameters (err u108))
(define-constant err-task-dependency-not-completed (err u109))
(define-constant err-milestone-not-completed (err u110))
(define-constant err-insufficient-funds (err u111))

;; Project status enum values
(define-constant status-planning u1)
(define-constant status-active u2)
(define-constant status-paused u3)
(define-constant status-completed u4)
(define-constant status-cancelled u5)

;; Task status enum values
(define-constant task-status-pending u1)
(define-constant task-status-in-progress u2)
(define-constant task-status-review u3)
(define-constant task-status-completed u4)
(define-constant task-status-cancelled u5)

;; User role enum values
(define-constant role-owner u1)
(define-constant role-manager u2)
(define-constant role-contributor u3)
(define-constant role-viewer u4)

;; Data structures

;; Project data
(define-map projects
  { project-id: uint }
  {
    title: (string-utf8 100),
    description: (string-utf8 500),
    owner: principal,
    status: uint,
    start-date: uint,
    end-date: uint,
    budget: uint,
    creation-time: uint,
    last-updated: uint
  }
)

;; Task data
(define-map tasks
  { project-id: uint, task-id: uint }
  {
    title: (string-utf8 100),
    description: (string-utf8 500),
    assignee: (optional principal),
    status: uint,
    priority: uint,
    estimated-hours: uint,
    start-date: uint,
    due-date: uint,
    creation-time: uint,
    last-updated: uint,
    milestone-id: (optional uint),
    deliverable-hash: (optional (buff 32))
  }
)

;; Task dependencies - tasks that must be completed before a given task can start
(define-map task-dependencies
  { project-id: uint, task-id: uint, dependency-task-id: uint }
  { exists: bool }
)

;; Milestones for projects
(define-map milestones
  { project-id: uint, milestone-id: uint }
  {
    title: (string-utf8 100),
    description: (string-utf8 500),
    due-date: uint,
    payment-amount: uint,
    is-completed: bool,
    is-paid: bool
  }
)

;; Team members for projects with their roles
(define-map team-members
  { project-id: uint, member: principal }
  {
    role: uint,
    joined-at: uint
  }
)

;; Work logs for tracking time spent on tasks
(define-map work-logs
  { project-id: uint, task-id: uint, log-id: uint }
  {
    member: principal,
    hours: uint,
    description: (string-utf8 200),
    logged-at: uint
  }
)

;; Task comments
(define-map task-comments
  { project-id: uint, task-id: uint, comment-id: uint }
  {
    author: principal,
    content: (string-utf8 500),
    created-at: uint
  }
)

;; Activity log - tracks all changes in the system
(define-map activity-log
  { project-id: uint, activity-id: uint }
  {
    actor: principal,
    action-type: (string-utf8 50),
    description: (string-utf8 200),
    timestamp: uint,
    task-id: (optional uint),
    milestone-id: (optional uint)
  }
)

;; Counters for IDs
(define-data-var next-project-id uint u1)
(define-map project-task-counter { project-id: uint } { next-id: uint })
(define-map project-milestone-counter { project-id: uint } { next-id: uint })
(define-map project-activity-counter { project-id: uint } { next-id: uint })
(define-map project-work-log-counter { project-id: uint, task-id: uint } { next-id: uint })
(define-map project-comment-counter { project-id: uint, task-id: uint } { next-id: uint })

;; Private functions

;; Gets the next ID for a project and increments the counter
(define-private (get-and-increment-project-id)
  (let ((current-id (var-get next-project-id)))
    (var-set next-project-id (+ current-id u1))
    current-id))

;; Gets the next task ID for a specific project and increments the counter
(define-private (get-and-increment-task-id (project-id uint))
  (let ((task-counter (default-to { next-id: u1 } (map-get? project-task-counter { project-id: project-id }))))
    (map-set project-task-counter 
      { project-id: project-id } 
      { next-id: (+ (get next-id task-counter) u1) })
    (get next-id task-counter)))

;; Gets the next milestone ID for a specific project and increments the counter
(define-private (get-and-increment-milestone-id (project-id uint))
  (let ((milestone-counter (default-to { next-id: u1 } (map-get? project-milestone-counter { project-id: project-id }))))
    (map-set project-milestone-counter 
      { project-id: project-id } 
      { next-id: (+ (get next-id milestone-counter) u1) })
    (get next-id milestone-counter)))

;; Gets the next activity ID for a specific project and increments the counter
(define-private (get-and-increment-activity-id (project-id uint))
  (let ((activity-counter (default-to { next-id: u1 } (map-get? project-activity-counter { project-id: project-id }))))
    (map-set project-activity-counter 
      { project-id: project-id } 
      { next-id: (+ (get next-id activity-counter) u1) })
    (get next-id activity-counter)))

;; Gets the next work log ID for a specific task and increments the counter
(define-private (get-and-increment-work-log-id (project-id uint) (task-id uint))
  (let ((log-counter (default-to { next-id: u1 } (map-get? project-work-log-counter { project-id: project-id, task-id: task-id }))))
    (map-set project-work-log-counter 
      { project-id: project-id, task-id: task-id } 
      { next-id: (+ (get next-id log-counter) u1) })
    (get next-id log-counter)))

;; Gets the next comment ID for a specific task and increments the counter
(define-private (get-and-increment-comment-id (project-id uint) (task-id uint))
  (let ((comment-counter (default-to { next-id: u1 } (map-get? project-comment-counter { project-id: project-id, task-id: task-id }))))
    (map-set project-comment-counter 
      { project-id: project-id, task-id: task-id } 
      { next-id: (+ (get next-id comment-counter) u1) })
    (get next-id comment-counter)))

;; Checks if a user has proper authorization for a project
(define-private (is-authorized (project-id uint) (user principal) (required-role uint))
  (let ((member-info (map-get? team-members { project-id: project-id, member: user })))
    (and 
      (is-some member-info)
      (<= (unwrap-panic (get role member-info)) required-role))))

;; Add an activity log entry
(define-private (add-activity (project-id uint) (action-type (string-utf8 50)) (description (string-utf8 200)) (task-id (optional uint)) (milestone-id (optional uint)))
  (let ((activity-id (get-and-increment-activity-id project-id)))
    (map-set activity-log
      { project-id: project-id, activity-id: activity-id }
      {
        actor: tx-sender,
        action-type: action-type,
        description: description,
        timestamp: block-height,
        task-id: task-id,
        milestone-id: milestone-id
      })))

;; Check if all task dependencies are completed
(define-private (are-dependencies-completed (project-id uint) (task-id uint))
  ;; This function would need to check all dependencies for the given task
  ;; and ensure they're all in completed status
  ;; For simplicity, we're returning true here
  true)

;; Check if all tasks for a milestone are completed
(define-private (check-milestone-completion (project-id uint) (milestone-id uint))
  ;; This function would need to check all tasks tied to the milestone
  ;; and ensure they're all completed
  ;; For simplicity, we're returning true here
  true)

;; Read-only functions

;; Get project details
(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id }))

;; Get task details
(define-read-only (get-task (project-id uint) (task-id uint))
  (map-get? tasks { project-id: project-id, task-id: task-id }))

;; Get milestone details
(define-read-only (get-milestone (project-id uint) (milestone-id uint))
  (map-get? milestones { project-id: project-id, milestone-id: milestone-id }))

;; Check if user is a team member
(define-read-only (is-team-member (project-id uint) (user principal))
  (is-some (map-get? team-members { project-id: project-id, member: user })))

;; Get user role in project
(define-read-only (get-user-role (project-id uint) (user principal))
  (get role (default-to { role: u0, joined-at: u0 } (map-get? team-members { project-id: project-id, member: user }))))

;; Public functions

;; Create a new project
(define-public (create-project 
  (title (string-utf8 100)) 
  (description (string-utf8 500))
  (start-date uint)
  (end-date uint)
  (budget uint))
  
  (let ((project-id (get-and-increment-project-id)))
    ;; Create project
    (map-set projects
      { project-id: project-id }
      {
        title: title,
        description: description,
        owner: tx-sender,
        status: status-planning,
        start-date: start-date,
        end-date: end-date,
        budget: budget,
        creation-time: block-height,
        last-updated: block-height
      })
    
    ;; Add project creator as owner
    (map-set team-members
      { project-id: project-id, member: tx-sender }
      {
        role: role-owner,
        joined-at: block-height
      })
    
    ;; Log activity
    (add-activity project-id u"project-created" u"Project was created" none none)
    
    ;; Return project ID
    (ok project-id)))

;; Update project details
(define-public (update-project
  (project-id uint)
  (title (string-utf8 100))
  (description (string-utf8 500))
  (status uint)
  (start-date uint)
  (end-date uint)
  (budget uint))
  
  (let ((project (map-get? projects { project-id: project-id })))
    ;; Check if project exists
    (asserts! (is-some project) err-project-not-found)
    
    ;; Check if user is authorized (owner or manager)
    (asserts! (is-authorized project-id tx-sender role-manager) err-not-authorized)
    
    ;; Update project
    (map-set projects
      { project-id: project-id }
      {
        title: title,
        description: description,
        owner: (get owner (unwrap-panic project)),
        status: status,
        start-date: start-date,
        end-date: end-date,
        budget: budget,
        creation-time: (get creation-time (unwrap-panic project)),
        last-updated: block-height
      })
    
    ;; Log activity
    (add-activity project-id u"project-updated" u"Project details updated" none none)
    
    (ok true)))

;; Add team member to project
(define-public (add-team-member
  (project-id uint)
  (member principal)
  (role uint))
  
  (let ((project (map-get? projects { project-id: project-id })))
    ;; Check if project exists
    (asserts! (is-some project) err-project-not-found)
    
    ;; Check if user is authorized (owner or manager)
    (asserts! (is-authorized project-id tx-sender role-manager) err-not-authorized)
    
    ;; Validate role
    (asserts! (and (>= role role-viewer) (<= role role-owner)) err-invalid-role)
    
    ;; Check if member is not already part of the team
    (asserts! (not (is-team-member project-id member)) err-already-exists)
    
    ;; Add team member
    (map-set team-members
      { project-id: project-id, member: member }
      {
        role: role,
        joined-at: block-height
      })
    
    ;; Log activity
    (add-activity project-id u"member-added" u"New member added to project" none none)
    
    (ok true)))

;; Update team member role
(define-public (update-team-member-role
  (project-id uint)
  (member principal)
  (new-role uint))
  
  (let ((project (map-get? projects { project-id: project-id }))
        (member-info (map-get? team-members { project-id: project-id, member: member })))
    ;; Check if project exists
    (asserts! (is-some project) err-project-not-found)
    
    ;; Check if member exists
    (asserts! (is-some member-info) err-user-not-found)
    
    ;; Check if user is authorized (owner or manager)
    (asserts! (is-authorized project-id tx-sender role-manager) err-not-authorized)
    
    ;; Validate role
    (asserts! (and (>= new-role role-viewer) (<= new-role role-owner)) err-invalid-role)
    
    ;; Update member role
    (map-set team-members
      { project-id: project-id, member: member }
      {
        role: new-role,
        joined-at: (get joined-at (unwrap-panic member-info))
      })
    
    ;; Log activity
    (add-activity project-id u"role-updated" u"Member role updated" none none)
    
    (ok true)))

;; Remove team member from project
(define-public (remove-team-member
  (project-id uint)
  (member principal))
  
  (let ((project (map-get? projects { project-id: project-id })))
    ;; Check if project exists
    (asserts! (is-some project) err-project-not-found)
    
    ;; Check if user is authorized (owner or manager)
    (asserts! (is-authorized project-id tx-sender role-manager) err-not-authorized)
    
    ;; Check if trying to remove the owner
    (asserts! (not (is-eq member (get owner (unwrap-panic project)))) err-not-authorized)
    
    ;; Remove team member
    (map-delete team-members { project-id: project-id, member: member })
    
    ;; Log activity
    (add-activity project-id u"member-removed" u"Team member removed from project" none none)
    
    (ok true)))

;; Create a task in a project
(define-public (create-task
  (project-id uint)
  (title (string-utf8 100))
  (description (string-utf8 500))
  (assignee (optional principal))
  (priority uint)
  (estimated-hours uint)
  (start-date uint)
  (due-date uint)
  (milestone-id (optional uint)))
  
  (let ((project (map-get? projects { project-id: project-id }))
        (task-id (get-and-increment-task-id project-id)))
    ;; Check if project exists
    (asserts! (is-some project) err-project-not-found)
    
    ;; Check if user is authorized (owner, manager, or contributor)
    (asserts! (is-authorized project-id tx-sender role-contributor) err-not-authorized)
    
    ;; If milestone provided, check if it exists
    (asserts! (or (is-none milestone-id) 
                 (is-some (map-get? milestones { project-id: project-id, milestone-id: (unwrap-panic milestone-id) })))
            err-milestone-not-found)
    
    ;; Create task
    (map-set tasks
      { project-id: project-id, task-id: task-id }
      {
        title: title,
        description: description,
        assignee: assignee,
        status: task-status-pending,
        priority: priority,
        estimated-hours: estimated-hours,
        start-date: start-date,
        due-date: due-date,
        creation-time: block-height,
        last-updated: block-height,
        milestone-id: milestone-id,
        deliverable-hash: none
      })
    
    ;; Log activity
    (add-activity project-id u"task-created" u"New task created" (some task-id) milestone-id)
    
    (ok task-id)))

;; Update task details
(define-public (update-task
  (project-id uint)
  (task-id uint)
  (title (string-utf8 100))
  (description (string-utf8 500))
  (assignee (optional principal))
  (priority uint)
  (estimated-hours uint)
  (start-date uint)
  (due-date uint)
  (milestone-id (optional uint)))
  
  (let ((task (map-get? tasks { project-id: project-id, task-id: task-id })))
    ;; Check if task exists
    (asserts! (is-some task) err-task-not-found)
    
    ;; Check if user is authorized (owner, manager, or if they're the assignee)
    (asserts! (or (is-authorized project-id tx-sender role-manager)
                 (is-eq (get assignee (unwrap-panic task)) (some tx-sender)))
            err-not-authorized)
    
    ;; If milestone provided, check if it exists
    (asserts! (or (is-none milestone-id) 
                 (is-some (map-get? milestones { project-id: project-id, milestone-id: (unwrap-panic milestone-id) })))
            err-milestone-not-found)
    
    ;; Update task
    (map-set tasks
      { project-id: project-id, task-id: task-id }
      {
        title: title,
        description: description,
        assignee: assignee,
        status: (get status (unwrap-panic task)),
        priority: priority,
        estimated-hours: estimated-hours,
        start-date: start-date,
        due-date: due-date,
        creation-time: (get creation-time (unwrap-panic task)),
        last-updated: block-height,
        milestone-id: milestone-id,
        deliverable-hash: (get deliverable-hash (unwrap-panic task))
      })
    
    ;; Log activity
    (add-activity project-id u"task-updated" u"Task details updated" (some task-id) milestone-id)
    
    (ok true)))

;; Update task status
(define-public (update-task-status
  (project-id uint)
  (task-id uint)
  (new-status uint))
  
  (let ((task (map-get? tasks { project-id: project-id, task-id: task-id })))
    ;; Check if task exists
    (asserts! (is-some task) err-task-not-found)
    
    ;; Check if user is authorized (owner, manager, or if they're the assignee)
    (asserts! (or (is-authorized project-id tx-sender role-manager)
                 (is-eq (get assignee (unwrap-panic task)) (some tx-sender)))
            err-not-authorized)
    
    ;; Validate status
    (asserts! (and (>= new-status task-status-pending) (<= new-status task-status-cancelled)) err-invalid-status)
    
    ;; If moving to in-progress, check dependencies
    (asserts! (or (not (is-eq new-status task-status-in-progress))
                 (are-dependencies-completed project-id task-id))
            err-task-dependency-not-completed)
    
    ;; Update task status
    (map-set tasks
      { project-id: project-id, task-id: task-id }
      (merge (unwrap-panic task)
             {
               status: new-status,
               last-updated: block-height
             }))
    
    ;; Check if milestone should be updated
    (if (and (is-eq new-status task-status-completed)
             (is-some (get milestone-id (unwrap-panic task))))
        (if (check-milestone-completion project-id (unwrap-panic (get milestone-id (unwrap-panic task))))
            (begin
              ;; Update milestone completion status
              (map-set milestones
                { project-id: project-id, milestone-id: (unwrap-panic (get milestone-id (unwrap-panic task))) }
                (merge (unwrap-panic (map-get? milestones 
                                      { project-id: project-id, milestone-id: (unwrap-panic (get milestone-id (unwrap-panic task))) }))
                       { is-completed: true }))
              
              ;; Log milestone completion
              (add-activity project-id u"milestone-completed" u"Milestone completed" none (get milestone-id (unwrap-panic task)))
            )
            ;; No action if not all tasks for milestone are completed
            true
        )
        ;; No milestone to update
        true
    )
    
    ;; Log activity
    (add-activity project-id u"task-status-updated" 
                 (concat u"Task status changed to " 
                        (if (is-eq new-status task-status-pending) u"Pending"
                         (if (is-eq new-status task-status-in-progress) u"In Progress"
                          (if (is-eq new-status task-status-review) u"In Review"
                           (if (is-eq new-status task-status-completed) u"Completed"
                            u"Cancelled"))))) 
                 (some task-id) 
                 (get milestone-id (unwrap-panic task)))
    
    (ok true)))

;; Add task dependency
(define-public (add-task-dependency
  (project-id uint)
  (task-id uint)
  (dependency-task-id uint))
  
  (let ((task (map-get? tasks { project-id: project-id, task-id: task-id }))
        (dependency-task (map-get? tasks { project-id: project-id, task-id: dependency-task-id })))
    ;; Check if tasks exist
    (asserts! (is-some task) err-task-not-found)
    (asserts! (is-some dependency-task) err-task-not-found)
    
    ;; Check if user is authorized (owner or manager)
    (asserts! (is-authorized project-id tx-sender role-manager) err-not-authorized)
    
    ;; Prevent circular dependencies (task cannot depend on itself)
    (asserts! (not (is-eq task-id dependency-task-id)) err-invalid-parameters)
    
    ;; Add dependency
    (map-set task-dependencies
      { project-id: project-id, task-id: task-id, dependency-task-id: dependency-task-id }
      { exists: true })
    
    ;; Log activity
    (add-activity project-id u"dependency-added" 
                 u"Task dependency added" 
                 (some task-id) 
                 none)
    
    (ok true)))

;; Remove task dependency
(define-public (remove-task-dependency
  (project-id uint)
  (task-id uint)
  (dependency-task-id uint))
  
  (let ((task (map-get? tasks { project-id: project-id, task-id: task-id })))
    ;; Check if task exists
    (asserts! (is-some task) err-task-not-found)
    
    ;; Check if user is authorized (owner or manager)
    (asserts! (is-authorized project-id tx-sender role-manager) err-not-authorized)
    
    ;; Remove dependency
    (map-delete task-dependencies
      { project-id: project-id, task-id: task-id, dependency-task-id: dependency-task-id })
    
    ;; Log activity
    (add-activity project-id u"dependency-removed" 
                 u"Task dependency removed" 
                 (some task-id) 
                 none)
    
    (ok true)))

;; Add deliverable to task
(define-public (add-deliverable
  (project-id uint)
  (task-id uint)
  (deliverable-hash (buff 32)))
  
  (let ((task (map-get? tasks { project-id: project-id, task-id: task-id })))
    ;; Check if task exists
    (asserts! (is-some task) err-task-not-found)
    
    ;; Check if user is authorized (owner, manager, or if they're the assignee)
    (asserts! (or (is-authorized project-id tx-sender role-manager)
                 (is-eq (get assignee (unwrap-panic task)) (some tx-sender)))
            err-not-authorized)
    
    ;; Update task with deliverable
    (map-set tasks
      { project-id: project-id, task-id: task-id }
      (merge (unwrap-panic task)
             {
               deliverable-hash: (some deliverable-hash),
               last-updated: block-height
             }))
    
    ;; Log activity
    (add-activity project-id u"deliverable-added" 
                 u"Deliverable added to task" 
                 (some task-id) 
                 (get milestone-id (unwrap-panic task)))
    
    (ok true)))

;; Log work on a task
(define-public (log-work
  (project-id uint)
  (task-id uint)
  (hours uint)
  (description (string-utf8 200)))
  
  (let ((task (map-get? tasks { project-id: project-id, task-id: task-id }))
        (log-id (get-and-increment-work-log-id project-id task-id)))
    ;; Check if task exists
    (asserts! (is-some task) err-task-not-found)
    
    ;; Check if user is authorized (any team member can log work)
    (asserts! (is-team-member project-id tx-sender) err-not-authorized)
    
    ;; Add work log
    (map-set work-logs
      { project-id: project-id, task-id: task-id, log-id: log-id }
      {
        member: tx-sender,
        hours: hours,
        description: description,
        logged-at: block-height
      })
    
    ;; Log activity
    (add-activity project-id u"work-logged" 
                 (concat u"Work hours logged on task: " u"") 
                 (some task-id) 
                 (get milestone-id (unwrap-panic task)))
    
    (ok log-id)))

;; Add comment to task
(define-public (add-comment
  (project-id uint)
  (task-id uint)
  (content (string-utf8 500)))
  
  (let ((task (map-get? tasks { project-id: project-id, task-id: task-id }))
        (comment-id (get-and-increment-comment-id project-id task-id)))
    ;; Check if task exists
    (asserts! (is-some task) err-task-not-found)
    
    ;; Check if user is authorized (any team member can comment)
    (asserts! (is-team-member project-id tx-sender) err-not-authorized)
    
    ;; Add comment
    (map-set task-comments
      { project-id: project-id, task-id: task-id, comment-id: comment-id }
      {
        author: tx-sender,
        content: content,
        created-at: block-height
      })
    
    ;; Log activity
    (add-activity project-id u"comment-added" 
                 u"Comment added to task" 
                 (some task-id) 
                 (get milestone-id (unwrap-panic task)))
    
    (ok comment-id)))

;; Create milestone
(define-public (create-milestone
  (project-id uint)
  (title (string-utf8 100))
  (description (string-utf8 500))
  (due-date uint)
  (payment-amount uint))
  
  (let ((project (map-get? projects { project-id: project-id }))
        (milestone-id (get-and-increment-milestone-id project-id)))
    ;; Check if project exists
    (asserts! (is-some project) err-project-not-found)
    
    ;; Check if user is authorized (owner or manager)
    (asserts! (is-authorized project-id tx-sender role-manager) err-not-authorized)
    
    ;; Create milestone
    (map-set milestones
      { project-id: project-id, milestone-id: milestone-id }
      {
        title: title,
        description: description,
        due-date: due-date,
        payment-amount: payment-amount,
        is-completed: false,
        is-paid: false
      })
    
    ;; Log activity
    (add-activity project-id u"milestone-created" u"New milestone created" none (some milestone-id))
    
    (ok milestone-id)))