;; Project Registry Contract
;; A companion contract to the Distribution Manager that handles project
;; and participant registration, tracking, and governance

(use-trait distribution-manager-trait .distribution-manager.distribution-manager-trait)

;; Project types and statuses
(define-constant project-type-corporate u1)
(define-constant project-type-community u2)
(define-constant project-type-open-source u3)

(define-constant project-status-pending u1)
(define-constant project-status-active u2)
(define-constant project-status-paused u3)
(define-constant project-status-completed u4)

;; Error codes
(define-constant err-unauthorized (err u100))
(define-constant err-project-not-found (err u101))
(define-constant err-project-already-exists (err u102))
(define-constant err-invalid-project-type (err u103))

;; Project registry map
(define-map projects
  { project-id: uint }
  {
    name: (string-utf8 100),
    description: (string-utf8 500),
    project-type: uint,
    status: uint,
    creator: principal,
    created-at: uint,
    distribution-contract: (optional principal)
  }
)

;; Project counter
(define-data-var next-project-id uint u1)

;; Get next available project ID
(define-private (get-next-project-id)
  (let ((current-id (var-get next-project-id)))
    (var-set next-project-id (+ current-id u1))
    current-id))

;; Create a new project
(define-public (create-project
  (name (string-utf8 100))
  (description (string-utf8 500))
  (project-type uint))
  
  ;; Validate project type
  (asserts! (or 
    (is-eq project-type project-type-corporate)
    (is-eq project-type project-type-community)
    (is-eq project-type project-type-open-source))
    err-invalid-project-type)
  
  (let ((project-id (get-next-project-id)))
    (map-set projects
      { project-id: project-id }
      {
        name: name,
        description: description,
        project-type: project-type,
        status: project-status-pending,
        creator: tx-sender,
        created-at: block-height,
        distribution-contract: none
      })
    
    (ok project-id)))

;; Get project details
(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id }))

;; Set distribution contract for a project
(define-public (set-distribution-contract
  (project-id uint)
  (distribution-contract <distribution-manager-trait>))
  
  (let ((project (map-get? projects { project-id: project-id })))
    ;; Validate project exists and caller is creator
    (asserts! (is-some project) err-project-not-found)
    (asserts! (is-eq (get creator (unwrap-panic project)) tx-sender) err-unauthorized)
    
    ;; Update project with distribution contract
    (map-set projects
      { project-id: project-id }
      (merge (unwrap-panic project)
        { 
          distribution-contract: (some (contract-of distribution-contract)),
          status: project-status-active 
        }))
    
    (ok true)))