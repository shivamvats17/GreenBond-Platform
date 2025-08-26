;; GreenBond Platform Contract
;; Environmental project funding through blockchain-based green bonds and impact tracking

;; Define the green bond token
(define-fungible-token green-bond-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-amount (err u101))
(define-constant err-project-not-found (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-project-already-funded (err u104))

;; Data structures
(define-map environmental-projects 
  uint 
  {
    project-name: (string-ascii 50),
    funding-goal: uint,
    current-funding: uint,
    carbon-offset-target: uint,
    project-status: (string-ascii 20),
    project-owner: principal,
    creation-block: uint
  })

(define-map project-impacts
  uint
  {
    carbon-reduced: uint,
    trees-planted: uint,
    renewable-energy-kwh: uint,
    last-updated: uint
  })

;; Data variables
(define-data-var next-project-id uint u1)
(define-data-var total-funded-projects uint u0)

;; Function 1: Issue Green Bond for Environmental Project
(define-public (issue-green-bond 
  (project-name (string-ascii 50))
  (funding-goal uint)
  (carbon-offset-target uint))
  (let ((project-id (var-get next-project-id)))
    (begin
      (asserts! (> funding-goal u0) err-invalid-amount)
      (asserts! (> carbon-offset-target u0) err-invalid-amount)
      
      ;; Create new environmental project
      (map-set environmental-projects project-id
        {
          project-name: project-name,
          funding-goal: funding-goal,
          current-funding: u0,
          carbon-offset-target: carbon-offset-target,
          project-status: "FUNDING",
          project-owner: tx-sender,
          creation-block: stacks-block-height
        })
      
      ;; Initialize impact tracking
      (map-set project-impacts project-id
        {
          carbon-reduced: u0,
          trees-planted: u0,
          renewable-energy-kwh: u0,
          last-updated: stacks-block-height
        })
      
      ;; Increment project counter
      (var-set next-project-id (+ project-id u1))
      
      ;; Mint green bond tokens equivalent to funding goal
      (try! (ft-mint? green-bond-token funding-goal tx-sender))
      
      (print {
        event: "green-bond-issued",
        project-id: project-id,
        project-name: project-name,
        funding-goal: funding-goal
      })
      
      (ok project-id))))

;; Function 2: Update Environmental Impact Tracking
(define-public (update-environmental-impact
  (project-id uint)
  (carbon-reduced uint)
  (trees-planted uint)
  (renewable-energy-kwh uint))
  (let ((project (unwrap! (map-get? environmental-projects project-id) err-project-not-found)))
    (begin
      ;; Verify project owner
      (asserts! (is-eq tx-sender (get project-owner project)) err-owner-only)
      
      ;; Update impact metrics
      (map-set project-impacts project-id
        {
          carbon-reduced: carbon-reduced,
          trees-planted: trees-planted,
          renewable-energy-kwh: renewable-energy-kwh,
          last-updated: stacks-block-height
        })
      
      ;; Update project status if significant progress made
      (if (>= carbon-reduced (/ (get carbon-offset-target project) u2))
        (map-set environmental-projects project-id
          (merge project {project-status: "ACTIVE"}))
        true)
      
      (print {
        event: "impact-updated",
        project-id: project-id,
        carbon-reduced: carbon-reduced,
        trees-planted: trees-planted,
        renewable-energy: renewable-energy-kwh
      })
      
      (ok true))))

;; Read-only functions
(define-read-only (get-project-details (project-id uint))
  (ok (map-get? environmental-projects project-id)))

(define-read-only (get-project-impact (project-id uint))
  (ok (map-get? project-impacts project-id)))

(define-read-only (get-total-projects)
  (ok (- (var-get next-project-id) u1)))

(define-read-only (get-green-bond-balance (account principal))
  (ok (ft-get-balance green-bond-token account)))