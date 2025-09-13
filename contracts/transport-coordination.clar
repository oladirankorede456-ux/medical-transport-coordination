;; Medical Transport Coordination Smart Contract
;; A smart contract for managing patient transportation services including
;; appointment scheduling, vehicle dispatch, insurance billing, and medical equipment tracking.

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_APPOINTMENT_NOT_FOUND (err u101))
(define-constant ERR_VEHICLE_NOT_AVAILABLE (err u102))
(define-constant ERR_INVALID_STATUS (err u103))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u104))
(define-constant ERR_EQUIPMENT_NOT_AVAILABLE (err u105))

;; Data Variables
(define-data-var next-appointment-id uint u1)
(define-data-var next-vehicle-id uint u1)
(define-data-var service-fee uint u1000000) ;; 1 STX in microSTX

;; Data Maps

;; Appointments
(define-map appointments uint {
  patient-address: principal,
  pickup-location: (string-ascii 100),
  destination: (string-ascii 100),
  scheduled-time: uint,
  vehicle-id: (optional uint),
  medical-equipment: (list 5 (string-ascii 50)),
  insurance-provider: (string-ascii 50),
  status: (string-ascii 20), ;; "scheduled", "assigned", "in-progress", "completed", "cancelled"
  total-cost: uint,
  payment-status: (string-ascii 20) ;; "pending", "paid", "insurance-billed"
})

;; Vehicles
(define-map vehicles uint {
  driver-address: principal,
  license-plate: (string-ascii 20),
  vehicle-type: (string-ascii 30), ;; "ambulance", "wheelchair-van", "standard"
  available-equipment: (list 10 (string-ascii 50)),
  is-available: bool,
  current-location: (string-ascii 100)
})

;; Driver Registry
(define-map authorized-drivers principal bool)

;; Insurance Providers
(define-map insurance-providers (string-ascii 50) {
  contact-info: (string-ascii 100),
  coverage-rate: uint ;; percentage (0-100)
})

;; Equipment Inventory
(define-map medical-equipment (string-ascii 50) {
  available-quantity: uint,
  required-vehicle-type: (string-ascii 30)
})

;; Private Functions

(define-private (is-authorized-driver (driver principal))
  (default-to false (map-get? authorized-drivers driver))
)

(define-private (calculate-insurance-coverage (provider (string-ascii 50)) (total-cost uint))
  (let ((provider-info (map-get? insurance-providers provider)))
    (match provider-info
      info (* total-cost (get coverage-rate info) (/ u1 u100))
      u0
    )
  )
)

(define-private (check-equipment-availability (equipment-list (list 5 (string-ascii 50))))
  (fold check-single-equipment equipment-list true)
)

(define-private (check-single-equipment (equipment (string-ascii 50)) (available-so-far bool))
  (if available-so-far
    (let ((equipment-info (map-get? medical-equipment equipment)))
      (match equipment-info
        info (> (get available-quantity info) u0)
        false
      )
    )
    false
  )
)

;; Public Functions

;; Initialize contract owner as authorized driver
(define-public (initialize)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set authorized-drivers CONTRACT_OWNER true)
    (ok true)
  )
)

;; Register a new driver
(define-public (register-driver (driver principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set authorized-drivers driver true)
    (ok true)
  )
)

;; Register a vehicle
(define-public (register-vehicle 
  (license-plate (string-ascii 20))
  (vehicle-type (string-ascii 30))
  (equipment (list 10 (string-ascii 50)))
  (location (string-ascii 100))
)
  (let ((vehicle-id (var-get next-vehicle-id)))
    (asserts! (is-authorized-driver tx-sender) ERR_NOT_AUTHORIZED)
    (map-set vehicles vehicle-id {
      driver-address: tx-sender,
      license-plate: license-plate,
      vehicle-type: vehicle-type,
      available-equipment: equipment,
      is-available: true,
      current-location: location
    })
    (var-set next-vehicle-id (+ vehicle-id u1))
    (ok vehicle-id)
  )
)

;; Register insurance provider
(define-public (register-insurance-provider
  (provider-name (string-ascii 50))
  (contact-info (string-ascii 100))
  (coverage-rate uint)
)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= coverage-rate u100) ERR_INVALID_STATUS)
    (map-set insurance-providers provider-name {
      contact-info: contact-info,
      coverage-rate: coverage-rate
    })
    (ok true)
  )
)

;; Add medical equipment to inventory
(define-public (add-medical-equipment
  (equipment-name (string-ascii 50))
  (quantity uint)
  (required-vehicle-type (string-ascii 30))
)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set medical-equipment equipment-name {
      available-quantity: quantity,
      required-vehicle-type: required-vehicle-type
    })
    (ok true)
  )
)

;; Schedule an appointment
(define-public (schedule-appointment
  (pickup-location (string-ascii 100))
  (destination (string-ascii 100))
  (scheduled-time uint)
  (required-equipment (list 5 (string-ascii 50)))
  (insurance-provider (string-ascii 50))
)
  (let ((appointment-id (var-get next-appointment-id))
        (total-cost (var-get service-fee)))
    (asserts! (check-equipment-availability required-equipment) ERR_EQUIPMENT_NOT_AVAILABLE)
    (map-set appointments appointment-id {
      patient-address: tx-sender,
      pickup-location: pickup-location,
      destination: destination,
      scheduled-time: scheduled-time,
      vehicle-id: none,
      medical-equipment: required-equipment,
      insurance-provider: insurance-provider,
      status: "scheduled",
      total-cost: total-cost,
      payment-status: "pending"
    })
    (var-set next-appointment-id (+ appointment-id u1))
    (ok appointment-id)
  )
)

;; Assign vehicle to appointment
(define-public (assign-vehicle (appointment-id uint) (vehicle-id uint))
  (let ((appointment (unwrap! (map-get? appointments appointment-id) ERR_APPOINTMENT_NOT_FOUND))
        (vehicle (unwrap! (map-get? vehicles vehicle-id) ERR_VEHICLE_NOT_AVAILABLE)))
    (asserts! (is-authorized-driver tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (get is-available vehicle) ERR_VEHICLE_NOT_AVAILABLE)
    (asserts! (is-eq (get status appointment) "scheduled") ERR_INVALID_STATUS)
    
    ;; Update appointment with vehicle assignment
    (map-set appointments appointment-id
      (merge appointment { vehicle-id: (some vehicle-id), status: "assigned" })
    )
    
    ;; Mark vehicle as unavailable
    (map-set vehicles vehicle-id
      (merge vehicle { is-available: false })
    )
    
    (ok true)
  )
)

;; Update appointment status
(define-public (update-appointment-status (appointment-id uint) (new-status (string-ascii 20)))
  (let ((appointment (unwrap! (map-get? appointments appointment-id) ERR_APPOINTMENT_NOT_FOUND)))
    (asserts! (is-authorized-driver tx-sender) ERR_NOT_AUTHORIZED)
    (map-set appointments appointment-id
      (merge appointment { status: new-status })
    )
    
    ;; If completing, make vehicle available again
    (if (is-eq new-status "completed")
      (match (get vehicle-id appointment)
        vehicle-id (let ((vehicle (unwrap! (map-get? vehicles vehicle-id) ERR_VEHICLE_NOT_AVAILABLE)))
                     (map-set vehicles vehicle-id
                       (merge vehicle { is-available: true })
                     )
                     (ok true)
                   )
        (ok true)
      )
      (ok true)
    )
  )
)

;; Process payment
(define-public (process-payment (appointment-id uint))
  (let ((appointment (unwrap! (map-get? appointments appointment-id) ERR_APPOINTMENT_NOT_FOUND))
        (payment-amount (get total-cost appointment)))
    (asserts! (is-eq tx-sender (get patient-address appointment)) ERR_NOT_AUTHORIZED)
    (asserts! (>= (stx-get-balance tx-sender) payment-amount) ERR_INSUFFICIENT_PAYMENT)
    
    ;; Transfer payment to contract
    (try! (stx-transfer? payment-amount tx-sender (as-contract tx-sender)))
    
    ;; Update payment status
    (map-set appointments appointment-id
      (merge appointment { payment-status: "paid" })
    )
    
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-appointment (appointment-id uint))
  (map-get? appointments appointment-id)
)

(define-read-only (get-vehicle (vehicle-id uint))
  (map-get? vehicles vehicle-id)
)

(define-read-only (get-service-fee)
  (var-get service-fee)
)

(define-read-only (is-driver-authorized (driver principal))
  (is-authorized-driver driver)
)

(define-read-only (get-insurance-coverage (provider (string-ascii 50)) (total-cost uint))
  (calculate-insurance-coverage provider total-cost)
)
