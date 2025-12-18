;; OpenScholar Paper Submission Contract

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u401))
(define-constant ERR_PAPER_NOT_FOUND (err u404))
(define-constant ERR_INVALID_STATUS (err u400))
(define-constant ERR_TOO_MANY_PAPERS (err u413))
(define-constant ERR_INVALID_IPFS_HASH (err u415))

;; Data variables
(define-data-var next-paper-id uint u1)

;; Data maps
(define-map papers
  uint
  {
    author: principal,
    title: (string-utf8 256),
    abstract: (string-utf8 1024),
    ipfs-hash: (string-ascii 46),
    status: (string-ascii 20),
    timestamp: uint
  }
)

(define-map author-papers
  principal
  (list 100 uint)
)

;; Public functions
;; Submit a new paper
(define-public (submit-paper (title (string-utf8 256)) (abstract (string-utf8 1024)) (ipfs-hash (string-ascii 46)))
  (let
    (
      (paper-id (var-get next-paper-id))
      (author tx-sender)
      (author-paper-list (default-to (list) (map-get? author-papers author)))
    )
    (asserts! (is-valid-ipfs-hash ipfs-hash) ERR_INVALID_IPFS_HASH)

    (map-set papers
      paper-id
      {
        author: author,
        title: title,
        abstract: abstract,
        ipfs-hash: ipfs-hash,
        status: "submitted",
        timestamp: block-height
      }
    )

    (map-set author-papers
      author
      (unwrap! (as-max-len? (append author-paper-list paper-id) u100) ERR_TOO_MANY_PAPERS)
    )

    (var-set next-paper-id (+ paper-id u1))
    (ok paper-id)
  )
)

;; Get paper details
(define-read-only (get-paper (paper-id uint))
  (ok (unwrap! (map-get? papers paper-id) ERR_PAPER_NOT_FOUND))
)

;; Get papers by author
(define-read-only (get-author-papers (author principal))
  (ok (default-to (list) (map-get? author-papers author)))
)

;; Update paper status (only contract owner can do this)
(define-public (update-paper-status (paper-id uint) (new-status (string-ascii 20)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (or (is-eq new-status "accepted") (is-eq new-status "rejected") (is-eq new-status "in-review")) ERR_INVALID_STATUS)

    (match (map-get? papers paper-id)
      paper (ok (map-set papers
                  paper-id
                  (merge paper { status: new-status })))
      ERR_PAPER_NOT_FOUND
    )
  )
)

;; Helper function to validate IPFS hash format
(define-private (is-valid-ipfs-hash (hash (string-ascii 46)))
  (and 
    (is-eq (len hash) u46)
    (is-eq (default-to "" (element-at hash u0)) "Q")
    (is-eq (default-to "" (element-at hash u1)) "m")
  )
)

;; Initialize contract
(begin
  (var-set next-paper-id u1)
)
