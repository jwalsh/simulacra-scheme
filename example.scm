;; Simulacra and Simulation - Practical Example for Guile Scheme
;; Based on Jean Baudrillard's philosophical concepts

;; Load the main simulacra code
(load "simulacra.scm")

;; ============================================================
;; Practical Example: Social Media Reality Construction
;; ============================================================

;; Define a social media post using Guile's SRFI-9
(use-modules (srfi srfi-9))

(define-record-type <post>
  (make-post content likes shares reality-reference)
  post?
  (content post-content)
  (likes post-likes set-post-likes!)
  (shares post-shares set-post-shares!)
  (reality-reference post-reality-reference))

;; First order: A post directly reflecting an event
(define (create-original-post event)
  (make-post 
    (string-append "I witnessed " event)
    0
    0
    'direct-experience))

;; Second order: A post sharing someone else's post
(define (share-post original-post)
  (make-post
    (string-append "Sharing: " (post-content original-post))
    0
    0
    'second-hand))

;; Third order: A post about a trending topic with no personal verification
(define (create-trending-post topic)
  (make-post
    (string-append "Everyone is talking about " topic)
    0
    0
    'social-construct))

;; Fourth order: A completely fabricated viral post
(define (create-viral-post)
  (make-post
    "You won't believe what happened! #trending #viral"
    0
    0
    'pure-simulation))

;; Social Media Feed - simulates how posts propagate and reality is constructed
(define (simulate-social-media-feed initial-posts iterations)
  (let loop ((posts initial-posts)
             (iter iterations)
             (reality-perception 'grounded))
    (if (= iter 0)
        (list posts reality-perception)
        (let* ((most-popular (find-most-popular posts))
               (shared-posts (map (lambda (p) 
                                    (if (eq? p most-popular)
                                        (begin
                                          (set-post-shares! p (+ (post-shares p) 1))
                                          p)
                                        p))
                                  posts))
               (new-post (share-post most-popular))
               (all-posts (cons new-post shared-posts))
               (new-perception (update-reality-perception reality-perception most-popular)))
          (display (string-append "Iteration " (number->string iter) 
                                  ": Reality perception is now " 
                                  (symbol->string new-perception) "\n"))
          (loop all-posts (- iter 1) new-perception)))))

;; Helper function to find the most popular post
;; Note: Not using SRFI-95 since it may not be available in all Guile installations

(define (find-most-popular posts)
  (let loop ((remaining (cdr posts))
             (most-popular (car posts)))
    (if (null? remaining)
        most-popular
        (let ((current (car remaining)))
          (loop (cdr remaining)
                (if (> (+ (post-likes current) (post-shares current))
                       (+ (post-likes most-popular) (post-shares most-popular)))
                    current
                    most-popular))))))

;; Helper function to update reality perception based on propagating posts
(define (update-reality-perception current-perception popular-post)
  (let ((reference (post-reality-reference popular-post)))
    (case reference
      ((direct-experience) 'grounded)
      ((second-hand) 'mediated)
      ((social-construct) 'constructed)
      ((pure-simulation) 'hyperreal)
      (else current-perception))))

;; ============================================================
;; Demonstration
;; ============================================================

;; Run a simulation of how social media constructs hyperreality
(define (run-social-media-simulation)
  (let* ((real-event (create-original-post "a beautiful sunset"))
         (shared-event (share-post real-event))
         (trending-topic (create-trending-post "the meaning of sunsets"))
         (viral-content (create-viral-post))
         (initial-posts (list real-event shared-event trending-topic viral-content)))
    
    ;; Give some initial likes to simulate user engagement
    (set-post-likes! real-event 5)
    (set-post-likes! shared-event 12)
    (set-post-likes! trending-topic 45)
    (set-post-likes! viral-content 103)
    
    ;; Run the simulation for 10 iterations
    (simulate-social-media-feed initial-posts 10)))

;; This simulation demonstrates how social media can transform a direct experience
;; (first order simulacrum) into a hyperreality (fourth order simulacrum)
;; where viral content with no reality reference becomes more "real" and influential
;; than actual experiences.

;; Example usage:
;; (define simulation-result (run-social-media-simulation))
