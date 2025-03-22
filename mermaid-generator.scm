;; Generate Mermaid diagrams from Simulacra models
;; For use with Guile Scheme

;; Using Guile's built-in format module
(use-modules (ice-9 format))
(load "simulacra.scm")

;; Function to generate Mermaid flowchart string for the four orders of simulacra
(define (generate-simulacra-flowchart)
  (let* ((original (make-reality "Original Reality" 'authentic 'original))
         (first (first-order-simulacra original))
         (second (second-order-simulacra original))
         (third (third-order-simulacra original))
         (fourth (fourth-order-simulacra)))
    
    ;; Start building the Mermaid diagram string
    (string-append
      "```mermaid\ngraph TD\n"
      "    A[\"" (reality-name original) "\"] --> B[\"" (reality-name first) "\"]\n"
      "    B --> C[\"" (reality-name second) "\"]\n"
      "    C --> D[\"" (reality-name third) "\"]\n"
      "    D --> E[\"" (reality-name fourth) "\"]\n"
      "    \n"
      "    style A fill:#f9f9f9,stroke:#333,stroke-width:1px\n"
      "    style B fill:#e6f7ff,stroke:#333,stroke-width:1px\n"
      "    style C fill:#b3e0ff,stroke:#333,stroke-width:1px\n"
      "    style D fill:#4db8ff,stroke:#333,stroke-width:1px\n"
      "    style E fill:#0080ff,stroke:#333,stroke-width:1px\n"
      "```")))

;; Function to generate Mermaid diagram for Disneyland effect
(define (generate-disneyland-diagram)
  (let ((disney-effect (disneyland-effect)))
    (string-append
      "```mermaid\ngraph TD\n"
      "    A[\"Disneyland\"] -->|Presented as| B[\"Fantasy/Imaginary\"]\n"
      "    B -->|Makes us believe| C[\"America\"]\n"
      "    C -->|Is| D[\"Real\"]\n"
      "    A -->|Actually conceals that| E[\"All America\"]\n"
      "    E -->|Is| F[\"Disneyland/Simulation\"]\n"
      "    \n"
      "    style A fill:#f9f9f9,stroke:#333,stroke-width:1px\n"
      "    style B fill:#e6f7ff,stroke:#333,stroke-width:1px\n"
      "    style C fill:#b3e0ff,stroke:#333,stroke-width:1px\n"
      "    style D fill:#4db8ff,stroke:#333,stroke-width:1px\n"
      "    style E fill:#e6f7ff,stroke:#333,stroke-width:1px\n"
      "    style F fill:#b3e0ff,stroke:#333,stroke-width:1px\n"
      "```")))

;; Generate a Mermaid diagram from social media simulation
(define (generate-social-media-diagram)
  (string-append
    "```mermaid\nflowchart TD\n"
    "    A[\"Original Experience\"] -->|Shared as| B[\"Direct Post\"]\n"
    "    B -->|Shared as| C[\"Second-hand Post\"]\n"
    "    C -->|Becomes| D[\"Trending Topic\"]\n"
    "    D -->|Transforms into| E[\"Viral Content\"]\n"
    "    E -.->|No relation to| A\n"
    "    \n"
    "    subgraph \"Reality Perception\"\n"
    "    P1[\"Grounded\"] --> P2[\"Mediated\"] --> P3[\"Constructed\"] --> P4[\"Hyperreal\"]\n"
    "    end\n"
    "    \n"
    "    A -.-> P1\n"
    "    B -.-> P2\n"
    "    C -.-> P3\n"
    "    D -.-> P3\n"
    "    E -.-> P4\n"
    "```"))

;; Function to write all diagrams to a markdown file
(define (write-mermaid-diagrams output-file)
  (let ((port (open-output-file output-file)))
    (format port "# Generated Mermaid Diagrams\n\n")
    (format port "## The Four Orders of Simulacra\n\n")
    (format port "~a\n\n" (generate-simulacra-flowchart))
    (format port "## The Disneyland Effect\n\n")
    (format port "~a\n\n" (generate-disneyland-diagram))
    (format port "## Social Media Reality Construction\n\n")
    (format port "~a\n\n" (generate-social-media-diagram))
    (close-port port)))

;; Generate the diagrams (when this file is executed)
(write-mermaid-diagrams "generated-diagrams.md")

;; Display a success message
(display "Mermaid diagrams generated to 'generated-diagrams.md'\n")
