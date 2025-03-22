;;; init.el --- Emacs configuration for Simulacra-Scheme project

;;; Commentary:
;; This configuration provides Geiser support for Guile Scheme
;; and enhances syntax highlighting for better presentations and demos.

;;; Code:

;; Package system setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Geiser for Scheme interaction
(use-package geiser
  :config
  (setq geiser-active-implementations '(guile)))

(use-package geiser-guile
  :after geiser
  :config
  (setq geiser-guile-binary "guile")
  (setq geiser-guile-load-path '("/Users/jasonwalsh/projects/jwalsh/simulacra-scheme")))

;; Enhanced Scheme mode with better syntax highlighting
(use-package scheme-mode
  :ensure nil  ; Built-in
  :hook (scheme-mode . (lambda ()
                         (setq prettify-symbols-alist
                               '(("lambda" . ?λ)
                                 ("define" . ?≡)
                                 ("eq?" . ?≟)
                                 ("equal?" . ?≡)
                                 ("#t" . ?✓)
                                 ("#f" . ?✗)))))
  :config
  (setq scheme-program-name "guile"))

;; Rainbow delimiters for better bracket visibility
(use-package rainbow-delimiters
  :hook (scheme-mode . rainbow-delimiters-mode))

;; Company for autocompletion
(use-package company
  :hook (scheme-mode . company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1))

;; Paredit for structured editing
(use-package paredit
  :hook (scheme-mode . paredit-mode))

;; Theme setup for presentations
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  ;; Enhanced contrast for presentations
  (setq doom-one-brighter-comments t)
  (setq doom-one-brighter-modeline t))

;; Nice mode line
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Helpful for demos - highlights the current line
(global-hl-line-mode 1)

;; Font settings suitable for presentations
(set-face-attribute 'default nil :font "Fira Code" :height 140)
(set-face-attribute 'variable-pitch nil :font "Fira Sans" :height 140)

;; Org-mode settings for documentation
(use-package org
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-startup-with-inline-images t)
  ;; Enable Mermaid diagrams in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((scheme . t)
     (mermaid . t))))

;; Functions for demo purposes

(defun simulacra-load-and-run ()
  "Load the current Scheme file and run the main example."
  (interactive)
  (geiser-load-current-buffer)
  (run-scheme "guile -l example.scm -c \"(run-social-media-simulation)\""))

(defun simulacra-generate-diagrams ()
  "Generate Mermaid diagrams from Scheme code."
  (interactive)
  (async-shell-command "make generate-diagrams"))

;; Keybindings for Simulacra-Scheme project
(global-set-key (kbd "C-c r") 'simulacra-load-and-run)
(global-set-key (kbd "C-c g") 'simulacra-generate-diagrams)
(global-set-key (kbd "C-c s") 'run-scheme)

;; Project-specific settings
(add-hook 'scheme-mode-hook
          (lambda ()
            (when (string-match-p "simulacra-scheme" default-directory)
              (setq left-margin-width 2)
              (setq right-margin-width 2)
              (set-window-buffer nil (current-buffer)))))

;; Tweak colors for Simulacra concepts
(defface simulacra-reality-face
  '((t (:foreground "#98c379" :weight bold)))
  "Face for reality concepts.")

(defface simulacra-hyperreality-face
  '((t (:foreground "#c678dd" :weight bold)))
  "Face for hyperreality concepts.")

(defface simulacra-simulation-face
  '((t (:foreground "#e5c07b" :weight bold)))
  "Face for simulation concepts.")

;; Add font-lock keywords for Simulacra concepts
(font-lock-add-keywords
 'scheme-mode
 '(("\\<\\(reality\\|original\\|authentic\\)\\>" . 'simulacra-reality-face)
   ("\\<\\(hyperreal\\|simulation\\|simulacra\\)\\>" . 'simulacra-hyperreality-face)
   ("\\<\\(first-order\\|second-order\\|third-order\\|fourth-order\\)\\>" . 'simulacra-simulation-face)))

;; Auto-start functionality
(add-hook 'after-init-hook
          (lambda ()
            (message "Simulacra-Scheme environment initialized!")))

(provide 'init)
;;; init.el ends here
