(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
;; (package-refresh-contents)

(defvar my-packages
  '(neotree
    paredit
    rainbow-delimiters
    cider
    swift-mode
    clojure-mode
    rust-mode
    company
    magit
    loccur
    typescript-mode
    smartparens))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (print (format "Installing %s" package))
    (package-install package)))

;; Global
(load-theme 'tsdh-dark t)
(setq neo-autorefresh nil)
(show-paren-mode 1)
(setq company-idle-delay nil)
(global-set-key (kbd "M-TAB") #'company-complete)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Elisp
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
(with-eval-after-load "cider-mode"
  (define-key cider-repl-mode-map
    (kbd "C-c c")
    'cider-repl-clear-buffer))

;; Smart parens
(use-package smartparens :ensure t
  :config (require 'smartparens-rust))

;; Eglot
(use-package eglot
  :ensure t
  :hook ((rust-mode . eglot-ensure)))

;; Rust
(require 'rust-mode)

(defun pptt/rust/mode-hook ()
  (column-number-mode)
  (display-line-numbers-mode)
  (hs-minor-mode)
  (smartparens-mode)
  (setq indent-tabs-mode nil)
  (define-key rust-mode-map (kbd "C-<right>")   'sp-forward-slurp-sexp)
  (define-key rust-mode-map (kbd "C-<left>")    'sp-forward-barf-sexp)
  (define-key rust-mode-map (kbd "C-M-<right>") 'sp-backward-slurp-sexp)
  (define-key rust-mode-map (kbd "C-M-<left>")  'sp-backward-barf-sexp)
  (define-key rust-mode-map "\C-c>" 'hs-show-all)
  (define-key rust-mode-map "\C-c<" 'hs-hide-all)
  (define-key rust-mode-map "\C-c;" 'hs-toggle-hiding)
  (define-key rust-mode-map "\C-c'" 'hs-hide-level))

(use-package rust-mode
  :ensure t
  :hook (rust-mode . pptt/rust/mode-hook)
  :init
  (setq rust-mode-treesitter-derive t))


;; Java
(defun my-java-mode-hook ()
  (setq c-basic-offset 2)
  (c-set-offset 'substatement-open 0))

(add-hook 'java-mode-hook 'my-java-mode-hook)

;; Ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Loccur
(require 'loccur)
(define-key global-map (kbd "C-M-o") 'loccur)

(windmove-default-keybindings)

;; overriding image.el function image-type-available-p
(defun image-type-available-p (type)
  "Return t if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
  (if (eq 'svg type)
      nil
    (and (fboundp 'init-image-library)
         (init-image-library type))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(loccur swift-mode paredit neotree)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
