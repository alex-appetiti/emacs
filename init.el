(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)

(defvar my-packages
  '(neotree
    paredit
    rainbow-delimiters
    cider
    clojure-mode
    company
    magit))

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

;; Ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Eshell
(setq eshell-prompt-function
      (lambda ()
	(concat (eshell/pwd) " λ> ")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (paredit neotree))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
