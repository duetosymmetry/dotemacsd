;; append to load path
;; (add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/local/")

;; For org-mode
;(require 'org-install)
;(setq org-directory "~/org")
;(setq org-mobile-inbox-for-pull "~/org/mobile-captured.org")
;(setq org-refile-targets (quote ((org-agenda-files :regexp . "*"))))
;(setq org-refile-use-outline-path 't)
;(setq org-mobile-directory "~/Dropbox/MobileOrg")

;; (require 'package) ;; You might already have this line
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.org/packages/"))
;; (when (< emacs-major-version 24)
;;   ;; For important compatibility libraries like cl-lib
;;   (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;; (package-initialize) ;; You might already have this line

;; To save and restore the previous desktop
(setq desktop-dirname "~/.emacs.d/")
(desktop-save-mode 1)

;; I might need this??
(server-start)
(put 'downcase-region 'disabled nil)

;; From https://github.com/yoshiki/yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; setup files ending in ".markdown" to open in markdown-mode
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; Add my keybindings
(load "key-bindings")

;; This is kind of key-binding related
(require 'ffap)
(ffap-bindings)

;; My local definitions
(require 'leo-lib)

;; I'm on Darwin (Mac OS X), so I have to install gnu coreutils and
;; use "gls" instead of "ls" for dired
(when (eq system-type 'darwin)
  (set-variable 'insert-directory-program "gls")
)

;; Use ggtags
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (ggtags-mode 1))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-electric-left-right-brace t)
 '(LaTeX-mode-hook (quote (LaTeX-math-mode turn-on-reftex auto-fill-mode)))
 '(TeX-PDF-mode t)
 '(TeX-electric-escape t)
 '(TeX-electric-math (quote ("$" . "$")))
 '(TeX-electric-sub-and-superscript t)
 '(TeX-parse-self t)
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-list
   (quote
    (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b"))))
 '(TeX-view-program-selection
   (quote
    ((output-dvi "open")
     (output-pdf "Skim")
     (output-html "open"))))
 '(custom-safe-themes
   (quote
    ("67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" default)))
 '(ffap-require-prefix t)
 '(global-font-lock-mode t)
 '(indent-tabs-mode nil)
 '(ispell-program-name "aspell")
 '(org-capture-templates
   (quote
    (("n" "Note" entry
      (file "~/org/notes.org")
      "* %T " :empty-lines 1))))
 '(package-archives
   (quote
    (("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (zenburn-theme browse-kill-ring company company-c-headers company-shell company-web expand-region ggtags git-gutter-fringe magit-svn rainbow-mode markdown-mode+ yaml-mode tabbar scroll-restore magit auctex org)))
 '(reftex-plug-into-AUCTeX t)
 '(show-paren-mode t)
 '(tab-width 2)
 '(text-scale-mode-step 1.1)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :family "Inconsolata")))))
