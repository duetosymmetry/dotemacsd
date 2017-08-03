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

;; I allow myself to use this 'confusing' command
(put 'downcase-region 'disabled nil)

;; add yasnippet to markdown mode
(add-hook 'markdown-mode-hook #'yas-minor-mode)

;; Add my keybindings
(load "key-bindings")

;; This is kind of key-binding related
(require 'ffap)
(ffap-bindings)

;; make show-paren a bit smarter
(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
     echo area. Has no effect if the character before point is not of
     the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point))))
         (and cb
              (char-equal (char-syntax cb) ?\) )
              (blink-matching-open))))

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
 '(column-number-mode t)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" default)))
 '(desktop-restore-in-current-display nil)
 '(desktop-save-mode t)
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
    (which-key unfill smooth-scroll multiple-cursors emoji-fontset yasnippet zenburn-theme browse-kill-ring company company-c-headers company-shell company-web expand-region ggtags git-gutter-fringe magit-svn rainbow-mode markdown-mode+ yaml-mode tabbar scroll-restore magit auctex org)))
 '(reftex-plug-into-AUCTeX t)
 '(savehist-mode t)
 '(show-paren-mode t)
 '(tab-width 2)
 '(text-scale-mode-step 1.1)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :family "Inconsolata")))))

(require 'package)
(package-initialize)

(load-theme 'zenburn)

;; Use default Apple font for emoji
(emoji-fontset-enable "Apple Color Emoji")

;; Always Be Serving
(server-start)
