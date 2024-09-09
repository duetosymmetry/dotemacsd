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

;; I allow myself to use these 'confusing' command
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; add yasnippet and auto-fille-mode to markdown mode
(add-hook 'markdown-mode-hook #'yas-minor-mode)
(add-hook 'markdown-mode-hook #'auto-fill-mode)

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

;; I now put dired+.el in local
(require 'dired+)

(add-to-list 'load-path "~/.emacs.d/local/bibretrieve/")
(require 'bibretrieve)

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
 '(LaTeX-indent-environment-list
   '(("verbatim" current-indentation)
     ("verbatim*" current-indentation)
     ("tabular" LaTeX-indent-tabular)
     ("tabular*" LaTeX-indent-tabular)
     ("align")
     ("align*")
     ("array" LaTeX-indent-tabular)
     ("eqnarray" LaTeX-indent-tabular)
     ("eqnarray*" LaTeX-indent-tabular)
     ("displaymath")
     ("equation")
     ("equation*")
     ("picture")
     ("tabbing")))
 '(LaTeX-mode-hook
   '(LaTeX-math-mode turn-on-reftex auto-fill-mode latex-extra-mode flyspell-mode))
 '(TeX-PDF-mode t)
 '(TeX-electric-escape t)
 '(TeX-electric-math '("$" . "$"))
 '(TeX-electric-sub-and-superscript t)
 '(TeX-parse-self t)
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-list
   '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")))
 '(TeX-view-program-selection
   '((output-dvi "open")
     (output-pdf "Skim")
     (output-html "open")))
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(auth-sources '("~/.authinfo"))
 '(column-number-mode t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("816bacf37139d6204b761fea0d25f7f2f43b94affa14aa4598bce46157c160c2" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "0c32e4f0789f567a560be625f239ee9ec651e524e46a4708eb4aba3b9cdc89c5" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "599f1561d84229e02807c952919cd9b0fbaa97ace123851df84806b067666332" "5e52ce58f51827619d27131be3e3936593c9c7f9f9f9d6b33227be6331bf9881" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" default))
 '(desktop-restore-in-current-display nil)
 '(desktop-save-mode t)
 '(diredp-hide-details-initially-flag nil)
 '(display-line-numbers 'visual)
 '(display-line-numbers-type 'relative)
 '(epg-gpg-program "gpg")
 '(fci-rule-color "#383838")
 '(ffap-require-prefix t)
 '(global-display-line-numbers-mode t)
 '(global-font-lock-mode t)
 '(global-visual-line-mode t)
 '(ibuffer-expert t)
 '(ido-default-buffer-method 'selected-window)
 '(ido-enable-flex-matching t)
 '(ido-use-filename-at-point 'guess)
 '(ido-use-url-at-point t)
 '(indent-tabs-mode nil)
 '(ispell-program-name "aspell")
 '(js-indent-level 2)
 '(magit-diff-refine-hunk 'all)
 '(mouse-wheel-flip-direction t)
 '(mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control))))
 '(mouse-wheel-tilt-scroll t)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-capture-templates
   '(("n" "Note" entry
      (file "~/org/notes.org")
      "* %T " :empty-lines 1)))
 '(org-clock-sound t)
 '(package-archives
   '(("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-selected-packages
   '(loccur julia-mode use-package lsp-pyright lsp-mode lsp-ui forge diff-hl coffee-mode json-mode highlight-indent-guides pcre2el visual-regexp-steroids nyan-mode bind-key wc-mode dired+ latex-extra biblio which-key unfill smooth-scroll multiple-cursors emoji-fontset yasnippet zenburn-theme browse-kill-ring company company-c-headers company-shell company-web expand-region ggtags git-gutter-fringe magit-svn rainbow-mode markdown-mode+ yaml-mode tabbar scroll-restore magit auctex org))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(reftex-plug-into-AUCTeX t)
 '(safe-local-variable-values '((TeX-PDF-mode . true)))
 '(savehist-mode t)
 '(show-paren-mode t)
 '(tab-width 2)
 '(text-scale-mode-step 1.1)
 '(tls-checktrust t)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
 '(wc-modeline-format "WC[%W%w=%twW; %C%c=%tcC]")
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Hack"))))
 '(line-number ((t (:background "#383838" :foreground "#6F6F6F" :family "Hack"))))
 '(tab-bar ((t (:height 1.1 :foreground "black" :background "grey85" :inherit variable-pitch)))))

(require 'package)
(package-initialize)

;; See https://github.com/d12frosted/homebrew-emacs-plus/issues/55
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . nil))

(load-theme 'zenburn)

;; Add my keybindings
(load "key-bindings")

;; Use ido
(ido-mode 1)

;; Use default Apple font for emoji
(emoji-fontset-enable "Apple Color Emoji")

;; Use forge when using magit
(with-eval-after-load 'magit
  (require 'forge))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; Always Be Serving
(server-start)
