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
 '(LaTeX-mode-hook
   '(LaTeX-math-mode turn-on-reftex turn-on-auto-fill latex-extra-mode flyspell-mode))
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
 '(auth-sources '("~/.authinfo"))
 '(bibtex-generate-url-list
   '((("url" . ".*:.*"))
     (("doi" . "10\\.[0-9]+/.+")
      "https://doi.org/%s"
      ("doi" ".*" 0))
     (("eprint" . ".*")
      "https://arxiv.org/abs/%s"
      ("eprint" ".*" 0))))
 '(column-number-mode t)
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" "816bacf37139d6204b761fea0d25f7f2f43b94affa14aa4598bce46157c160c2" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "0c32e4f0789f567a560be625f239ee9ec651e524e46a4708eb4aba3b9cdc89c5" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "599f1561d84229e02807c952919cd9b0fbaa97ace123851df84806b067666332" "5e52ce58f51827619d27131be3e3936593c9c7f9f9f9d6b33227be6331bf9881" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" default))
 '(desktop-restore-in-current-display nil)
 '(desktop-save-mode t)
 '(dgi-auto-hide-details-p nil)
 '(dired-vc-rename-file t)
 '(diredp-hide-details-initially-flag nil)
 '(display-line-numbers 'visual)
 '(display-line-numbers-type 'relative)
 '(epg-gpg-program "gpg")
 '(ffap-require-prefix t)
 '(gc-cons-threshold 100000000)
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
 '(org-capture-templates
   '(("n" "Note" entry
      (file "~/org/notes.org")
      "* %T " :empty-lines 1)))
 '(org-clock-sound t)
 '(package-archives
   '(("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("elpa-devel" . "https://elpa.gnu.org/devel/")))
 '(package-pinned-packages '((auctex . "elpa-devel")))
 '(package-selected-packages
   '(flycheck yaml-mode magit transient marginalia vertico html-to-markdown dired-git-info cmake-mode expand-region treepy emacsql-sqlite tramp company zenburn-theme yasnippet multiple-cursors which-key wc-mode nyan-mode unfill bind-key lsp-mode clang-format loccur julia-mode use-package lsp-ui diff-hl coffee-mode json-mode highlight-indent-guides pcre2el visual-regexp-steroids biblio smooth-scroll emoji-fontset browse-kill-ring company-c-headers company-shell company-web ggtags git-gutter-fringe markdown-mode+ tabbar scroll-restore))
 '(reftex-plug-into-AUCTeX t)
 '(safe-local-variable-values
   '((etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc")
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/" "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))
     (TeX-PDF-mode . true)))
 '(savehist-mode t)
 '(show-paren-mode t)
 '(tab-width 2)
 '(text-scale-mode-step 1.1)
 '(tls-checktrust t)
 '(tool-bar-mode nil)
 '(tramp-use-ssh-controlmaster-options nil)
 '(truncate-lines t)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
 '(warning-suppress-types '((comp)))
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

;; Make ffap work for AUCTeX too
(require 'ffap)
(add-to-list 'ffap-alist '(LaTeX-mode . ffap-latex-mode))
(add-to-list 'ffap-string-at-point-mode-alist '(LaTeX-mode "--:\\\\$+<>@-Z_[:alpha:]~*?" "<@" "@>;.,!:"))

(load-theme 'zenburn)

;; Add my keybindings
(load "key-bindings")

;; Use ido
(ido-mode 1)

;; Use vertico and marginalia for a better minibuffer experience
(vertico-mode 1)
(marginalia-mode 1)

;; Use default Apple font for emoji
(emoji-fontset-enable "Apple Color Emoji")

;; Use forge when using magit
(with-eval-after-load 'magit
  (require 'forge))

;; Increase the amount of data which Emacs reads from the process. The emacs
;; default is too low 4k considering that the some of the language server
;; responses are in 800k - 3M range. Set to 1MB
(setq read-process-output-max (* 1024 1024))

;; Always Be Serving
(server-start)
