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
   '(LaTeX-math-mode turn-on-reftex turn-on-auto-fill latex-extra-mode
                     flyspell-mode))
 '(TeX-PDF-mode t)
 '(TeX-electric-escape t)
 '(TeX-electric-math '("$" . "$"))
 '(TeX-electric-sub-and-superscript t)
 '(TeX-parse-self t)
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-list
   '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")))
 '(TeX-view-program-selection
   '((output-dvi "open") (output-pdf "Skim") (output-html "open")))
 '(auth-sources '("~/.authinfo"))
 '(bibtex-generate-url-list
   '((("url" . ".*:.*"))
     (("doi" . "10\\.[0-9]+/.+") "https://doi.org/%s" ("doi" ".*" 0))
     (("eprint" . ".*") "https://arxiv.org/abs/%s" ("eprint" ".*" 0))))
 '(browse-url-handlers '(("[Aa][Rr][Xx][Ii][Vv]:[a-zA-Z0-9./-]+" . browse-arXiv)))
 '(column-number-mode t)
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7"
     "816bacf37139d6204b761fea0d25f7f2f43b94affa14aa4598bce46157c160c2"
     "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba"
     "0c32e4f0789f567a560be625f239ee9ec651e524e46a4708eb4aba3b9cdc89c5"
     "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df"
     "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109"
     "599f1561d84229e02807c952919cd9b0fbaa97ace123851df84806b067666332"
     "5e52ce58f51827619d27131be3e3936593c9c7f9f9f9d6b33227be6331bf9881"
     "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" default))
 '(desktop-restore-in-current-display nil)
 '(desktop-save-mode t)
 '(dgi-auto-hide-details-p nil)
 '(dired-vc-rename-file t)
 '(diredp-hide-details-initially-flag nil)
 '(display-line-numbers 'visual)
 '(display-line-numbers-type 'relative)
 '(epg-gpg-program "gpg")
 '(ffap-require-prefix t)
 '(fill-column 80)
 '(frame-resize-pixelwise t)
 '(gc-cons-threshold 100000000)
 '(global-display-fill-column-indicator-mode t)
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
 '(lsp-clients-clangd-args
   '("--header-insertion-decorators=0" "-j=4" "--clang-tidy"
     "--fallback-style=google" "--enable-config" "--pch-storage=disk"
     "--rename-file-limit=0"))
 '(magit-diff-refine-hunk 'all)
 '(mouse-wheel-flip-direction t)
 '(mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control))))
 '(mouse-wheel-tilt-scroll t)
 '(org-capture-templates
   '(("n" "Note" entry (file "~/org/notes.org") "* %T " :empty-lines 1)))
 '(org-clock-sound t)
 '(package-archives
   '(("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("elpa-devel" . "https://elpa.gnu.org/devel/")))
 '(package-pinned-packages '((auctex . "elpa-devel")))
 '(package-selected-packages
   '(auctex biblio bind-key browse-kill-ring clang-format closql cmake-mode
            coffee-mode company company-c-headers company-shell company-web
            compat cond-let consult corfu diff-hl dired-git-info eldoc-box
            emoji-fontset expand-region flycheck forge ggtags ghub
            git-gutter-fringe google-c-style highlight-indent-guides
            html-to-markdown indent-bars json-mode julia-mode keycast
            latex-extra llama loccur lsp-julia lsp-mode lsp-pyright lsp-ui magit
            magit-section marginalia markdown-mode markdown-mode+
            multiple-cursors nyan-mode orderless pcre2el projectile
            reveal-in-osx-finder scroll-restore smooth-scroll tabbar time-zones
            tramp transpose-frame treepy unfill vertico visual-regexp-steroids
            wc-mode which-key with-editor yaml yaml-mode yasnippet zenburn-theme))
 '(package-vc-selected-packages
   '((time-zones :url "https://github.com/xenodium/time-zones")))
 '(reftex-plug-into-AUCTeX t)
 '(safe-local-variable-values
   '((etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc") "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/"
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))
     (TeX-PDF-mode . true)))
 '(savehist-mode t)
 '(show-paren-mode t)
 '(tab-width 2)
 '(text-scale-mode-step 1.1)
 '(tls-checktrust t)
 '(tool-bar-mode nil)
 '(tramp-use-connection-share nil)
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
 '(default ((t (:family "JuliaMono" :foundry "nil" :slant normal :weight regular :height 120 :width normal)))))

;; ;; Use Menlo as a fallback in fontset-startup
;; ;; before resorting to fontset-default.
;; (set-fontset-font "fontset-startup" nil "Arial Unicode MS"
;;                   nil 'append)

;; (when (member "DejaVu Sans Mono" (font-family-list))
;;   (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono"))
;;   (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono")))

;; ;; Use Symbola as default for unicode characters
;; (when (member "Symbola" (font-family-list))
;;   (set-fontset-font t 'unicode "Symbola" nil 'prepend))

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

;; from the README at https://github.com/minad/vertico
(use-package vertico
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))
;; from the README at https://github.com/minad/marginalia
;; Enable rich annotations using the Marginalia package

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Use projectile
(require 'projectile)
;; Recommended keymap prefix on macOS
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(projectile-mode +1)


;; Use default Apple font for emoji
(emoji-fontset-enable "Apple Color Emoji")

;; google-c-style
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Needed for lsp-mode to know how to connect to an lsp using LanguageServer.jl
(require 'lsp-julia)

;; Enable lsp for a few modes
(require 'lsp-mode)
(add-hook 'julia-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)

(use-package indent-bars
  :hook ((python-mode
          yaml-mode
          emacs-lisp-mode
          lisp-interaction-mode
          julia-mode
          c-mode-common)
         . indent-bars-mode))

;; Use forge when using magit
(with-eval-after-load 'magit
  (require 'forge))

;; Increase the amount of data which Emacs reads from the process. The emacs
;; default is too low 4k considering that the some of the language server
;; responses are in 800k - 3M range. Set to 1MB
(setq read-process-output-max (* 1024 1024))

(use-package time-zones
  :vc (:url "git@github.com:xenodium/time-zones.git"))

;; from the README at https://github.com/oantolin/orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic partial-completion))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; from the README at https://github.com/minad/corfu
(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  :hook ((emacs-lisp-mode . corfu-mode)
         (lisp-interaction-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  ;; (global-corfu-mode)

  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  )

;; Always Be Serving
(server-start)
