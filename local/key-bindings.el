;; This comes from https://github.com/magnars/.emacs.d/blob/master/key-bindings.el
;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(bind-key "C-x r q" 'save-buffers-kill-emacs)
(bind-key "C-x C-c" 'delete-frame)

;; (require 'expand-region)
(bind-key "C-=" 'er/expand-region)

;; Use ibuffer
(bind-key "C-x C-b" 'ibuffer)

;; Enter magit-status
(bind-key "C-x g" 'magit-status)

;; Start org-capturing
(bind-key "C-c c" 'org-capture)

(with-eval-after-load 'org
  (bind-key "C-c i" 'org-insert-item))

;; Zooming in a mac fashion
(bind-key "s-=" 'text-scale-increase)
(bind-key "s--" 'text-scale-decrease)
(bind-key "s-0" (lambda () (interactive) (text-scale-set 0)))

;; Top/bottom of buffer in a mac fashion
(bind-key "<s-up>"   'beginning-of-buffer)
(bind-key "<s-down>" 'end-of-buffer)

;; Generate mouse-2 commands via Cmd-click
;; Taken from https://emacs.stackexchange.com/a/20948/16482
;; (define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))

(eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; After TeX input method is first loaded, add
;; \vec to insert COMBINING RIGHT ARROW ABOVE
;; See https://www.emacswiki.org/emacs/TeXInputMethod
(with-eval-after-load "quail/latin-ltx"
  (with-temp-buffer
    (quail-select-package "TeX")
    (quail-define-rules ((append . t))
     ("\\vec" ?⃗))))
