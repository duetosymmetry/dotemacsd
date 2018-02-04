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
