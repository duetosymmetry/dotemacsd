;; These are Leo's local function definitions

(defun buffer-file-git-diff-regions () ""
  (require 'magit)
  (or (magit-git-dir) (error "Dir NOT in a git repo: %s" default-directory))
  (let ((file (buffer-file-name)))
    (or file (error "Buffer \"%s\" does not have a file associated to it" file))
    (let* ((command (concat "git diff -U0 " (shell-quote-argument file) "| grep \"^@@\" | cut -d' ' -f3 | tr +, ' '"))
           (output (shell-command-to-string command))
           (lines (split-string output "\n" t))
           (line-pair-helper (lambda (x) (if (not (cadr x)) (list (car x) 1) x)))
           (line-maybe-pairs (mapcar (lambda (x) (mapcar 'string-to-number (split-string x " " t))) lines)))
      (mapcar line-pair-helper line-maybe-pairs))))

(defun buffer-file-git-diff-regions-apply (func) ""
  (interactive "aFunction to apply to dirty regions: ")
  (save-excursion
    (dolist (line-len (buffer-file-git-diff-regions))
      (goto-char (point-min))
      (forward-line (1- (car line-len)))
      (push-mark)
      (forward-line (cadr line-len))
      (funcall func (region-beginning) (region-end))
      (pop-mark))))

(defun whitespace-cleanup-git-diff-regions () ""
  (interactive)
  (buffer-file-git-diff-regions-apply 'whitespace-cleanup-region))


;; See https://glyph.twistedmatrix.com/2015/11/editor-malware.html
;; about setting up TLS
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

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

(provide 'leo-lib)
