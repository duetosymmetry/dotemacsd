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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make ffap work with my exercisepath thingy...
;; This needs to be generalized with an AUCTeX hooking mechanism
(require 'ffap)

(defun ffap-latex-mode-with-exercisepath (name)
  "`ffap' function suitable for latex buffers.
This uses the program kpsewhich if available.  In this case, the
variable `ffap-latex-guess-rules' is used for building a filename
out of NAME.
If the function `LaTeX-parse-addexercisepath' has been defined,
it is called to parse extra path info from the buffer for searching."
  (let ((extra-path (if (fboundp 'LaTeX-parse-addexercisepath)
                        (LaTeX-parse-addexercisepath)
                      '())))
    (cond ((file-exists-p name)
           name)
          ((not (executable-find "kpsewhich"))
           (ffap-tex-init)
           (ffap-locate-file name '(".cls" ".sty" ".tex" "")
                             (append extra-path ffap-tex-path)))
          (t
           (let ((curbuf (current-buffer))
                 (guess-rules ffap-latex-guess-rules)
                 (preferred-suffix-rules '(("input" . ".tex")
                                           ("include" . ".tex")
                                           ("usepackage" . ".sty")
                                           ("RequirePackageWithOptions" . ".sty")
                                           ("RequirePackage" . ".sty")
                                           ("documentclass" . ".cls")
                                           ("documentstyle" . ".cls")
                                           ("LoadClass" . ".cls")
                                           ("LoadClassWithOptions" . ".cls")
                                           ("bibliography" . ".bib")
                                           ("addbibresource" . ""))))
             ;; We now add preferred suffix in front of suffixes.
             (when
                 ;; The condition is essentially:
                 ;; (assoc (TeX-current-macro)
                 ;;        (mapcar 'car preferred-suffix-rules))
                 ;; but (TeX-current-macro) can take time, so we just
                 ;; check if one of the `car' in preferred-suffix-rules
                 ;; is found before point on the current line.  It
                 ;; should cover most cases.
                 (save-excursion
                   (re-search-backward (regexp-opt
                                        (mapcar 'car preferred-suffix-rules))
                                       (line-beginning-position)
                                       t))
               (push (cons "" (cdr (assoc (match-string 0) ; i.e. "(TeX-current-macro)"
                                          preferred-suffix-rules)))
                     guess-rules))
             (with-temp-buffer
               (let ((process-environment (buffer-local-value
                                            'process-environment curbuf))
                      (exec-path (buffer-local-value 'exec-path curbuf))
                      (extra-path-string (mapconcat 'identity extra-path ":"))
                      (args (mapcar (lambda (rule)
                                      (concat (car rule) name (cdr rule)))
                                    guess-rules)))
                 (apply #'call-process "kpsewhich" nil t nil args)
                 (if (not (string= "" extra-path-string))
                     (apply #'call-process "kpsewhich" nil t nil "-path" extra-path-string args)))
               (when (< (point-min) (point-max))
                 (buffer-substring (goto-char (point-min)) (line-end-position)))))))))

(advice-add 'ffap-latex-mode :override #'ffap-latex-mode-with-exercisepath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp settings

(connection-local-set-profile-variables
 'remote-path-mbot
 '((tramp-remote-path . ("/opt/sxs/software/gcc-11.4.0/llvm/17.0.6/bin" tramp-default-remote-path))))

(connection-local-set-profiles
 '(:application tramp :machine "mbot") 'remote-path-mbot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ANSI colorize buffer
;; From https://stackoverflow.com/a/23382008/1695428

(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(provide 'leo-lib)
