;;; leops.el --- Style hook for Leo's leops document class.  -*- lexical-binding: t; -*-

;;; Code:

(require 'tex)
(require 'latex)

(defvar LaTeX-leops-package-options-list
  '(;; Uni and course info
    ("schoolname")
    ("deptname")
    ("coursename")
    ("coursenum")
    ("termyear")
    ("instructor")
    ;; Problem set info
    ("psnum")
    ("psdue")
    ;; Show solutions or not
    ("solutions" ("true" "false")))
  "Package options for the leops package.")

(TeX-add-style-hook
 "leops"
 (function
  (lambda ()
    (TeX-run-style-hooks "article" "geometry" "amsmath" "amssymb"
                         "amsfonts" "amsthm" "xcolor" "tabularx"
                         "fontenc" "inputenc" "enumitem" "fancyhdr"
                         "hyperref" "graphicx" "bm" "verbatim"
                         "caption" "mdframed" "tikz")
    (TeX-add-symbols
     '("pssetup" (TeX-arg-key-val LaTeX-leops-package-options-list))
     '("showsolutions" 0)
     '("hidesolutions" 0)
     '("ifsolutions" t)
     '("ifnotsolutions" t)
     '("makeheader" 0)
     '("bs" t)
     '("pd" 0)
     '("nn" 0)
     '("cd" 0)
     '("avg" t)
     '("rcurs" 0)
     '("brcurs" 0)
     '("hrcurs" 0)
     '("diag" 0)
     '("sign" 0)
     '("Tr" 0)
     '("itemrange" t))
    (LaTeX-add-environments
     '("solution" ["Label (default is 'Solution:')"]))
    )))

;;; leops.el ends here.
