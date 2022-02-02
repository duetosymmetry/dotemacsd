;;; leops.el --- Style hook for Leo's leops document class.  -*- lexical-binding: t; -*-

;;; Code:

(require 'tex)
(require 'latex)

(TeX-add-style-hook
 "leops"
 (function
  (lambda ()
    (TeX-run-style-hooks "article" "geometry" "amsmath" "amssymb"
                         "amsfonts" "amsthm" "xcolor" "tabularx"
                         "fontenc" "inputenc" "enumitem" "fancyhdr"
                         "hyperref" "graphicx" "bm" "verbatim"
                         "caption"))))

;;; leops.el ends here.
