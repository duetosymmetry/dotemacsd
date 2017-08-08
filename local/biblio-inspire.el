;;; biblio-inspire.el --- Lookup and import bibliographic entries from INSPIRE -*- lexical-binding: t -*-

;; Copyright (C) 2017  Leo C. Stein

;; Author: Leo C. Stein <leo.stein@gmail.com>
;; URL: TODO
;; Based on Clément Pit-Claudel's 'biblio-arxiv.el'

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Lookup and download bibliographic records from INSPIRE using `inspire-lookup'.
;;
;; This package uses `biblio-selection-mode', and plugs into the more general
;; `biblio' package (which see for more documentation).

;;; Code:

(require 'biblio-core)
(require 'biblio-doi)
(require 'timezone)

(defgroup biblio-inspire nil
  "INSPIRE support in biblio.el"
  :group 'biblio)

(defcustom biblio-inspire-bibtex-header "biblio-inspire"
  "Which header to use for BibTeX entries generated from INSPIRE metadata."
  :group 'biblio
  :type 'string)

;;(defun biblio-inspire--fetch-real-bibtex (metadata) ...)

(defun biblio-inspire--build-bibtex-1 (metadata)
  "Create an unformated BibTeX record for METADATA."
  (let-alist metadata
    (format "@%s{NO_KEY,
author = {%s},
title = {{%s}},
year = {%s},
archivePrefix = {arXiv},
eprint = {%s},
primaryClass = {%s}}"
            biblio-inspire-bibtex-header
            (biblio-join-1 " AND " .authors)
            .title .year .identifier .category)))

(defun biblio-inspire--build-bibtex (metadata)
  "Create a BibTeX record for METADATA."
  (let-alist metadata
    (message "Auto-generating a BibTeX entry for %S." .id)
    (biblio-format-bibtex (biblio-inspire--build-bibtex-1 metadata) t)))

(defun biblio-inspire--forward-bibtex (metadata forward-to)
  "Forward BibTeX for INSPIRE entry METADATA to FORWARD-TO."
  (let-alist metadata
    (message (concat "identifier is" .identifier))
    (funcall forward-to (biblio-arxiv--build-bibtex metadata))))

(defun biblio-inspire--get-author (author)
  "Get author for INSPIRE search results."
  (when (eq (car-safe author) 'dc:creator)
    (car (cddr author))))

(defun biblio-inspire--get-subject (subject)
  "Get subject for INSPIRE search results."
  (when (eq (car-safe subject) 'dc:subject)
    (car (cddr subject))))

(defun biblio-inspire--get-jref (jref)
  "Get the journal reference from INSPIRE search results."
  (when (eq (car-safe jref) 'journal)
    (let-alist (cdr jref)
      (cadr .publication))))

(defun biblio-inspire--extract-id (id)
  "Extract identifier from ID, the URL of an INSPIRE abstract."
  (replace-regexp-in-string "https?://inspirehep.net/record/" "" id))

(defun biblio-inspire--extract-interesting-fields (entry)
  "Prepare an INSPIRE search result ENTRY for display."
  (let-alist entry
    (let ((id (biblio-inspire--extract-id (cadr .dc:source))))
      (list (cons 'identifier id)
            (cons 'year (aref (timezone-parse-date (cadr .dc:date)) 0))
            (cons 'title (cadr .dc:title))
            (cons 'authors (seq-map #'biblio-inspire--get-author entry))
            (cons 'container (seq-map #'biblio-inspire--get-jref entry))
            (cons 'category (seq-map #'biblio-inspire--get-subject entry))
            (cons 'type "article")
            (cons 'url (cadr .dc:source))
            (cons 'direct-url (cadr .dc:identifier))))))

(defun biblio-inspire--entryp (entry)
  "Check if ENTRY is an INSPIRE entry."
  (eq (car-safe entry) 'dc:dc))

(defun biblio-inspire--parse-search-results ()
  "Extract search results from INSPIRE response."
  (biblio-decode-url-buffer 'utf-8)
  (let-alist (xml-parse-region (point-min) (point-max))
    (seq-map #'biblio-inspire--extract-interesting-fields
             (seq-filter #'biblio-inspire--entryp .collection))))

(defun biblio-inspire--url (query)
  "Create an INSPIRE url to look up QUERY."
  ;; Below, of=xd gets the 'detailed' XML format
  (format "http://inspirehep.net/search?ln=en&ln=en&p=%s&of=xd&action_search=Search&so=d&rg=250&sc=0"
          (url-encode-url query)))

;;;###autoload
(defun biblio-inspire-backend (command &optional arg &rest more)
  "An INSPIRE backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'."
  (pcase command
    (`name "INSPIRE")
    (`prompt "INSPIRE query: ")
    (`url (biblio-inspire--url arg))
    (`parse-buffer (biblio-inspire--parse-search-results))
    (`forward-bibtex (biblio-inspire--forward-bibtex arg (car more)))
    (`register (add-to-list 'biblio-backends #'biblio-inspire-backend))))

;;;###autoload
(add-hook 'biblio-init-hook #'biblio-inspire-backend)

;;;###autoload
(defun biblio-inspire-lookup (&optional query)
  "Start an INSPIRE search for QUERY, prompting if needed."
  (interactive)
  (biblio-lookup #'biblio-inspire-backend query))

;;;###autoload
(defalias 'inspire-lookup 'biblio-inspire-lookup)

(provide 'biblio-inspire)
;;; biblio-inspire.el ends here
