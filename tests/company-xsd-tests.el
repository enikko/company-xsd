;;; company-xsd-tests.el --- Test for company-mode backend for xml files -*- lexical-binding: t -*-

;; Copyright (C) 2020 Erik Nikko

;; Author: Erik Nikko <65210465+enikko@users.noreply.github.com>
;; Created: 2020-08-06
;; Keywords: Autocompletion, company-mode backend, xml, xsd

;; This file is not part of GNU Emacs
;; The file is free software available under BSD 3-clause license (see LICENSE).
;; No warranties are provided.

;;; Commentary:
;; This file contains tests for company-xsd.el

;;; Code:
(require 'company-xsd)
(load-file "tests/lib.el")

;; Inhibit all messages to get rid of some messages when moving point
(setq inhibit-message t)

(describe "Test unqualified target namespace xsd"
  :var (buffer completions)
  (before-all
    (setq buffer (get-buffer-create "test-buffer"))
    (with-current-buffer buffer
      (insert-file-contents-literally "tests/data/unqualified-target-namespace-schema.xml")
      (nxml-mode)
      (company-xsd-init-buffer)))
  (after-all
   (kill-buffer buffer))
  (it "Test frame existance"
      (with-current-buffer buffer
        (expect company-xsd--xsd-compilation-frame :not :to-be nil)))
  (it "Test root completion"
      (with-completion-buffer buffer
        (goto-line 2)
        (setq completions (mapcar 'substring-no-properties (company-call-backend 'candidates "")))
        (expect (company-xsd--completion-type) :to-be 'company-xsd--new-tag-name)
        (expect completions :to-have-same-items-as '("<fb:foo/>" "<fb:foo>")))))

(describe "Test import schemas"
  :var (buffer completions)
  (before-all
    (setq buffer (get-buffer-create "test-buffer"))
    (with-current-buffer buffer
      (insert-file-contents-literally "tests/data/import.xml")
      (nxml-mode)
      (company-xsd-init-buffer)))
  (after-all
    (kill-buffer buffer))
  (it "Test frame existance"
    (with-current-buffer buffer
      (expect company-xsd--xsd-compilation-frame :not :to-be nil)))
  (it "Test root completion from imported and root"
    (with-completion-buffer buffer
        (goto-line 2)
        (setq completions (mapcar 'substring-no-properties (company-call-backend 'candidates "")))
        (expect (company-xsd--completion-type) :to-be 'company-xsd--new-tag-name)
        (expect completions :to-have-same-items-as
                '("<im:importedElement>" "<im:importedElement/>" "<root:foo>" "<root:foo/>"))))
  (it "Test imported sub-element"
      (with-completion-buffer buffer
        (goto-line 7)
        (setq completions (mapcar 'substring-no-properties (company-call-backend 'candidates "")))
        (expect (company-xsd--completion-type) :to-be 'company-xsd--new-tag-name)
        (expect completions :to-have-same-items-as
                '("<im:importedFoo>" "<im:importedFoo/>" "<im:importedBar>" "<im:importedBar/>" "</im:importedElement>")))))

(describe "Test set schema"
  :var (buffer)
  (it "Verify that schema paths are as expected"
    (setq buffer (get-buffer-create "test-buffer"))
    (with-current-buffer buffer
      (insert-file-contents-literally "tests/data/elisp-set-namespace.xml")
      (setq company-xsd-schemas '(("virtual://importing/namespace" . "tests/data/importing.xsd")))
      (setq company-xsd-namespace-qualifier-alist '(("virtual://importing/namespace" . "root") ("virtual://imported/namespace" . "im")))
      (nxml-mode)
      (company-xsd-init-buffer)
      (expect company-xsd--buffer-schemas :to-equal `(("virtual://importing/namespace" . ,(concat "file://" (file-truename (concat default-directory "tests/data/importing.xsd"))))))
      (expect company-xsd-namespace-qualifier-alist :to-equal
              company-xsd--buffer-namespace-qualifier-alist))
    (kill-buffer buffer)))
  

;;; company-xsd-tests.el ends here
  ;; Local Variables:
  ;; eval: (buttercup-minor-mode)
  ;; End:
