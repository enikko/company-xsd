;;; lib.el --- Test library

;; Copyright (C) 2020 Erik Nikko

;; Author: Erik Nikko <65210465+enikko@users.noreply.github.com>
;; Created: 2020-08-06
;; Keywords: Autocompletion, company-mode backend, xml, xsd

;; This file is not part of GNU Emacs
;; The file is free software available under BSD 3-clause license (see LICENSE).
;; No warranties are provided.

;;; Commentary:
;; This file contains a library for tests of company-xsd.el

;;; Code:

(defmacro with-completion-buffer (buffer &rest body)
  "Use BUFFER where completions is possible and execute BODY."
  `(with-current-buffer ,buffer
     (save-window-excursion
       (set-window-buffer nil ,buffer)
       (let ((company-backend 'company-xsd-backend))
         ,@body))))

;;; lib.el ends here
