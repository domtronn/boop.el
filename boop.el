;;; boop.el --- Monitor Anything simlpy in the mode line

;; Copyright (C) 2016  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A plugin which can be used to monitor services or endpoints and
;; give you live reports (in the form of coloured characters) of the
;; status of that thing

;; It does this by asynchronously running scripts and mapping the exit
;; codes to colours

;;; Code:

;; TODO: Add in validation that `boop-plugins-dir` exists
;; TODO: Make the strategy stuff work

;;;; Customs

(require 'async)
(require 'dash)
(require 's)

(defgroup boop nil
	"Manage how boop gets and displays monitoring results."
	:prefix "boop-"
	:group 'tools
	:group 'convenience)

(defcustom boop-success "1"
	"The echo string to deem a plugin as successful"
	:group 'boop
	:type 'string)

(defcustom boop-success-colour "#63ca13"
	"The colour to use for success."
	:group 'boop
	:type 'string)

(defcustom boop-failure "0"
	"The echo string to deem a plugin as failed"
	:group 'boop
	:type 'string)

(defcustom boop-failure-colour "#c64512"
	"The colour to use for failure."
	:group 'boop
	:type 'string)

(defcustom boop-warning-colour "#ffcb13"
	"The colour to use for success."
	:group 'boop
	:type 'string)

(defcustom boop-monitor-symbol "●"
	"The symbol used as the monitor thing."
	:group 'boop
	:type 'string)

(defcustom boop-execution-strategy 'config
  "The startegy to use for executing plugins.

By default, with a value of `config` it will use
`boop-config-alist` and map each value in that to an execution of
a script in `boop-plugins-dir` with arguments.

Setting this value to `all` will run all the plugins in
`boop-plugins-dir` with no arguments.")

(defvar boop-plugins-dir (expand-file-name (concat (getenv "HOME") "/.boopelplugins"))
	"The default directory to read plugins from.")

(defvar boop-config-alist nil
	"An alist of scripts and the arguments to run them with.")

(setq boop-config-alist
      '((service-fuji jenkins "jenkins.connected-tv.tools.bbc.co.uk" "service-fuji")
        (contract-validation-int jenkins "jenkins.connected-tv.tools.bbc.co.uk" "taf-api-contract-validation-tests-on-int")
        ;; (jenkins "jenkins.connected-tv.tools.bbc.co.uk" "taf-router")
        (script pass "pass")))

(defvar boop-info-alist nil
	"This is an alist that is created based on the current
	`boop-config-alist` to keep the results synchronized when
	performing asynchronous actions")

(defvar boop-info nil
	"A variable to store the info produced by running the plugin scripts")

(defvar boop-timer nil
	"The value associated with the starting the boop timer so that
	it can be canceled.")

(defcustom boop-interval "10 sec"
  "The interval at which to run `boop-update`, it should be in
  the format that the elisp function `run-at-time` requires.

 e.g. 10 sec / 1 min"
  :group 'boop
	:type 'string)

;;;; Functions

(defun boop-build-string (&rest args)
  "Build the boop string by calling all of the functions in ARGS."
  (concat " " (mapconcat 'funcall args " ")))

(defun boop--get-plugin-scripts ()
	"Get a list of all of the executable scripts inside `boop-plugins-dir`."
	(let ((files (directory-files boop-plugins-dir t)))
		(--filter (and (file-executable-p it) (file-regular-p it)) files)))

(defun boop--get-plugin-alist ()
  "Get a list associating the file name base with the script path in `boop-plugins-dir`."
  (let ((plugins (boop--get-plugin-scripts)))
    (mapcar '(lambda (it) (cons (intern (file-name-base it)) it)) plugins)))

(defun boop-format-info ()
  "Format the info values"
  (mapconcat
   '(lambda (info)
      (cond ((string-equal (cdr info) boop-success) (boop--success))
            ((string-equal (cdr info) boop-failure) (boop--failure))
            (t (boop--warning)))) boop-info-alist " "))

(defun boop-update-info ()
  "Execute all of the plugins and return a list of the results."
	(let* ((plugins (boop--get-plugin-alist)))
	  (-map (lambda (config)
            (let* ((id (car config))
                   (script (cdr (assoc (cadr config) plugins)))
                   (args (mapconcat 'identity (cddr config) " "))
                   (cmd (format "%s %s" script args)))
              (async-start `(lambda () (shell-command-to-string ,cmd))
                           `(lambda (result)
                              (if (assoc (quote ,id) boop-info-alist)
                                  (setf (cdr (assoc (quote ,id) boop-info-alist)) (s-trim result))
                                (setq boop-info-alist (append (list (cons (quote ,id) (s-trim result))) boop-info-alist)))))))
          boop-config-alist)))

;; Interactive Functions

(defun boop-start ()
  "Start the boop timer executing your plugins."
  (interactive)
  (if (not boop-timer)
      (setq boop-timer (run-at-time "1 sec" (string-to-number boop-interval) 'boop-update-info))
    (error "You are already running BOOP - Call `boop-stop` to cancel")))

(defun boop-stop ()
  "Stop the boop timer executing."
  (interactive)
  (if boop-timer
      (progn (cancel-timer boop-timer) (setq boop-timer nil) (boop--clear-info-list))
    (error "You are not running BOOP - Call `boop-start` to begin")))

(defun boop--clear-info-list () (setq boop-info-alist nil))
(defun boop--success () (propertize boop-monitor-symbol 'face `(foreground-color . ,boop-success-colour)))
(defun boop--failure () (propertize boop-monitor-symbol 'face `(foreground-color . ,boop-failure-colour)))
(defun boop--warning () (propertize boop-monitor-symbol 'face `(foreground-color . ,boop-warning-colour)))

;; (run-at-time "10 sec" 10 'boop-execute-plugins)

;;; boop.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 1)
;; End:
