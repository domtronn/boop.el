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
;; TODO: Add in Grouping of configs?
;; TODO: Add in click behaviour

;;;; Customs

(require 'async)
(require 'dash)
(require 's)

(defgroup boop nil
  "Manage how boop gets and displays monitoring results."
  :prefix "boop-"
  :group 'tools
  :group 'convenience)

(defcustom boop-default-format '(:symbol ?● :color "#ffcb13" :status "Unknown")
  "The default format when a script echo result doesn't match a value in the `boop-format-alist`.")

(defcustom boop-format-alist
  '((1 :symbol ?● :color "#63ca13" :status "Pass")
    (0 :symbol ?● :color "#c64512" :status "Fail")
    (3 :symbol ?● :color "#23a2fb" :status "Building"))
   "An alist to map the echo results of the plugins to a display format.

The alist is an INTEGER status id that is expected to be returned from a plugin
script, the `cdr` of this list is a plist with the following properties.

:symbol - The character symbol to be displayed for that
          result (these can be emojis)
:color  - &optional The HEX color to render this text in
          (this has no effect on emojis)
:status - &optional A string representation of the status
          (used to filter items by natural language)
:action - &optional A function to be called when this form status
          is changed to.
          e.g.  If a boop has a status of 1 then
          changes to a status of 2 which has an action")

(defcustom boop-execution-strategy 'config
  "The startegy to use for executing plugins.

By default, with a value of `config` it will use
`boop-config-alist` and map each value in that to an execution of
a script in `boop-plugins-dir` with arguments.

Setting this value to `all` will run all the plugins in
`boop-plugins-dir` with no arguments.")

(defvar boop-update-hook nil
  "A list of hooks to run when `boop-update-info` runs.

This allows you to programmatically create/update boops on an
interval.  If you have anything you want to monitor using an
elisp function, add that function to this list.")

(defvar boop-plugins-dir (expand-file-name (concat (getenv "HOME") "/.boopelplugins"))
  "The default directory to read plugins from.")

(defvar boop-config-alist nil
  "An alist of scripts and the arguments to run them with.

Each entry Should be in the format (ID SCRIPT_NAME &optional ARGS)")

(defvar boop-result-alist nil
  "This is an alist to track the results of plugins and boops.

It is created based on the current `boop-config-alist` to keep the
results synchronized when performing asynchronous actions")

(defvar boop-timer nil
  "The id of the boop timer so that it can be canceled.")

(defcustom boop-interval 10
  "The interval at which to run `boop-update`.

It should be in the format that the elisp function `run-at-time` requires.

 e.g. 10 sec / 1 min"
  :group 'boop
  :type 'string)

(defcustom boop-sort-func nil
  "Comparator function used to sort `boop-result-alist` when being formatted.

When non-nil, `boop-sort-func` should be a comparator funciton
which takes two arguments, When set to nil, no sorting is
applied.")

(defvar boop--format-func 'boop--format-result "The function to use to format results.")
(defvar boop--shorten-id-func 'boop--shorten-substring "The function used to shorten an ID string.")
;;;; Functions

(defun boop--get-plugin-scripts ()
  "Get a list of all of the executable scripts inside `boop-plugins-dir`."
  (let ((files (directory-files boop-plugins-dir t)))
    (--filter (and (file-executable-p it) (file-regular-p it)) files)))

(defun boop--get-plugin-alist ()
  "Get a list associating the file name base with the script path in `boop-plugins-dir`."
  (let ((plugins (boop--get-plugin-scripts)))
    (mapcar (lambda (it) (cons (intern (file-name-base it)) it)) plugins)))

;; Formatting and Renderings the results

(defun boop-format-results ()
  "Format `boop-result-alist` into a propertized display string."
  (let ((sorted-result-alist
         (if boop-sort-func (--sort (funcall boop-sort-func (cdr it) (cdr other)) boop-result-alist)
           boop-result-alist)))
    (mapconcat boop--format-func sorted-result-alist "")))

(defun boop--format-result-as-id (result)
  "Format an individual RESULT using its ID."
  (let* ((form (or (cdr (assoc (cdr result) boop-format-alist))
                   boop-default-format))
         (map-symbol (intern (format "boop-%s-mode-line-map" (car result))))
         (string-id (format "%s" (car result)))
         (short-id (funcall boop--shorten-id-func string-id)))

    (format "[%s] " (boop--propertize form string-id (when (boundp map-symbol) map-symbol) short-id))))

(defun boop--format-result (result)
  "Format an individual RESULT normally."
  (let ((form (or (cdr (assoc (cdr result) boop-format-alist))
                  boop-default-format))
        (map-symbol (intern (format "boop-%s-mode-line-map" (car result)))))
    (boop--propertize form (format "%s" (car result)) (when (boundp map-symbol) map-symbol))))

(defun boop--propertize (form &optional help-echo map symbol-override)
  "Propertizes FORM with optional HELP-ECHO string.

FORM should be a plist containing, at minimum, a :symbol to render.
See `boop-format-alist` for examples of what these FORMs should look like.
You can override the symbol in FORM using SYMBOL-OVERRIDE."
  (let ((symbol (plist-get form :symbol))
        (colour (plist-get form :color)))
    (propertize (if symbol-override (format "%s" symbol-override) (format "%c " symbol))
                'face      `(foreground-color . ,colour)
                'help-echo help-echo
                'local-map (eval map)
                'pointer (when map 'hand))))

;; Click bindings

(defmacro boop-defmodelinemap (id f)
  "Macro to create a mode-line-map-by-id."
  (let ((id (intern (format "boop-%s-mode-line-map" id ))))
    `(defvar ,id (let ((map (make-sparse-keymap)))
                   (define-key map [mode-line down-mouse-1] ,f) map))))

;; Shorten functions

(defun boop--shorten-substring (id)
  "Formats an ID string to be the substring of up to 5 characters."
  (substring id 0 (min (length id) 5)))

(defun boop--shorten-delim (id)
  "Formats an ID string to be the substring of up to 5 characters."
  (downcase (mapconcat (lambda (s) (substring s 0 1)) (s-split-words id) "-")))

;; Updating the results

(defun boop--clear-result-list () "Clear the result list." (setq boop-result-alist nil))
(defun boop--sync-result-and-config ()
  "Synchronizes the result alist to contain only items in the config alist."
  (setq boop-result-alist (--filter (assoc (car it) boop-config-alist) boop-result-alist)))

(defun boop--handle-result (id result)
  "Update the resut with ID by handling the RESULT of the async `shell-command-to-string`."
  (if (assoc id boop-result-alist)
      (boop--update-result id result)
    (setq boop-result-alist (append (list (cons id result)) boop-result-alist))))

(defun boop--update-result (id result)
  "Update the boop with ID to have the new RESULT.

Updating the result will also trigger any actions associated with that RESULT form."
  (let* ((current-result (cdr (assoc id boop-result-alist)))
         (form (or (cdr (assoc result boop-format-alist))
                   boop-default-format))
         (name (car (assoc id boop-result-alist)))
         (action (plist-get form :action)))
    (setf (cdr (assoc id boop-result-alist)) result)
    (when (and (not (eq current-result result)) action)
      (funcall (if (symbolp action)
                   (symbol-function action)
               action) name))))

;; Interactive Functions

(defun boop-update-info ()
  "Execute all of the plugins and return a list of the results."
  (interactive)
  (let* ((plugins (boop--get-plugin-alist)))
    (boop--sync-result-and-config)
    (run-hooks 'boop-update-hook)
    (-map (lambda (config)
            (let* ((id (car config))
                   (click (plist-get (cdr config) :onclick))
                   (script (cdr (assoc (plist-get (cdr config) :script) plugins)))
                   (args (mapconcat 'identity (plist-get (cdr config) :args) " ")))
              ;; Set up the :onclick events
              (when click (eval `(boop-defmodelinemap ,id click)))
              ;; Only boop the configs with scripts
              (when script
                (async-start `(lambda () (shell-command-to-string (format "%s %s" ,script ,args)))
                             `(lambda (result) (boop--handle-result (quote ,id) (string-to-number result)))))))
          boop-config-alist)))

(defun boop-flash-result-ids ()
  "Flashes the results based on their ID for 5 seconds."
  (interactive)
  (let ((previous-format-func boop--format-func))
    (setq boop--format-func 'boop--format-result-as-id)
    (run-at-time "2 sec" nil `(lambda () (setq boop--format-func (quote ,previous-format-func))))))

(defun boop (id status)
  "Manually boop something and set ID to have a status of STATUS."
  (if (assoc id boop-result-alist)
      (boop--update-result id status)
    (progn
      ;; Add the new boop to the config and results
      (setq boop-config-alist (append (list (list id)) boop-config-alist))
      (setq boop-result-alist (append (list (cons id status)) boop-result-alist)))))

(defun deboop (&optional id)
  "Remove boop with ID from `boop-config-alist` and sync with the results."
  (interactive)
  (let ((id (or id (intern (completing-read "Deboop: " boop-result-alist)))))
    (setq boop-config-alist (assq-delete-all id boop-config-alist))
    (boop--sync-result-and-config)))

(defun beep-boop ()
  "List all of the boops with `STATUS`."
  (interactive)
  (let* ((status-alist (append (--map (cons (plist-get (cdr it) :status) (car it)) boop-format-alist)
                               (list (list (plist-get boop-default-format :status)))))
         (status (assoc (completing-read "Status: " status-alist) status-alist))
         (with-status
          (--map (format "%s" (car it))
                 (if (equal (car status) (plist-get boop-default-format :status))
                     (--filter (not (-contains? (-map 'car boop-format-alist) (cdr it))) boop-result-alist)
                     (--filter (equal (cdr it) (cdr status)) boop-result-alist)))))
    (if with-status
        (message "%s Boops: %s" (car status) (mapconcat (lambda (it) (format "[ %s ]" it)) with-status " "))
      (message "No Boops have a status of [%s]" (car status)))))

;;;###autoload
(defun boop-start ()
  "Start the boop timer executing your plugins."
  (interactive)
  (if (not boop-timer)
      (setq boop-timer (run-at-time "1 sec" boop-interval 'boop-update-info))
    (error "You are already running BOOP - Call `boop-stop` to cancel")))

;;;###autoload
(defun boop-stop ()
  "Stop the boop timer executing."
  (interactive)
  (boop--clear-result-list)
  (if boop-timer
      (progn (cancel-timer boop-timer) (setq boop-timer nil))
    (error "You are not running BOOP - Call `boop-start` to begin")))

;; (run-at-time "10 sec" 10 'boop-execute-plugins)

;;; boop.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; eval: (nameless-mode 1)
;; End:
