<img src="logo.png" width="1200">

# boop.el #

**Boop** Is a tool designed to radiate state unobtrusively in the
`mode-line` _(or wherever you want it)_ whilst you're working.

Very often you see dashboards littered across the floors of start ups
and companies a like designed to track the state of a service, usually
**red**, **green**, **yellow** or **blue** widgets which tell you
something is either _broken_ or _stable_ or some other state.

The problem is, these dashboards often get saturated and all the loud
colours become noise.

**Boop** is designed to replicate the purpose of a dashboard from
within **Emacs**, you can write plugin scripts and configure
**Boop** to run these scripts periodically and print coloured dots
in your `mode-line` to represent how that script returned.

<img src="modeline.png" width="1200">

## Installation ##

Clone this repository and add it to Emac's `load-path`. Then require
the file:

```
(add-to-list 'load-path "/path/to/cloned/repo")
(require 'boop)
```

Then start boop by running
```
(boop-start)
```

## Defining a Config

In order to start tracking things using **Boop** you will need to do one of two things,

* Set `boop-config-alist`
* Add to `boop-update-hook`

### `boop-config-alist` ###

This variable defines items to monitor and the plugin scripts used to
monitor, each entry in the `alist` is made of an `id` and a
`plist`.

The `plist` should contain a;

* `:script` symbol which matches a **plugin** name,
* `&optional :args` which is a list of string arguments to be passed into the `script`
* `&optional :group` a group symbol to associate this config item with
* `&optional :onselect` a function which will be evaluated when the boop is clicked

For example,
```elisp
(setq boop-config-alist
    ;; ID                   PLIST
    '((contract-validation  :script examplejenkins
                            :args ("my-jenkins.co.uk" "contract-validation")
                            :group backend
                            :onselect (lambda () (interactive) 
										(browse-url "http://my-jenkins.co.uk/job/contract-validation")))

     (service-js             :script examplejenkins
                            :args ("my-jenkins.co.uk" "service-js")
                            :group backend)

     (html-client            :script status
                            :onselect (lambda () (interactive) (message "Yes this is working")))))
```

### `boop-update-hook`

This hook is run whenever `boop-update-info` is called. When the hooks run, it is expected that they will end by calling the `boop` function.

For example,
```elisp
(add-hook 'boop-update-hook
;;                  BOOP ID                               PASS STATUS   UNKNOWN STATUS
 '(lambda () (boop 'project-open (if (boundp 'project-id) 1             2))))
```

## Plugins

Plugins are used to delegate the heavy lifting of the
monitoring. These scripts should _echo_ or _print_ the result that
matches the formats you define in `boop-format-alist`.

By default, plugins are found in `~/.boopelplugins`, but this
directory can be customised by setting the variable `boop-plugins-dir`.
