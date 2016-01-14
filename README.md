<img src="boop.png" width="1200">

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

### Features ###

* Jump to a file within the project
* Toggle between code and its test file ( `main.js` <=> `mainSpec.js` )
* Reformat a file with proper tabs/spaces indentation levels
* Project specific styling/formatting
* Create throw-away project instances from directories
* Switch between buffers in a project
* Per Instance Caching of projects for quick switching

# Installation #

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
