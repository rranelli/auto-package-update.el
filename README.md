<a href="http://github.com/rranelli/auto-package-update.el"><img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="Emacs Logo" width="80" height="80" align="right"></a>
## auto-package-update.el
*Automatically update Emacs packages.*

---
[![Build Status](https://travis-ci.org/rranelli/auto-package-update.el.svg?branch=master)](https://travis-ci.org/rranelli/auto-package-update.el)
[![MELPA](http://melpa.org/packages/auto-package-update-badge.svg)](http://melpa.org/#/auto-package-update)
[![MELPA Stable](http://stable.melpa.org/packages/auto-package-update-badge.svg)](http://stable.melpa.org/#/auto-package-update)

This package provides functionality for automatically updating your Emacs
packages periodically. It is specially useful for people that work in
multiple machines and tend to forget to manually update packages from time to
time.

The main idea is that you set a desired periodicity for the updates, and when
you start Emacs, the packages will be automatically updated if enough days
have passed since the last update.

### Requirements


This package was tested for GNU Emacs 24.4 and above. Older Emacsen are not
supported yet.

### Installation


You can install via `MELPA`, or manually by downloading `auto-package-update.el` and
adding the following to your init file:

```elisp
(add-to-list 'load-path "/path/to/auto-package-update")
(require 'auto-package-update)
```

### Usage


If `auto-package-update.el` is installed properly, you can add the following
line to your `.emacs`.

```elisp
(auto-package-update-maybe)
```

This will update your installed packages at startup if there is an update
pending.

You can register a check every day at a given time using `auto-package-update-at-time`:

```elisp
(auto-package-update-at-time "03:00")
```

will check for pending updates every three o'clock a.m..

You can also use the function `auto-package-update-now` to update your
packages immediatelly at any given time.

Or use `auto-package-update-now-async` without blocking Emacs. Since we
update packages after

```elisp
(package-refresh-contents :async)
```

we won't get all packages updated. The best practice is

```elisp
M-x package-refresh-contents
```

first, then use `auto-package-update-now-async`. Note, it's not 100% async,
byte compiling packages can still block Emacs.

### Customization


The periodicity (in days) of the update is given by the custom
variable `auto-package-update-interval`. The default interval is 7
days but if you want to change it, all you need is:

```elisp
(setq auto-package-update-interval 14)
```

Sometimes it is useful to skip an automatic update, e.g. when you're in a hurry
or don't have a working internet connection.
Use this setting to show a manual prompt before automatic updates:

```elisp
(setq auto-package-update-prompt-before-update t)
```

To delete residual old version directory when updating, set to
true variable `auto-package-update-delete-old-versions`. The
default value is `nil`. If you want to enable deleting:

```elisp
(setq auto-package-update-delete-old-versions t)
```

### Hooks


If you want to add functions to run *before* and *after* the package update, you can
use the `auto-package-update-before-hook` and `auto-package-update-after-hook` hooks.
For example:

```elisp
(add-hook 'auto-package-update-before-hook
          (lambda () (message "I will update packages now")))
```

### Changelog

1.7 - Add option to prompt user before running auto-package-update-maybe <br/>
1.6.1 - Replace deprecated `toggle-read-only` with `read-only-mode` to remove byte compile warnings. Thanx to @syohex. <br/>
1.6 - Add option to remove old packages from `.emacs.d/elpa` when updating. Thanks to @JesusMtnez. <br/>
1.5 - Allow user to check for updates every day at specified time. <br/>
1.4 - Add before and after update hooks. <br/>
1.3 - Do not break if a package is not available in the repositories.
      Show update results in a temporary buffer instead of the echo area<br/>
1.2 - Refactor for independence on package-menu functions. <br/>
1.1 - Support GNU Emacs 24.3. <br/>
1.0 - First release. <br/>



### Customization Documentation

#### `auto-package-update-interval`

Interval in DAYS for automatic package update.

#### `auto-package-update-before-hook`

List of functions to be called before running an automatic package update.

#### `auto-package-update-after-hook`

List of functions to be called after running an automatic package update.

#### `auto-package-update-last-update-day-filename`

Name of the file in which the last update day is going to be stored.

#### `auto-package-update-buffer-name`

Name of the buffer that shows updated packages and error after execution.

#### `auto-package-preview-buffer-name`

Name of the buffer that shows a preview of the packages to be updated.

#### `auto-package-update-delete-old-versions`

If not nil, delete old versions directories.

#### `auto-package-update-prompt-before-update`

Prompt user (y/n) before running auto-package-update-maybe

#### `auto-package-update-show-preview`

If not nil, show the list of packages to be updated when
prompting before running auto-package-update-maybe

#### `auto-package-update-hide-results`

If not nil, the result of auto package update in buffer
`auto-package-update-buffer-name` will not be shown.

#### `auto-package-update-excluded-packages`

List of packages to exclude from automatic package update.

### Function and Macro Documentation

#### `(auto-package-update-now &optional ASYNC)`

Update installed Emacs packages.

#### `(auto-package-update-now-async &optional FORCE)`

Update installed Emacs packages with an async manner.
If FORCE is non-nil, kill the update thread anyway.

#### `(auto-package-update-at-time TIME)`

Try to update every day at the specified TIME.

#### `(auto-package-update-maybe)`

Update installed Emacs packages if at least
`auto-package-update-interval` days have passed since the last
update.

-----
<div style="padding-top:15px;color: #d0d0d0;">
Markdown README file generated by
<a href="https://github.com/mgalgs/make-readme-markdown">make-readme-markdown.el</a>
</div>
