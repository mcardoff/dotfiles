;;; explain-pause-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "explain-pause-mode" "explain-pause-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from explain-pause-mode.el

(defvar explain-pause-mode nil "\
Non-nil if explain-pause mode is enabled.
See the `explain-pause-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `explain-pause-mode'.")

(custom-autoload 'explain-pause-mode "explain-pause-mode" nil)

(autoload 'explain-pause-mode "explain-pause-mode" "\
Toggle whether to attempt to discover and explain pauses in emacs.

This is a minor mode.  If called interactively, toggle the
`explain-pause mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the
mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value 'explain-pause-mode)'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

When enabled, explain-pause will attempt to time how long blocking activity
takes. If it measures blocking work that takes longer then a configurable
amount of time, explain-pause logs contextual information that can be used
to help diagnose and propose areas of elisp that might affect emacs
interactivity.

When blocking work takes too long many times, explain-mode profiles the
blocking work using the builtin Emacs profiler (`profiler' package). A fixed
number of these are saved.

This mode hooks `call-interactively', both idle and regular timers, and process
filters and sentinels.

When running interactively, e.g. run from `M-x' or similar, `explain-pause-mode'
must install itself after some time while Emacs is not doing anything.

\(fn &optional ARG)" t nil)

(autoload 'explain-pause-top "explain-pause-mode" "\
Show a top-like report of commands recently ran and their runtime. Returns
the buffer." t nil)

(autoload 'explain-pause-log-to-socket "explain-pause-mode" "\
Log the event stream to a UNIX file socket, FILE-SOCKET. If FILE-SOCKET is nil,
then the default location `explain-pause-default-log' is used. This file socket
should already exist. It might be created by `explain-pause-socket' in another
Emacs process, in which case `explain-mode-top-from-socket' will receive and
present that data. Or you can simply receive the data in any other process that
can create UNIX sockets, for example `netcat'.To turn off logging, run
`explain-pause-log-off'.

The stream is written as newline delimited elisp readable lines. See
`explain-pause-log--send-*' family of commands for the format of those objects.

Returns the process that is connected to the socket.

\(fn &optional FILE-SOCKET)" t nil)

(register-definition-prefixes "explain-pause-mode" '("explain-"))

;;;***

(provide 'explain-pause-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; explain-pause-mode-autoloads.el ends here
