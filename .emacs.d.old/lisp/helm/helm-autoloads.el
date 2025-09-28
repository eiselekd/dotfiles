;;; helm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm"
;;;;;;  "helm.el" "95737de058a3f79135d4e4b0c450b59d")
;;; Generated autoloads from helm.el

(autoload 'helm-define-multi-key "../../../git/dotfiles/.emacs.d/lisp/helm/helm" "\
In KEYMAP, define key sequence KEY for function list FUNCTIONS.
Each function runs sequentially for each KEY press. 
If DELAY is specified, switch back to initial function of FUNCTIONS list
after DELAY seconds.
The functions in FUNCTIONS list take no args.
e.g
  (defun foo ()
    (message \"Run foo\"))
  (defun bar ()
    (message \"Run bar\"))
  (defun baz ()
    (message \"Run baz\"))

\(helm-define-multi-key global-map \"<f5> q\" '(foo bar baz) 2)

Each time \"<f5> q\" is pressed, the next function is executed. Waiting 
more than 2 seconds between key presses switches back to executing the first 
function on the next hit.

\(fn KEYMAP KEY FUNCTIONS &optional DELAY)" nil nil)

(autoload 'helm-multi-key-defun "../../../git/dotfiles/.emacs.d/lisp/helm/helm" "\
Define NAME as a multi-key command running FUNS.
After DELAY seconds, the FUNS list is reinitialized.
See `helm-define-multi-key'.

\(fn NAME DOCSTRING FUNS &optional DELAY)" nil t)

(put 'helm-multi-key-defun 'lisp-indent-function '2)

(autoload 'helm-define-key-with-subkeys "../../../git/dotfiles/.emacs.d/lisp/helm/helm" "\
Defines in MAP a KEY and SUBKEY to COMMAND.

This allows typing KEY to call COMMAND the first time and
type only SUBKEY on subsequent calls.

Arg MAP is the keymap to use, SUBKEY is the initial short key-binding to
call COMMAND.

Arg OTHER-SUBKEYS is an alist specifying other short key-bindings
to use once started.
e.g:

\(helm-define-key-with-subkeys global-map
   (kbd \"C-x v n\") ?n 'git-gutter:next-hunk '((?p . git-gutter:previous-hunk)))


In this example, `C-x v n' will run `git-gutter:next-hunk'
subsequent \"n\"'s run this command again
and subsequent \"p\"'s run `git-gutter:previous-hunk'.

Arg MENU is a string displayed in minibuffer that 
describes SUBKEY and OTHER-SUBKEYS.
Arg EXIT-FN specifies a function to run on exit.

For any other keys pressed, run their assigned command as defined
in MAP and then exit the loop running EXIT-FN, if specified.

NOTE: SUBKEY and OTHER-SUBKEYS bindings support char syntax only 
\(e.g ?n), so don't use strings or vectors to define them.

\(fn MAP KEY SUBKEY COMMAND &optional OTHER-SUBKEYS MENU EXIT-FN)" nil nil)

(put 'helm-define-key-with-subkeys 'lisp-indent-function '1)

(autoload 'helm-debug-open-last-log "../../../git/dotfiles/.emacs.d/lisp/helm/helm" "\
Open helm log file of last helm session.
If `helm-last-log-file' is nil, switch to `helm-debug-buffer' .

\(fn)" t nil)

(autoload 'helm "../../../git/dotfiles/.emacs.d/lisp/helm/helm" "\
Main function to execute helm sources.

Keywords supported:
:sources :input :prompt :resume :preselect
:buffer :keymap :default :history :allow-nest

Extra LOCAL-VARS keywords are supported, see below.

PLIST is a list like (:key1 val1 :key2 val2 ...) or
\(&optional sources input prompt resume
            preselect buffer keymap default history).

Basic keywords are the following:

:sources

A list of sources used for this session.  It also accepts a
symbol, interpreted as a variable of a helm source
i.e (a symbol can be passed instead of a list of sources).
It also accepts an alist representing a helm source, which is
detected by (assq 'name ANY-SOURCES).
NOTE: In this case the source is embedded in the helm command and
have no symbol name, so it is not reachable from outside.
It will be referenced in `helm-sources' as a whole alist.

:input

Temporary value of `helm-pattern', ie. initial input of minibuffer.

:prompt

Prompt other than \"pattern: \".

:resume

If t, Resurrect previously instance of `helm'.  Skip the initialization.
If 'noresume, this instance of `helm' cannot be resumed.

:preselect

Initially selected candidate.  Specified by exact candidate or a regexp.

:buffer

`helm-buffer' instead of *helm*.

:keymap

`helm-map' for current `helm' session.

:default

A default argument that will be inserted in minibuffer with
\\<minibuffer-local-map>\\[next-history-element]. When nil or not
present `thing-at-point' will be used instead. If
`helm--maybe-use-default-as-input' is non-`nil' display will be
updated using :default arg as input unless :input is specified,
which in this case will take precedence over :default. This is a
string or a list. If list, car of the list becomes initial
default input. \\<minibuffer-local-map>\\[next-history-element]
cycles through the list items.

:history

Minibuffer input, by default, is pushed to `minibuffer-history'.
When an argument HISTORY is provided, input is pushed to
HISTORY. The HISTORY element should be a valid symbol.

:allow-nest

Allow running this helm command in a running helm session.

Standard arguments are supported. These two are the same:

\(helm :sources sources :input input :prompt prompt :resume resume
       :preselect preselect :buffer buffer :keymap keymap :default default
       :history history)

and

\(helm sources input prompt resume preselect buffer keymap default history)

are the same for now. However, the use of non-keyword args is
deprecated and should not be used.

Other keywords are interpreted as local variables of this helm
session. The `helm-' prefix can be omitted. For example,

\(helm :sources 'helm-source-buffers-list
       :buffer \"*helm buffers*\" :candidate-number-limit 10)

starts helm session with `helm-source-buffers' source in
*helm buffers* buffer and sets variable `helm-candidate-number-limit'
to 10 as a session local variable.

\(fn &key SOURCES INPUT PROMPT RESUME PRESELECT BUFFER KEYMAP DEFAULT HISTORY ALLOW-NEST OTHER-LOCAL-VARS)" nil nil)

(autoload 'helm-other-buffer "../../../git/dotfiles/.emacs.d/lisp/helm/helm" "\
Simplified `helm' interface with other `helm-buffer'.
Call `helm' only with ANY-SOURCES and ANY-BUFFER as args.

\(fn ANY-SOURCES ANY-BUFFER)" nil nil)

(autoload 'helm-debug-toggle "../../../git/dotfiles/.emacs.d/lisp/helm/helm" "\
Enable/disable helm debugging from outside of helm session.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-adaptive"
;;;;;;  "helm-adaptive.el" "c28ae029de9ed1c33aa747fef7efd11a")
;;; Generated autoloads from helm-adaptive.el

(defvar helm-adaptive-mode nil "\
Non-nil if Helm-Adaptive mode is enabled.
See the command `helm-adaptive-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-adaptive-mode'.")

(custom-autoload 'helm-adaptive-mode "../../../git/dotfiles/.emacs.d/lisp/helm/helm-adaptive" nil)

(autoload 'helm-adaptive-mode "../../../git/dotfiles/.emacs.d/lisp/helm/helm-adaptive" "\
Toggle adaptive sorting in all sources.

\(fn &optional ARG)" t nil)

(autoload 'helm-reset-adaptive-history "../../../git/dotfiles/.emacs.d/lisp/helm/helm-adaptive" "\
Delete all `helm-adaptive-history' and his file.
Useful when you have a old or corrupted `helm-adaptive-history-file'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-apt"
;;;;;;  "helm-apt.el" "2ef814bb5e4dd01f5cdf4ed543beae1a")
;;; Generated autoloads from helm-apt.el

(autoload 'helm-apt "../../../git/dotfiles/.emacs.d/lisp/helm/helm-apt" "\
Preconfigured `helm' : frontend of APT package manager.
With a prefix arg reload cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-bookmark"
;;;;;;  "helm-bookmark.el" "a4d0ec242040c91b342a4974df3dccd6")
;;; Generated autoloads from helm-bookmark.el

(autoload 'helm-bookmarks "../../../git/dotfiles/.emacs.d/lisp/helm/helm-bookmark" "\
Preconfigured `helm' for bookmarks.

\(fn)" t nil)

(autoload 'helm-filtered-bookmarks "../../../git/dotfiles/.emacs.d/lisp/helm/helm-bookmark" "\
Preconfigured helm for bookmarks (filtered by category).
Optional source `helm-source-bookmark-addressbook' is loaded
only if external library addressbook-bookmark.el is available.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-buffers"
;;;;;;  "helm-buffers.el" "f5fd51e381587afad0f4727b0f11caa2")
;;; Generated autoloads from helm-buffers.el

(autoload 'helm-buffers-list "../../../git/dotfiles/.emacs.d/lisp/helm/helm-buffers" "\
Preconfigured `helm' to list buffers.

\(fn)" t nil)

(autoload 'helm-mini "../../../git/dotfiles/.emacs.d/lisp/helm/helm-buffers" "\
Preconfigured `helm' lightweight version (buffer -> recentf).

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-color"
;;;;;;  "helm-color.el" "e13cedea73698afd3b8ffff8e42cbc20")
;;; Generated autoloads from helm-color.el

(autoload 'helm-colors "../../../git/dotfiles/.emacs.d/lisp/helm/helm-color" "\
Preconfigured `helm' for color.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-command"
;;;;;;  "helm-command.el" "d59bba8f7cd033fa5f554fe3f8dd5a1f")
;;; Generated autoloads from helm-command.el

(autoload 'helm-M-x "../../../git/dotfiles/.emacs.d/lisp/helm/helm-command" "\
Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x' `execute-extended-command'.

Unlike regular `M-x' emacs vanilla `execute-extended-command' command,
the prefix args if needed, are passed AFTER starting `helm-M-x'.

You can get help on each command by persistent action.

\(fn ARG &optional COMMAND-NAME)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-config"
;;;;;;  "helm-config.el" "ab4879f7e129f6ba95e0d8c1877b5cc0")
;;; Generated autoloads from helm-config.el

(autoload 'helm-configuration "../../../git/dotfiles/.emacs.d/lisp/helm/helm-config" "\
Customize `helm'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-dabbrev"
;;;;;;  "helm-dabbrev.el" "9fdf1f96e0c14943abe84e171e3a96f8")
;;; Generated autoloads from helm-dabbrev.el

(autoload 'helm-dabbrev "../../../git/dotfiles/.emacs.d/lisp/helm/helm-dabbrev" "\
Preconfigured helm for dynamic abbreviations.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elisp"
;;;;;;  "helm-elisp.el" "0954bb1f4759ed3eac37021bd02eb87d")
;;; Generated autoloads from helm-elisp.el

(autoload 'helm-lisp-completion-at-point "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elisp" "\
Preconfigured helm for lisp symbol completion at point.

\(fn)" t nil)

(autoload 'helm-complete-file-name-at-point "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elisp" "\
Preconfigured helm to complete file name at point.

\(fn &optional FORCE)" t nil)

(autoload 'helm-lisp-indent "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elisp" "\


\(fn)" t nil)

(autoload 'helm-lisp-completion-or-file-name-at-point "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elisp" "\
Preconfigured helm to complete lisp symbol or filename at point.
Filename completion happen if string start after or between a double quote.

\(fn)" t nil)

(autoload 'helm-apropos "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elisp" "\
Preconfigured helm to describe commands, functions, variables and faces.
In non interactives calls DEFAULT argument should be provided as a string,
i.e the `symbol-name' of any existing symbol.

\(fn DEFAULT)" t nil)

(autoload 'helm-manage-advice "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elisp" "\
Preconfigured `helm' to disable/enable function advices.

\(fn)" t nil)

(autoload 'helm-locate-library "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elisp" "\
Preconfigured helm to locate elisp libraries.

\(fn)" t nil)

(autoload 'helm-timers "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elisp" "\
Preconfigured `helm' for timers.

\(fn)" t nil)

(autoload 'helm-complex-command-history "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elisp" "\
Preconfigured helm for complex command history.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elisp-package"
;;;;;;  "helm-elisp-package.el" "7e339ecbf7a8f6f929cbcddcd108aceb")
;;; Generated autoloads from helm-elisp-package.el

(autoload 'helm-list-elisp-packages "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elisp-package" "\
Preconfigured helm for listing and handling emacs packages.

\(fn ARG)" t nil)

(autoload 'helm-list-elisp-packages-no-fetch "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elisp-package" "\
Preconfigured helm for emacs packages.
Same as `helm-list-elisp-packages' but don't fetch packages on remote.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elscreen"
;;;;;;  "helm-elscreen.el" "4830916b7c7f685fe4b6d38b1e9c18af")
;;; Generated autoloads from helm-elscreen.el

(autoload 'helm-elscreen "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elscreen" "\
Preconfigured helm to list elscreen.

\(fn)" t nil)

(autoload 'helm-elscreen-history "../../../git/dotfiles/.emacs.d/lisp/helm/helm-elscreen" "\
Preconfigured helm to list elscreen in history order.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-eshell"
;;;;;;  "helm-eshell.el" "0a8240b093a06193ebb4fe6321d56cc3")
;;; Generated autoloads from helm-eshell.el

(autoload 'helm-esh-pcomplete "../../../git/dotfiles/.emacs.d/lisp/helm/helm-eshell" "\
Preconfigured helm to provide helm completion in eshell.

\(fn)" t nil)

(autoload 'helm-eshell-history "../../../git/dotfiles/.emacs.d/lisp/helm/helm-eshell" "\
Preconfigured helm for eshell history.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-eval"
;;;;;;  "helm-eval.el" "22c099dd0e67933d9c42b1c0d7c85fab")
;;; Generated autoloads from helm-eval.el

(autoload 'helm-eval-expression "../../../git/dotfiles/.emacs.d/lisp/helm/helm-eval" "\
Preconfigured helm for `helm-source-evaluation-result'.

\(fn ARG)" t nil)

(autoload 'helm-eval-expression-with-eldoc "../../../git/dotfiles/.emacs.d/lisp/helm/helm-eval" "\
Preconfigured helm for `helm-source-evaluation-result' with `eldoc' support. 

\(fn)" t nil)

(autoload 'helm-calcul-expression "../../../git/dotfiles/.emacs.d/lisp/helm/helm-eval" "\
Preconfigured helm for `helm-source-calculation-result'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-external"
;;;;;;  "helm-external.el" "eca08ee0da291bb844a30b586f4f304b")
;;; Generated autoloads from helm-external.el

(autoload 'helm-run-external-command "../../../git/dotfiles/.emacs.d/lisp/helm/helm-external" "\
Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`helm-external-commands-list'.

\(fn PROGRAM)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-files"
;;;;;;  "helm-files.el" "669dbf2e38ce99408b8ba824da827a3e")
;;; Generated autoloads from helm-files.el

(autoload 'helm-browse-project "../../../git/dotfiles/.emacs.d/lisp/helm/helm-files" "\
Preconfigured helm to browse projects.
Browse files and see status of project with its vcs.
Only HG and GIT are supported for now.
Fall back to `helm-browse-project-find-files'
if current directory is not under control of one of those vcs.
With a prefix ARG browse files recursively, with two prefix ARG
rebuild the cache.
If the current directory is found in the cache, start
`helm-browse-project-find-files' even with no prefix ARG.
NOTE: The prefix ARG have no effect on the VCS controlled directories.

Needed dependencies for VCS:
<https://github.com/emacs-helm/helm-ls-git>
and
<https://github.com/emacs-helm/helm-ls-hg>
and
<http://melpa.org/#/helm-ls-svn>.

\(fn ARG)" t nil)

(autoload 'helm-find "../../../git/dotfiles/.emacs.d/lisp/helm/helm-files" "\
Preconfigured `helm' for the find shell command.

\(fn ARG)" t nil)

(autoload 'helm-find-files "../../../git/dotfiles/.emacs.d/lisp/helm/helm-files" "\
Preconfigured `helm' for helm implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `helm-find-files-1' instead.
This is the starting point for nearly all actions you can do on files.

\(fn ARG)" t nil)

(autoload 'helm-for-files "../../../git/dotfiles/.emacs.d/lisp/helm/helm-files" "\
Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-preferred-list'.

\(fn)" t nil)

(autoload 'helm-multi-files "../../../git/dotfiles/.emacs.d/lisp/helm/helm-files" "\
Preconfigured helm similar to `helm-for-files' but that don't run locate.
Allow toggling from locate to others sources.
This allow seeing first if what you search is in other sources before launching
locate.

\(fn)" t nil)

(autoload 'helm-recentf "../../../git/dotfiles/.emacs.d/lisp/helm/helm-files" "\
Preconfigured `helm' for `recentf'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-font"
;;;;;;  "helm-font.el" "536e16431188f80eeb261006813338de")
;;; Generated autoloads from helm-font.el

(autoload 'helm-select-xfont "../../../git/dotfiles/.emacs.d/lisp/helm/helm-font" "\
Preconfigured `helm' to select Xfont.

\(fn)" t nil)

(autoload 'helm-ucs "../../../git/dotfiles/.emacs.d/lisp/helm/helm-font" "\
Preconfigured helm for `ucs-names' math symbols.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-grep"
;;;;;;  "helm-grep.el" "6821dc15abf4f02b69f4cbbd5c031c36")
;;; Generated autoloads from helm-grep.el

(autoload 'helm-goto-precedent-file "../../../git/dotfiles/.emacs.d/lisp/helm/helm-grep" "\
Go to precedent file in helm grep/etags buffers.

\(fn)" t nil)

(autoload 'helm-goto-next-file "../../../git/dotfiles/.emacs.d/lisp/helm/helm-grep" "\
Go to precedent file in helm grep/etags buffers.

\(fn)" t nil)

(autoload 'helm-do-grep-ag "../../../git/dotfiles/.emacs.d/lisp/helm/helm-grep" "\
Preconfigured helm for grepping with AG in `default-directory'.
With prefix-arg prompt for type if available with your AG version.

\(fn ARG)" t nil)

(autoload 'helm-grep-do-git-grep "../../../git/dotfiles/.emacs.d/lisp/helm/helm-grep" "\
Preconfigured helm for git-grepping `default-directory'.
With a prefix arg ARG git-grep the whole repository.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-help"
;;;;;;  "helm-help.el" "98a80a3a9f19396a575a622d09c3871c")
;;; Generated autoloads from helm-help.el

(autoload 'helm-documentation "../../../git/dotfiles/.emacs.d/lisp/helm/helm-help" "\
Preconfigured helm for helm documentation.
With a prefix arg refresh the documentation.

Find here the documentation of all sources actually documented.

\(fn ARG)" t nil)

(defvar helm-comp-read-mode-line "\\<helm-comp-read-map>C/\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-help]:Help \\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(defvar helm-read-file-name-mode-line-string "\\<helm-read-file-map>\\[helm-help]:Help C/\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend" "\
String displayed in mode-line in `helm-source-find-files'.")

(defvar helm-top-mode-line "\\<helm-top-map>\\[helm-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(autoload 'helm-describe-helm-attribute "../../../git/dotfiles/.emacs.d/lisp/helm/helm-help" "\
Display the full documentation of HELM-ATTRIBUTE.
HELM-ATTRIBUTE should be a symbol.

\(fn HELM-ATTRIBUTE)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-id-utils"
;;;;;;  "helm-id-utils.el" "41b705c5f04021afe24191f1581b2076")
;;; Generated autoloads from helm-id-utils.el

(autoload 'helm-gid "../../../git/dotfiles/.emacs.d/lisp/helm/helm-id-utils" "\
Preconfigured helm for `gid' command line of `ID-Utils'.
Need A database created with the command `mkid'
above `default-directory'.
Need id-utils as dependency which provide `mkid', `gid' etc...
See <https://www.gnu.org/software/idutils/>.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-imenu"
;;;;;;  "helm-imenu.el" "e0c2217b97dfff49998c18a70925154d")
;;; Generated autoloads from helm-imenu.el

(autoload 'helm-imenu "../../../git/dotfiles/.emacs.d/lisp/helm/helm-imenu" "\
Preconfigured `helm' for `imenu'.

\(fn)" t nil)

(autoload 'helm-imenu-in-all-buffers "../../../git/dotfiles/.emacs.d/lisp/helm/helm-imenu" "\
Preconfigured helm for fetching imenu entries of all buffers.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-info"
;;;;;;  "helm-info.el" "86690ceefc08197f24ad82573d5e255b")
;;; Generated autoloads from helm-info.el

(autoload 'helm-info "../../../git/dotfiles/.emacs.d/lisp/helm/helm-info" "\
Preconfigured `helm' for searching Info files' indices.

\(fn)" t nil)

(autoload 'helm-info-at-point "../../../git/dotfiles/.emacs.d/lisp/helm/helm-info" "\
Preconfigured `helm' for searching info at point.
With a prefix-arg insert symbol at point.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-locate"
;;;;;;  "helm-locate.el" "5f1654c9bafcf5514736c5dbec37d2b7")
;;; Generated autoloads from helm-locate.el

(autoload 'helm-projects-find-files "../../../git/dotfiles/.emacs.d/lisp/helm/helm-locate" "\
Find files with locate in `helm-locate-project-list'.
With a prefix arg refresh the database in each project.

\(fn UPDATE)" t nil)

(autoload 'helm-locate "../../../git/dotfiles/.emacs.d/lisp/helm/helm-locate" "\
Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options and also `helm-locate-command'.

You can specify a local database with prefix argument ARG.
With two prefix arg, refresh the current local db or create it
if it doesn't exists.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-man"
;;;;;;  "helm-man.el" "f7300b8dd7f7d6bb51deffcd64d5e78f")
;;; Generated autoloads from helm-man.el

(autoload 'helm-man-woman "../../../git/dotfiles/.emacs.d/lisp/helm/helm-man" "\
Preconfigured `helm' for Man and Woman pages.
With a prefix arg reinitialize the cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-misc"
;;;;;;  "helm-misc.el" "d32a3feb1b1c603cfd85e4ee46576d23")
;;; Generated autoloads from helm-misc.el

(autoload 'helm-browse-menubar "../../../git/dotfiles/.emacs.d/lisp/helm/helm-misc" "\
Preconfigured helm to the menubar using lacarte.el.

\(fn)" t nil)

(autoload 'helm-world-time "../../../git/dotfiles/.emacs.d/lisp/helm/helm-misc" "\
Preconfigured `helm' to show world time.
Default action change TZ environment variable locally to emacs.

\(fn)" t nil)

(autoload 'helm-insert-latex-math "../../../git/dotfiles/.emacs.d/lisp/helm/helm-misc" "\
Preconfigured helm for latex math symbols completion.

\(fn)" t nil)

(autoload 'helm-ratpoison-commands "../../../git/dotfiles/.emacs.d/lisp/helm/helm-misc" "\
Preconfigured `helm' to execute ratpoison commands.

\(fn)" t nil)

(autoload 'helm-stumpwm-commands "../../../git/dotfiles/.emacs.d/lisp/helm/helm-misc" "\
Preconfigured helm for stumpwm commands.

\(fn)" t nil)

(autoload 'helm-minibuffer-history "../../../git/dotfiles/.emacs.d/lisp/helm/helm-misc" "\
Preconfigured `helm' for `minibuffer-history'.

\(fn)" t nil)

(autoload 'helm-comint-input-ring "../../../git/dotfiles/.emacs.d/lisp/helm/helm-misc" "\
Preconfigured `helm' that provide completion of `comint' history.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-mode"
;;;;;;  "helm-mode.el" "79ad86449ad18ae221a2db2252b6eb50")
;;; Generated autoloads from helm-mode.el

(autoload 'helm-comp-read "../../../git/dotfiles/.emacs.d/lisp/helm/helm-mode" "\
Read a string in the minibuffer, with helm completion.

It is helm `completing-read' equivalent.

- PROMPT is the prompt name to use.

- COLLECTION can be a list, vector, obarray or hash-table.
  It can be also a function that receives three arguments:
  the values string, predicate and t. See `all-completions' for more details.

Keys description:

- TEST: A predicate called with one arg i.e candidate.

- INITIAL-INPUT: Same as input arg in `helm'.

- PRESELECT: See preselect arg of `helm'.

- DEFAULT: This option is used only for compatibility with regular
  Emacs `completing-read' (Same as DEFAULT arg of `completing-read').

- BUFFER: Name of helm-buffer.

- MUST-MATCH: Candidate selected must be one of COLLECTION.

- FUZZY: Enable fuzzy matching.

- REVERSE-HISTORY: When non--nil display history source after current
  source completion.

- REQUIRES-PATTERN: Same as helm attribute, default is 0.

- HISTORY: A list containing specific history, default is nil.
  When it is non--nil, all elements of HISTORY are displayed in
  a special source before COLLECTION.

- INPUT-HISTORY: A symbol. the minibuffer input history will be
  stored there, if nil or not provided, `minibuffer-history'
  will be used instead.

- CASE-FOLD: Same as `helm-case-fold-search'.

- DEL-INPUT: Boolean, when non--nil (default) remove the partial
  minibuffer input from HISTORY is present.

- PERSISTENT-ACTION: A function called with one arg i.e candidate.

- PERSISTENT-HELP: A string to document PERSISTENT-ACTION.

- MODE-LINE: A string or list to display in mode line.
  Default is `helm-comp-read-mode-line'.

- KEYMAP: A keymap to use in this `helm-comp-read'.
  (the keymap will be shared with history source)

- NAME: The name related to this local source.

- EXEC-WHEN-ONLY-ONE: Bound `helm-execute-action-at-once-if-one'
  to non--nil. (possibles values are t or nil).

- VOLATILE: Use volatile attribute.

- SORT: A predicate to give to `sort' e.g `string-lessp'
  Use this only on small data as it is ineficient.
  If you want to sort faster add a sort function to
  FC-TRANSFORMER.
  Note that FUZZY when enabled is already providing a sort function.

- FC-TRANSFORMER: A `filtered-candidate-transformer' function
  or a list of functions.

- HIST-FC-TRANSFORMER: A `filtered-candidate-transformer'
  function for the history source.

- MARKED-CANDIDATES: If non--nil return candidate or marked candidates as a list.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP: (default is non--nil) See `helm-comp-read-get-candidates'.

- CANDIDATES-IN-BUFFER: when non--nil use a source build with
  `helm-source-in-buffer' which is much faster.
  Argument VOLATILE have no effect when CANDIDATES-IN-BUFFER is non--nil.

Any prefix args passed during `helm-comp-read' invocation will be recorded
in `helm-current-prefix-arg', otherwise if prefix args were given before
`helm-comp-read' invocation, the value of `current-prefix-arg' will be used.
That's mean you can pass prefix args before or after calling a command
that use `helm-comp-read' See `helm-M-x' for example.

\(fn PROMPT COLLECTION &key TEST INITIAL-INPUT DEFAULT PRESELECT (buffer \"*Helm Completions*\") MUST-MATCH FUZZY REVERSE-HISTORY (requires-pattern 0) HISTORY INPUT-HISTORY (case-fold helm-comp-read-case-fold-search) (del-input t) (persistent-action nil) (persistent-help \"DoNothing\") (mode-line helm-comp-read-mode-line) HELP-MESSAGE (keymap helm-comp-read-map) (name \"Helm Completions\") CANDIDATES-IN-BUFFER EXEC-WHEN-ONLY-ONE QUIT-WHEN-NO-CAND (volatile t) SORT (fc-transformer (quote helm-cr-default-transformer)) HIST-FC-TRANSFORMER MARKED-CANDIDATES NOMARK (alistp t) (candidate-number-limit helm-candidate-number-limit))" nil nil)

(autoload 'helm-read-file-name "../../../git/dotfiles/.emacs.d/lisp/helm/helm-mode" "\
Read a file name with helm completion.
It is helm `read-file-name' emulation.

Argument PROMPT is the default prompt to use.

Keys description:

- NAME: Source name, default to \"Read File Name\".

- INITIAL-INPUT: Where to start read file name, default to `default-directory'.

- BUFFER: `helm-buffer' name default to \"*Helm Completions*\".

- TEST: A predicate called with one arg 'candidate'.

- CASE-FOLD: Same as `helm-case-fold-search'.

- PRESELECT: helm preselection.

- HISTORY: Display HISTORY in a special source.

- MUST-MATCH: Can be 'confirm, nil, or t.

- MARKED-CANDIDATES: When non--nil return a list of marked candidates.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP: Don't use `all-completions' in history (take effect only on history).

- PERSISTENT-ACTION: a persistent action function.

- PERSISTENT-HELP: persistent help message.

- MODE-LINE: A mode line message, default is `helm-read-file-name-mode-line-string'.

\(fn PROMPT &key (name \"Read File Name\") (initial-input default-directory) (buffer \"*Helm file completions*\") TEST (case-fold helm-file-name-case-fold-search) PRESELECT HISTORY MUST-MATCH DEFAULT MARKED-CANDIDATES (candidate-number-limit helm-ff-candidate-number-limit) NOMARK (alistp t) (persistent-action (quote helm-find-files-persistent-action)) (persistent-help \"Hit1 Expand Candidate, Hit2 or (C-u) Find file\") (mode-line helm-read-file-name-mode-line-string))" nil nil)

(defvar helm-mode nil "\
Non-nil if Helm mode is enabled.
See the command `helm-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-mode'.")

(custom-autoload 'helm-mode "../../../git/dotfiles/.emacs.d/lisp/helm/helm-mode" nil)

(autoload 'helm-mode "../../../git/dotfiles/.emacs.d/lisp/helm/helm-mode" "\
Toggle generic helm completion.

All functions in Emacs that use `completing-read'
or `read-file-name' and friends will use helm interface
when this mode is turned on.
However you can modify this behavior for functions of your choice
with `helm-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can turn it on with `helm-mode'.

Some crap emacs functions may not be supported,
e.g `ffap-alternate-file' and maybe others
You can add such functions to `helm-completing-read-handlers-alist'
with a nil value.

Note: This mode is incompatible with Emacs23.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-net"
;;;;;;  "helm-net.el" "be6b3a846359a8498553d172763fb082")
;;; Generated autoloads from helm-net.el

(autoload 'helm-surfraw "../../../git/dotfiles/.emacs.d/lisp/helm/helm-net" "\
Preconfigured `helm' to search PATTERN with search ENGINE.

\(fn PATTERN ENGINE)" t nil)

(autoload 'helm-google-suggest "../../../git/dotfiles/.emacs.d/lisp/helm/helm-net" "\
Preconfigured `helm' for google search with google suggest.

\(fn)" t nil)

(autoload 'helm-wikipedia-suggest "../../../git/dotfiles/.emacs.d/lisp/helm/helm-net" "\
Preconfigured `helm' for Wikipedia lookup with Wikipedia suggest.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-org"
;;;;;;  "helm-org.el" "7a46879251e0601b9e2ae6bf2b62830e")
;;; Generated autoloads from helm-org.el

(autoload 'helm-org-agenda-files-headings "../../../git/dotfiles/.emacs.d/lisp/helm/helm-org" "\
Preconfigured helm for org files headings.

\(fn)" t nil)

(autoload 'helm-org-in-buffer-headings "../../../git/dotfiles/.emacs.d/lisp/helm/helm-org" "\
Preconfigured helm for org buffer headings.

\(fn)" t nil)

(autoload 'helm-org-parent-headings "../../../git/dotfiles/.emacs.d/lisp/helm/helm-org" "\
Preconfigured helm for org headings that are parents of the
current heading.

\(fn)" t nil)

(autoload 'helm-org-capture-templates "../../../git/dotfiles/.emacs.d/lisp/helm/helm-org" "\
Preconfigured helm for org templates.

\(fn)" t nil)

(autoload 'helm-org-completing-read-tags "../../../git/dotfiles/.emacs.d/lisp/helm/helm-org" "\


\(fn PROMPT COLLECTION PRED REQ INITIAL HIST DEF INHERIT-INPUT-METHOD NAME BUFFER)" nil nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-regexp"
;;;;;;  "helm-regexp.el" "145d56705e4b066fb3bcd017cd17d81e")
;;; Generated autoloads from helm-regexp.el

(autoload 'helm-moccur-mode "../../../git/dotfiles/.emacs.d/lisp/helm/helm-regexp" "\
Major mode to provide actions in helm moccur saved buffer.

Special commands:
\\{helm-moccur-mode-map}

\(fn)" t nil)

(autoload 'helm-regexp "../../../git/dotfiles/.emacs.d/lisp/helm/helm-regexp" "\
Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp.

\(fn)" t nil)

(autoload 'helm-occur "../../../git/dotfiles/.emacs.d/lisp/helm/helm-regexp" "\
Preconfigured helm for Occur.

\(fn)" t nil)

(autoload 'helm-occur-from-isearch "../../../git/dotfiles/.emacs.d/lisp/helm/helm-regexp" "\
Invoke `helm-occur' from isearch.

\(fn)" t nil)

(autoload 'helm-multi-occur-from-isearch "../../../git/dotfiles/.emacs.d/lisp/helm/helm-regexp" "\
Invoke `helm-multi-occur' from isearch.

With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling
`helm-multi-occur-from-isearch' or during the buffer selection.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-ring"
;;;;;;  "helm-ring.el" "fce34930d4b68446aded6f4052b5c276")
;;; Generated autoloads from helm-ring.el

(defvar helm-push-mark-mode nil "\
Non-nil if Helm-Push-Mark mode is enabled.
See the command `helm-push-mark-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-push-mark-mode'.")

(custom-autoload 'helm-push-mark-mode "../../../git/dotfiles/.emacs.d/lisp/helm/helm-ring" nil)

(autoload 'helm-push-mark-mode "../../../git/dotfiles/.emacs.d/lisp/helm/helm-ring" "\
Provide an improved version of `push-mark'.
Modify the behavior of `push-mark' to update
the `global-mark-ring' after each new visit.

\(fn &optional ARG)" t nil)

(autoload 'helm-mark-ring "../../../git/dotfiles/.emacs.d/lisp/helm/helm-ring" "\
Preconfigured `helm' for `helm-source-mark-ring'.

\(fn)" t nil)

(autoload 'helm-global-mark-ring "../../../git/dotfiles/.emacs.d/lisp/helm/helm-ring" "\
Preconfigured `helm' for `helm-source-global-mark-ring'.

\(fn)" t nil)

(autoload 'helm-all-mark-rings "../../../git/dotfiles/.emacs.d/lisp/helm/helm-ring" "\
Preconfigured `helm' for `helm-source-global-mark-ring' and `helm-source-mark-ring'.

\(fn)" t nil)

(autoload 'helm-register "../../../git/dotfiles/.emacs.d/lisp/helm/helm-ring" "\
Preconfigured `helm' for Emacs registers.

\(fn)" t nil)

(autoload 'helm-show-kill-ring "../../../git/dotfiles/.emacs.d/lisp/helm/helm-ring" "\
Preconfigured `helm' for `kill-ring'.
It is drop-in replacement of `yank-pop'.

First call open the kill-ring browser, next calls move to next line.

\(fn)" t nil)

(autoload 'helm-execute-kmacro "../../../git/dotfiles/.emacs.d/lisp/helm/helm-ring" "\
Preconfigured helm for keyboard macros.
Define your macros with `f3' and `f4'.
See (info \"(emacs) Keyboard Macros\") for detailed infos.
This command is useful when used with persistent action.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-semantic"
;;;;;;  "helm-semantic.el" "aa072fab4a6ce32fc966fb56d4eee27f")
;;; Generated autoloads from helm-semantic.el

(autoload 'helm-semantic "../../../git/dotfiles/.emacs.d/lisp/helm/helm-semantic" "\
Preconfigured `helm' for `semantic'.
If ARG is supplied, pre-select symbol at point instead of current

\(fn ARG)" t nil)

(autoload 'helm-semantic-or-imenu "../../../git/dotfiles/.emacs.d/lisp/helm/helm-semantic" "\
Preconfigured helm for `semantic' or `imenu'.
If ARG is supplied, pre-select symbol at point instead of current
semantic tag in scope.

If `semantic-mode' is active in the current buffer, then use
semantic for generating tags, otherwise fall back to `imenu'.
Fill in the symbol at point by default.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-sys"
;;;;;;  "helm-sys.el" "66227dcf779b46417649baa665a8ab1c")
;;; Generated autoloads from helm-sys.el

(autoload 'helm-top "../../../git/dotfiles/.emacs.d/lisp/helm/helm-sys" "\
Preconfigured `helm' for top command.

\(fn)" t nil)

(autoload 'helm-list-emacs-process "../../../git/dotfiles/.emacs.d/lisp/helm/helm-sys" "\
Preconfigured `helm' for emacs process.

\(fn)" t nil)

(autoload 'helm-xrandr-set "../../../git/dotfiles/.emacs.d/lisp/helm/helm-sys" "\
Preconfigured helm for xrandr.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-tags"
;;;;;;  "helm-tags.el" "0c27a7492e5d0bf84b1a82443f447cce")
;;; Generated autoloads from helm-tags.el

(autoload 'helm-etags-select "../../../git/dotfiles/.emacs.d/lisp/helm/helm-tags" "\
Preconfigured helm for etags.
If called with a prefix argument REINIT
or if any of the tag files have been modified, reinitialize cache.

This function aggregates three sources of tag files:

  1) An automatically located file in the parent directories,
     by `helm-etags-get-tag-file'.
  2) `tags-file-name', which is commonly set by `find-tag' command.
  3) `tags-table-list' which is commonly set by `visit-tags-table' command.

\(fn REINIT)" t nil)

;;;***

;;;### (autoloads nil "../../../git/dotfiles/.emacs.d/lisp/helm/helm-utils"
;;;;;;  "helm-utils.el" "24afe49f5cf8cb0eb33abeb717ef29eb")
;;; Generated autoloads from helm-utils.el

(defvar helm-popup-tip-mode nil "\
Non-nil if Helm-Popup-Tip mode is enabled.
See the command `helm-popup-tip-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-popup-tip-mode'.")

(custom-autoload 'helm-popup-tip-mode "../../../git/dotfiles/.emacs.d/lisp/helm/helm-utils" nil)

(autoload 'helm-popup-tip-mode "../../../git/dotfiles/.emacs.d/lisp/helm/helm-utils" "\
Show help-echo informations in a popup tip at end of line.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "helm" "../../../../../.emacs.d/lisp/helm/helm.el"
;;;;;;  "95737de058a3f79135d4e4b0c450b59d")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm.el

(autoload 'helm-define-multi-key "helm" "\
In KEYMAP, define key sequence KEY for function list FUNCTIONS.
Each function runs sequentially for each KEY press. 
If DELAY is specified, switch back to initial function of FUNCTIONS list
after DELAY seconds.
The functions in FUNCTIONS list take no args.
e.g
  (defun foo ()
    (message \"Run foo\"))
  (defun bar ()
    (message \"Run bar\"))
  (defun baz ()
    (message \"Run baz\"))

\(helm-define-multi-key global-map \"<f5> q\" '(foo bar baz) 2)

Each time \"<f5> q\" is pressed, the next function is executed. Waiting 
more than 2 seconds between key presses switches back to executing the first 
function on the next hit.

\(fn KEYMAP KEY FUNCTIONS &optional DELAY)" nil nil)

(autoload 'helm-multi-key-defun "helm" "\
Define NAME as a multi-key command running FUNS.
After DELAY seconds, the FUNS list is reinitialized.
See `helm-define-multi-key'.

\(fn NAME DOCSTRING FUNS &optional DELAY)" nil t)

(put 'helm-multi-key-defun 'lisp-indent-function '2)

(autoload 'helm-define-key-with-subkeys "helm" "\
Defines in MAP a KEY and SUBKEY to COMMAND.

This allows typing KEY to call COMMAND the first time and
type only SUBKEY on subsequent calls.

Arg MAP is the keymap to use, SUBKEY is the initial short key-binding to
call COMMAND.

Arg OTHER-SUBKEYS is an alist specifying other short key-bindings
to use once started.
e.g:

\(helm-define-key-with-subkeys global-map
   (kbd \"C-x v n\") ?n 'git-gutter:next-hunk '((?p . git-gutter:previous-hunk)))


In this example, `C-x v n' will run `git-gutter:next-hunk'
subsequent \"n\"'s run this command again
and subsequent \"p\"'s run `git-gutter:previous-hunk'.

Arg MENU is a string displayed in minibuffer that 
describes SUBKEY and OTHER-SUBKEYS.
Arg EXIT-FN specifies a function to run on exit.

For any other keys pressed, run their assigned command as defined
in MAP and then exit the loop running EXIT-FN, if specified.

NOTE: SUBKEY and OTHER-SUBKEYS bindings support char syntax only 
\(e.g ?n), so don't use strings or vectors to define them.

\(fn MAP KEY SUBKEY COMMAND &optional OTHER-SUBKEYS MENU EXIT-FN)" nil nil)

(put 'helm-define-key-with-subkeys 'lisp-indent-function '1)

(autoload 'helm-debug-open-last-log "helm" "\
Open helm log file of last helm session.
If `helm-last-log-file' is nil, switch to `helm-debug-buffer' .

\(fn)" t nil)

(autoload 'helm "helm" "\
Main function to execute helm sources.

Keywords supported:
:sources :input :prompt :resume :preselect
:buffer :keymap :default :history :allow-nest

Extra LOCAL-VARS keywords are supported, see below.

PLIST is a list like (:key1 val1 :key2 val2 ...) or
\(&optional sources input prompt resume
            preselect buffer keymap default history).

Basic keywords are the following:

:sources

A list of sources used for this session.  It also accepts a
symbol, interpreted as a variable of a helm source
i.e (a symbol can be passed instead of a list of sources).
It also accepts an alist representing a helm source, which is
detected by (assq 'name ANY-SOURCES).
NOTE: In this case the source is embedded in the helm command and
have no symbol name, so it is not reachable from outside.
It will be referenced in `helm-sources' as a whole alist.

:input

Temporary value of `helm-pattern', ie. initial input of minibuffer.

:prompt

Prompt other than \"pattern: \".

:resume

If t, Resurrect previously instance of `helm'.  Skip the initialization.
If 'noresume, this instance of `helm' cannot be resumed.

:preselect

Initially selected candidate.  Specified by exact candidate or a regexp.

:buffer

`helm-buffer' instead of *helm*.

:keymap

`helm-map' for current `helm' session.

:default

A default argument that will be inserted in minibuffer with
\\<minibuffer-local-map>\\[next-history-element]. When nil or not
present `thing-at-point' will be used instead. If
`helm--maybe-use-default-as-input' is non-`nil' display will be
updated using :default arg as input unless :input is specified,
which in this case will take precedence over :default. This is a
string or a list. If list, car of the list becomes initial
default input. \\<minibuffer-local-map>\\[next-history-element]
cycles through the list items.

:history

Minibuffer input, by default, is pushed to `minibuffer-history'.
When an argument HISTORY is provided, input is pushed to
HISTORY. The HISTORY element should be a valid symbol.

:allow-nest

Allow running this helm command in a running helm session.

Standard arguments are supported. These two are the same:

\(helm :sources sources :input input :prompt prompt :resume resume
       :preselect preselect :buffer buffer :keymap keymap :default default
       :history history)

and

\(helm sources input prompt resume preselect buffer keymap default history)

are the same for now. However, the use of non-keyword args is
deprecated and should not be used.

Other keywords are interpreted as local variables of this helm
session. The `helm-' prefix can be omitted. For example,

\(helm :sources 'helm-source-buffers-list
       :buffer \"*helm buffers*\" :candidate-number-limit 10)

starts helm session with `helm-source-buffers' source in
*helm buffers* buffer and sets variable `helm-candidate-number-limit'
to 10 as a session local variable.

\(fn &key SOURCES INPUT PROMPT RESUME PRESELECT BUFFER KEYMAP DEFAULT HISTORY ALLOW-NEST OTHER-LOCAL-VARS)" nil nil)

(autoload 'helm-other-buffer "helm" "\
Simplified `helm' interface with other `helm-buffer'.
Call `helm' only with ANY-SOURCES and ANY-BUFFER as args.

\(fn ANY-SOURCES ANY-BUFFER)" nil nil)

(autoload 'helm-debug-toggle "helm" "\
Enable/disable helm debugging from outside of helm session.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-adaptive" "../../../../../.emacs.d/lisp/helm/helm-adaptive.el"
;;;;;;  "c28ae029de9ed1c33aa747fef7efd11a")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-adaptive.el

(defvar helm-adaptive-mode nil "\
Non-nil if Helm-Adaptive mode is enabled.
See the command `helm-adaptive-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-adaptive-mode'.")

(custom-autoload 'helm-adaptive-mode "helm-adaptive" nil)

(autoload 'helm-adaptive-mode "helm-adaptive" "\
Toggle adaptive sorting in all sources.

\(fn &optional ARG)" t nil)

(autoload 'helm-reset-adaptive-history "helm-adaptive" "\
Delete all `helm-adaptive-history' and his file.
Useful when you have a old or corrupted `helm-adaptive-history-file'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-apt" "../../../../../.emacs.d/lisp/helm/helm-apt.el"
;;;;;;  "2ef814bb5e4dd01f5cdf4ed543beae1a")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-apt.el

(autoload 'helm-apt "helm-apt" "\
Preconfigured `helm' : frontend of APT package manager.
With a prefix arg reload cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "helm-bookmark" "../../../../../.emacs.d/lisp/helm/helm-bookmark.el"
;;;;;;  "a4d0ec242040c91b342a4974df3dccd6")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-bookmark.el

(autoload 'helm-bookmarks "helm-bookmark" "\
Preconfigured `helm' for bookmarks.

\(fn)" t nil)

(autoload 'helm-filtered-bookmarks "helm-bookmark" "\
Preconfigured helm for bookmarks (filtered by category).
Optional source `helm-source-bookmark-addressbook' is loaded
only if external library addressbook-bookmark.el is available.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-buffers" "../../../../../.emacs.d/lisp/helm/helm-buffers.el"
;;;;;;  "f5fd51e381587afad0f4727b0f11caa2")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-buffers.el

(autoload 'helm-buffers-list "helm-buffers" "\
Preconfigured `helm' to list buffers.

\(fn)" t nil)

(autoload 'helm-mini "helm-buffers" "\
Preconfigured `helm' lightweight version (buffer -> recentf).

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-color" "../../../../../.emacs.d/lisp/helm/helm-color.el"
;;;;;;  "e13cedea73698afd3b8ffff8e42cbc20")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-color.el

(autoload 'helm-colors "helm-color" "\
Preconfigured `helm' for color.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-command" "../../../../../.emacs.d/lisp/helm/helm-command.el"
;;;;;;  "d59bba8f7cd033fa5f554fe3f8dd5a1f")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-command.el

(autoload 'helm-M-x "helm-command" "\
Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x' `execute-extended-command'.

Unlike regular `M-x' emacs vanilla `execute-extended-command' command,
the prefix args if needed, are passed AFTER starting `helm-M-x'.

You can get help on each command by persistent action.

\(fn ARG &optional COMMAND-NAME)" t nil)

;;;***

;;;### (autoloads nil "helm-config" "../../../../../.emacs.d/lisp/helm/helm-config.el"
;;;;;;  "ab4879f7e129f6ba95e0d8c1877b5cc0")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-config.el

(autoload 'helm-configuration "helm-config" "\
Customize `helm'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-dabbrev" "../../../../../.emacs.d/lisp/helm/helm-dabbrev.el"
;;;;;;  "9fdf1f96e0c14943abe84e171e3a96f8")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-dabbrev.el

(autoload 'helm-dabbrev "helm-dabbrev" "\
Preconfigured helm for dynamic abbreviations.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-elisp" "../../../../../.emacs.d/lisp/helm/helm-elisp.el"
;;;;;;  "0954bb1f4759ed3eac37021bd02eb87d")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-elisp.el

(autoload 'helm-lisp-completion-at-point "helm-elisp" "\
Preconfigured helm for lisp symbol completion at point.

\(fn)" t nil)

(autoload 'helm-complete-file-name-at-point "helm-elisp" "\
Preconfigured helm to complete file name at point.

\(fn &optional FORCE)" t nil)

(autoload 'helm-lisp-indent "helm-elisp" "\


\(fn)" t nil)

(autoload 'helm-lisp-completion-or-file-name-at-point "helm-elisp" "\
Preconfigured helm to complete lisp symbol or filename at point.
Filename completion happen if string start after or between a double quote.

\(fn)" t nil)

(autoload 'helm-apropos "helm-elisp" "\
Preconfigured helm to describe commands, functions, variables and faces.
In non interactives calls DEFAULT argument should be provided as a string,
i.e the `symbol-name' of any existing symbol.

\(fn DEFAULT)" t nil)

(autoload 'helm-manage-advice "helm-elisp" "\
Preconfigured `helm' to disable/enable function advices.

\(fn)" t nil)

(autoload 'helm-locate-library "helm-elisp" "\
Preconfigured helm to locate elisp libraries.

\(fn)" t nil)

(autoload 'helm-timers "helm-elisp" "\
Preconfigured `helm' for timers.

\(fn)" t nil)

(autoload 'helm-complex-command-history "helm-elisp" "\
Preconfigured helm for complex command history.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-elisp-package" "../../../../../.emacs.d/lisp/helm/helm-elisp-package.el"
;;;;;;  "7e339ecbf7a8f6f929cbcddcd108aceb")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-elisp-package.el

(autoload 'helm-list-elisp-packages "helm-elisp-package" "\
Preconfigured helm for listing and handling emacs packages.

\(fn ARG)" t nil)

(autoload 'helm-list-elisp-packages-no-fetch "helm-elisp-package" "\
Preconfigured helm for emacs packages.
Same as `helm-list-elisp-packages' but don't fetch packages on remote.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-elscreen" "../../../../../.emacs.d/lisp/helm/helm-elscreen.el"
;;;;;;  "4830916b7c7f685fe4b6d38b1e9c18af")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-elscreen.el

(autoload 'helm-elscreen "helm-elscreen" "\
Preconfigured helm to list elscreen.

\(fn)" t nil)

(autoload 'helm-elscreen-history "helm-elscreen" "\
Preconfigured helm to list elscreen in history order.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-eshell" "../../../../../.emacs.d/lisp/helm/helm-eshell.el"
;;;;;;  "0a8240b093a06193ebb4fe6321d56cc3")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-eshell.el

(autoload 'helm-esh-pcomplete "helm-eshell" "\
Preconfigured helm to provide helm completion in eshell.

\(fn)" t nil)

(autoload 'helm-eshell-history "helm-eshell" "\
Preconfigured helm for eshell history.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-eval" "../../../../../.emacs.d/lisp/helm/helm-eval.el"
;;;;;;  "22c099dd0e67933d9c42b1c0d7c85fab")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-eval.el

(autoload 'helm-eval-expression "helm-eval" "\
Preconfigured helm for `helm-source-evaluation-result'.

\(fn ARG)" t nil)

(autoload 'helm-eval-expression-with-eldoc "helm-eval" "\
Preconfigured helm for `helm-source-evaluation-result' with `eldoc' support. 

\(fn)" t nil)

(autoload 'helm-calcul-expression "helm-eval" "\
Preconfigured helm for `helm-source-calculation-result'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-external" "../../../../../.emacs.d/lisp/helm/helm-external.el"
;;;;;;  "eca08ee0da291bb844a30b586f4f304b")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-external.el

(autoload 'helm-run-external-command "helm-external" "\
Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`helm-external-commands-list'.

\(fn PROGRAM)" t nil)

;;;***

;;;### (autoloads nil "helm-files" "../../../../../.emacs.d/lisp/helm/helm-files.el"
;;;;;;  "669dbf2e38ce99408b8ba824da827a3e")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-files.el

(autoload 'helm-browse-project "helm-files" "\
Preconfigured helm to browse projects.
Browse files and see status of project with its vcs.
Only HG and GIT are supported for now.
Fall back to `helm-browse-project-find-files'
if current directory is not under control of one of those vcs.
With a prefix ARG browse files recursively, with two prefix ARG
rebuild the cache.
If the current directory is found in the cache, start
`helm-browse-project-find-files' even with no prefix ARG.
NOTE: The prefix ARG have no effect on the VCS controlled directories.

Needed dependencies for VCS:
<https://github.com/emacs-helm/helm-ls-git>
and
<https://github.com/emacs-helm/helm-ls-hg>
and
<http://melpa.org/#/helm-ls-svn>.

\(fn ARG)" t nil)

(autoload 'helm-find "helm-files" "\
Preconfigured `helm' for the find shell command.

\(fn ARG)" t nil)

(autoload 'helm-find-files "helm-files" "\
Preconfigured `helm' for helm implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `helm-find-files-1' instead.
This is the starting point for nearly all actions you can do on files.

\(fn ARG)" t nil)

(autoload 'helm-for-files "helm-files" "\
Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-preferred-list'.

\(fn)" t nil)

(autoload 'helm-multi-files "helm-files" "\
Preconfigured helm similar to `helm-for-files' but that don't run locate.
Allow toggling from locate to others sources.
This allow seeing first if what you search is in other sources before launching
locate.

\(fn)" t nil)

(autoload 'helm-recentf "helm-files" "\
Preconfigured `helm' for `recentf'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-font" "../../../../../.emacs.d/lisp/helm/helm-font.el"
;;;;;;  "536e16431188f80eeb261006813338de")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-font.el

(autoload 'helm-select-xfont "helm-font" "\
Preconfigured `helm' to select Xfont.

\(fn)" t nil)

(autoload 'helm-ucs "helm-font" "\
Preconfigured helm for `ucs-names' math symbols.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-grep" "../../../../../.emacs.d/lisp/helm/helm-grep.el"
;;;;;;  "6821dc15abf4f02b69f4cbbd5c031c36")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-grep.el

(autoload 'helm-goto-precedent-file "helm-grep" "\
Go to precedent file in helm grep/etags buffers.

\(fn)" t nil)

(autoload 'helm-goto-next-file "helm-grep" "\
Go to precedent file in helm grep/etags buffers.

\(fn)" t nil)

(autoload 'helm-do-grep-ag "helm-grep" "\
Preconfigured helm for grepping with AG in `default-directory'.
With prefix-arg prompt for type if available with your AG version.

\(fn ARG)" t nil)

(autoload 'helm-grep-do-git-grep "helm-grep" "\
Preconfigured helm for git-grepping `default-directory'.
With a prefix arg ARG git-grep the whole repository.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "helm-gtags" "../../../../../.emacs.d/lisp/helm/helm-gtags.el"
;;;;;;  "9588722a80004d0efbec446f97ec36b8")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-gtags.el

(autoload 'helm-gtags-clear-all-cache "helm-gtags" "\


\(fn)" t nil)

(autoload 'helm-gtags-clear-cache "helm-gtags" "\


\(fn)" t nil)

(autoload 'helm-gtags-next-history "helm-gtags" "\
Jump to next position on context stack

\(fn)" t nil)

(autoload 'helm-gtags-previous-history "helm-gtags" "\
Jump to previous position on context stack

\(fn)" t nil)

(autoload 'helm-gtags-select "helm-gtags" "\


\(fn)" t nil)

(autoload 'helm-gtags-select-path "helm-gtags" "\


\(fn)" t nil)

(autoload 'helm-gtags-tags-in-this-function "helm-gtags" "\
Show tagnames which are referenced in this function and jump to it.

\(fn)" t nil)

(autoload 'helm-gtags-create-tags "helm-gtags" "\


\(fn DIR LABEL)" t nil)

(autoload 'helm-gtags-find-tag "helm-gtags" "\
Jump to definition

\(fn TAG)" t nil)

(autoload 'helm-gtags-find-tag-other-window "helm-gtags" "\
Jump to definition in other window.

\(fn TAG)" t nil)

(autoload 'helm-gtags-find-rtag "helm-gtags" "\
Jump to referenced point

\(fn TAG)" t nil)

(autoload 'helm-gtags-find-symbol "helm-gtags" "\
Jump to the symbol location

\(fn TAG)" t nil)

(autoload 'helm-gtags-find-pattern "helm-gtags" "\
Grep and jump by gtags tag files.

\(fn PATTERN)" t nil)

(autoload 'helm-gtags-find-files "helm-gtags" "\
Find file from tagged with gnu global.

\(fn FILE)" t nil)

(autoload 'helm-gtags-find-tag-from-here "helm-gtags" "\
Jump point by current point information.
Jump to definition point if cursor is on its reference.
Jump to reference point if curosr is on its definition

\(fn)" t nil)

(autoload 'helm-gtags-dwim "helm-gtags" "\
Find by context. Here is
- on include statement then jump to included file
- on symbol definition then jump to its references
- on reference point then jump to its definition.

\(fn)" t nil)

(autoload 'helm-gtags-parse-file "helm-gtags" "\
Parse current file with gnu global. This is similar to `imenu'.
You can jump definitions of functions, symbols in this file.

\(fn)" t nil)

(autoload 'helm-gtags-pop-stack "helm-gtags" "\
Jump to previous point on the context stack and pop it from stack.

\(fn)" t nil)

(autoload 'helm-gtags-show-stack "helm-gtags" "\
Show current context stack.

\(fn)" t nil)

(autoload 'helm-gtags-clear-stack "helm-gtags" "\
Clear current context stack.

\(fn)" t nil)

(autoload 'helm-gtags-clear-all-stacks "helm-gtags" "\
Clear all context stacks.

\(fn)" t nil)

(autoload 'helm-gtags-update-tags "helm-gtags" "\
Update TAG file. Update All files with `C-u' prefix.
Generate new TAG file in selected directory with `C-u C-u'

\(fn)" t nil)

(autoload 'helm-gtags-resume "helm-gtags" "\
Resurrect previously invoked `helm-gtags` command.

\(fn)" t nil)

(autoload 'helm-gtags-mode "helm-gtags" "\
Toggle Helm-Gtags mode on or off.
With a prefix argument ARG, enable Helm-Gtags mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{helm-gtags-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "helm-help" "../../../../../.emacs.d/lisp/helm/helm-help.el"
;;;;;;  "98a80a3a9f19396a575a622d09c3871c")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-help.el

(autoload 'helm-documentation "helm-help" "\
Preconfigured helm for helm documentation.
With a prefix arg refresh the documentation.

Find here the documentation of all sources actually documented.

\(fn ARG)" t nil)

(defvar helm-comp-read-mode-line "\\<helm-comp-read-map>C/\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-help]:Help \\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(defvar helm-read-file-name-mode-line-string "\\<helm-read-file-map>\\[helm-help]:Help C/\\[helm-cr-empty-string]:Empty \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend" "\
String displayed in mode-line in `helm-source-find-files'.")

(defvar helm-top-mode-line "\\<helm-top-map>\\[helm-help]:Help \\<helm-map>\\[helm-select-action]:Act \\[helm-maybe-exit-minibuffer]/f1/f2/f-n:NthAct \\[helm-toggle-suspend-update]:Tog.suspend")

(autoload 'helm-describe-helm-attribute "helm-help" "\
Display the full documentation of HELM-ATTRIBUTE.
HELM-ATTRIBUTE should be a symbol.

\(fn HELM-ATTRIBUTE)" t nil)

;;;***

;;;### (autoloads nil "helm-id-utils" "../../../../../.emacs.d/lisp/helm/helm-id-utils.el"
;;;;;;  "41b705c5f04021afe24191f1581b2076")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-id-utils.el

(autoload 'helm-gid "helm-id-utils" "\
Preconfigured helm for `gid' command line of `ID-Utils'.
Need A database created with the command `mkid'
above `default-directory'.
Need id-utils as dependency which provide `mkid', `gid' etc...
See <https://www.gnu.org/software/idutils/>.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-imenu" "../../../../../.emacs.d/lisp/helm/helm-imenu.el"
;;;;;;  "e0c2217b97dfff49998c18a70925154d")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-imenu.el

(autoload 'helm-imenu "helm-imenu" "\
Preconfigured `helm' for `imenu'.

\(fn)" t nil)

(autoload 'helm-imenu-in-all-buffers "helm-imenu" "\
Preconfigured helm for fetching imenu entries of all buffers.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-info" "../../../../../.emacs.d/lisp/helm/helm-info.el"
;;;;;;  "86690ceefc08197f24ad82573d5e255b")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-info.el

(autoload 'helm-info "helm-info" "\
Preconfigured `helm' for searching Info files' indices.

\(fn)" t nil)

(autoload 'helm-info-at-point "helm-info" "\
Preconfigured `helm' for searching info at point.
With a prefix-arg insert symbol at point.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-locate" "../../../../../.emacs.d/lisp/helm/helm-locate.el"
;;;;;;  "5f1654c9bafcf5514736c5dbec37d2b7")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-locate.el

(autoload 'helm-projects-find-files "helm-locate" "\
Find files with locate in `helm-locate-project-list'.
With a prefix arg refresh the database in each project.

\(fn UPDATE)" t nil)

(autoload 'helm-locate "helm-locate" "\
Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options and also `helm-locate-command'.

You can specify a local database with prefix argument ARG.
With two prefix arg, refresh the current local db or create it
if it doesn't exists.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "helm-man" "../../../../../.emacs.d/lisp/helm/helm-man.el"
;;;;;;  "f7300b8dd7f7d6bb51deffcd64d5e78f")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-man.el

(autoload 'helm-man-woman "helm-man" "\
Preconfigured `helm' for Man and Woman pages.
With a prefix arg reinitialize the cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "helm-misc" "../../../../../.emacs.d/lisp/helm/helm-misc.el"
;;;;;;  "d32a3feb1b1c603cfd85e4ee46576d23")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-misc.el

(autoload 'helm-browse-menubar "helm-misc" "\
Preconfigured helm to the menubar using lacarte.el.

\(fn)" t nil)

(autoload 'helm-world-time "helm-misc" "\
Preconfigured `helm' to show world time.
Default action change TZ environment variable locally to emacs.

\(fn)" t nil)

(autoload 'helm-insert-latex-math "helm-misc" "\
Preconfigured helm for latex math symbols completion.

\(fn)" t nil)

(autoload 'helm-ratpoison-commands "helm-misc" "\
Preconfigured `helm' to execute ratpoison commands.

\(fn)" t nil)

(autoload 'helm-stumpwm-commands "helm-misc" "\
Preconfigured helm for stumpwm commands.

\(fn)" t nil)

(autoload 'helm-minibuffer-history "helm-misc" "\
Preconfigured `helm' for `minibuffer-history'.

\(fn)" t nil)

(autoload 'helm-comint-input-ring "helm-misc" "\
Preconfigured `helm' that provide completion of `comint' history.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-mode" "../../../../../.emacs.d/lisp/helm/helm-mode.el"
;;;;;;  "79ad86449ad18ae221a2db2252b6eb50")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-mode.el

(autoload 'helm-comp-read "helm-mode" "\
Read a string in the minibuffer, with helm completion.

It is helm `completing-read' equivalent.

- PROMPT is the prompt name to use.

- COLLECTION can be a list, vector, obarray or hash-table.
  It can be also a function that receives three arguments:
  the values string, predicate and t. See `all-completions' for more details.

Keys description:

- TEST: A predicate called with one arg i.e candidate.

- INITIAL-INPUT: Same as input arg in `helm'.

- PRESELECT: See preselect arg of `helm'.

- DEFAULT: This option is used only for compatibility with regular
  Emacs `completing-read' (Same as DEFAULT arg of `completing-read').

- BUFFER: Name of helm-buffer.

- MUST-MATCH: Candidate selected must be one of COLLECTION.

- FUZZY: Enable fuzzy matching.

- REVERSE-HISTORY: When non--nil display history source after current
  source completion.

- REQUIRES-PATTERN: Same as helm attribute, default is 0.

- HISTORY: A list containing specific history, default is nil.
  When it is non--nil, all elements of HISTORY are displayed in
  a special source before COLLECTION.

- INPUT-HISTORY: A symbol. the minibuffer input history will be
  stored there, if nil or not provided, `minibuffer-history'
  will be used instead.

- CASE-FOLD: Same as `helm-case-fold-search'.

- DEL-INPUT: Boolean, when non--nil (default) remove the partial
  minibuffer input from HISTORY is present.

- PERSISTENT-ACTION: A function called with one arg i.e candidate.

- PERSISTENT-HELP: A string to document PERSISTENT-ACTION.

- MODE-LINE: A string or list to display in mode line.
  Default is `helm-comp-read-mode-line'.

- KEYMAP: A keymap to use in this `helm-comp-read'.
  (the keymap will be shared with history source)

- NAME: The name related to this local source.

- EXEC-WHEN-ONLY-ONE: Bound `helm-execute-action-at-once-if-one'
  to non--nil. (possibles values are t or nil).

- VOLATILE: Use volatile attribute.

- SORT: A predicate to give to `sort' e.g `string-lessp'
  Use this only on small data as it is ineficient.
  If you want to sort faster add a sort function to
  FC-TRANSFORMER.
  Note that FUZZY when enabled is already providing a sort function.

- FC-TRANSFORMER: A `filtered-candidate-transformer' function
  or a list of functions.

- HIST-FC-TRANSFORMER: A `filtered-candidate-transformer'
  function for the history source.

- MARKED-CANDIDATES: If non--nil return candidate or marked candidates as a list.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP: (default is non--nil) See `helm-comp-read-get-candidates'.

- CANDIDATES-IN-BUFFER: when non--nil use a source build with
  `helm-source-in-buffer' which is much faster.
  Argument VOLATILE have no effect when CANDIDATES-IN-BUFFER is non--nil.

Any prefix args passed during `helm-comp-read' invocation will be recorded
in `helm-current-prefix-arg', otherwise if prefix args were given before
`helm-comp-read' invocation, the value of `current-prefix-arg' will be used.
That's mean you can pass prefix args before or after calling a command
that use `helm-comp-read' See `helm-M-x' for example.

\(fn PROMPT COLLECTION &key TEST INITIAL-INPUT DEFAULT PRESELECT (buffer \"*Helm Completions*\") MUST-MATCH FUZZY REVERSE-HISTORY (requires-pattern 0) HISTORY INPUT-HISTORY (case-fold helm-comp-read-case-fold-search) (del-input t) (persistent-action nil) (persistent-help \"DoNothing\") (mode-line helm-comp-read-mode-line) HELP-MESSAGE (keymap helm-comp-read-map) (name \"Helm Completions\") CANDIDATES-IN-BUFFER EXEC-WHEN-ONLY-ONE QUIT-WHEN-NO-CAND (volatile t) SORT (fc-transformer (quote helm-cr-default-transformer)) HIST-FC-TRANSFORMER MARKED-CANDIDATES NOMARK (alistp t) (candidate-number-limit helm-candidate-number-limit))" nil nil)

(autoload 'helm-read-file-name "helm-mode" "\
Read a file name with helm completion.
It is helm `read-file-name' emulation.

Argument PROMPT is the default prompt to use.

Keys description:

- NAME: Source name, default to \"Read File Name\".

- INITIAL-INPUT: Where to start read file name, default to `default-directory'.

- BUFFER: `helm-buffer' name default to \"*Helm Completions*\".

- TEST: A predicate called with one arg 'candidate'.

- CASE-FOLD: Same as `helm-case-fold-search'.

- PRESELECT: helm preselection.

- HISTORY: Display HISTORY in a special source.

- MUST-MATCH: Can be 'confirm, nil, or t.

- MARKED-CANDIDATES: When non--nil return a list of marked candidates.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP: Don't use `all-completions' in history (take effect only on history).

- PERSISTENT-ACTION: a persistent action function.

- PERSISTENT-HELP: persistent help message.

- MODE-LINE: A mode line message, default is `helm-read-file-name-mode-line-string'.

\(fn PROMPT &key (name \"Read File Name\") (initial-input default-directory) (buffer \"*Helm file completions*\") TEST (case-fold helm-file-name-case-fold-search) PRESELECT HISTORY MUST-MATCH DEFAULT MARKED-CANDIDATES (candidate-number-limit helm-ff-candidate-number-limit) NOMARK (alistp t) (persistent-action (quote helm-find-files-persistent-action)) (persistent-help \"Hit1 Expand Candidate, Hit2 or (C-u) Find file\") (mode-line helm-read-file-name-mode-line-string))" nil nil)

(defvar helm-mode nil "\
Non-nil if Helm mode is enabled.
See the command `helm-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-mode'.")

(custom-autoload 'helm-mode "helm-mode" nil)

(autoload 'helm-mode "helm-mode" "\
Toggle generic helm completion.

All functions in Emacs that use `completing-read'
or `read-file-name' and friends will use helm interface
when this mode is turned on.
However you can modify this behavior for functions of your choice
with `helm-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can turn it on with `helm-mode'.

Some crap emacs functions may not be supported,
e.g `ffap-alternate-file' and maybe others
You can add such functions to `helm-completing-read-handlers-alist'
with a nil value.

Note: This mode is incompatible with Emacs23.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "helm-net" "../../../../../.emacs.d/lisp/helm/helm-net.el"
;;;;;;  "be6b3a846359a8498553d172763fb082")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-net.el

(autoload 'helm-surfraw "helm-net" "\
Preconfigured `helm' to search PATTERN with search ENGINE.

\(fn PATTERN ENGINE)" t nil)

(autoload 'helm-google-suggest "helm-net" "\
Preconfigured `helm' for google search with google suggest.

\(fn)" t nil)

(autoload 'helm-wikipedia-suggest "helm-net" "\
Preconfigured `helm' for Wikipedia lookup with Wikipedia suggest.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-org" "../../../../../.emacs.d/lisp/helm/helm-org.el"
;;;;;;  "7a46879251e0601b9e2ae6bf2b62830e")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-org.el

(autoload 'helm-org-agenda-files-headings "helm-org" "\
Preconfigured helm for org files headings.

\(fn)" t nil)

(autoload 'helm-org-in-buffer-headings "helm-org" "\
Preconfigured helm for org buffer headings.

\(fn)" t nil)

(autoload 'helm-org-parent-headings "helm-org" "\
Preconfigured helm for org headings that are parents of the
current heading.

\(fn)" t nil)

(autoload 'helm-org-capture-templates "helm-org" "\
Preconfigured helm for org templates.

\(fn)" t nil)

(autoload 'helm-org-completing-read-tags "helm-org" "\


\(fn PROMPT COLLECTION PRED REQ INITIAL HIST DEF INHERIT-INPUT-METHOD NAME BUFFER)" nil nil)

;;;***

;;;### (autoloads nil "helm-regexp" "../../../../../.emacs.d/lisp/helm/helm-regexp.el"
;;;;;;  "145d56705e4b066fb3bcd017cd17d81e")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-regexp.el

(autoload 'helm-moccur-mode "helm-regexp" "\
Major mode to provide actions in helm moccur saved buffer.

Special commands:
\\{helm-moccur-mode-map}

\(fn)" t nil)

(autoload 'helm-regexp "helm-regexp" "\
Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp.

\(fn)" t nil)

(autoload 'helm-occur "helm-regexp" "\
Preconfigured helm for Occur.

\(fn)" t nil)

(autoload 'helm-occur-from-isearch "helm-regexp" "\
Invoke `helm-occur' from isearch.

\(fn)" t nil)

(autoload 'helm-multi-occur-from-isearch "helm-regexp" "\
Invoke `helm-multi-occur' from isearch.

With a prefix arg, reverse the behavior of
`helm-moccur-always-search-in-current'.
The prefix arg can be set before calling
`helm-multi-occur-from-isearch' or during the buffer selection.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "helm-ring" "../../../../../.emacs.d/lisp/helm/helm-ring.el"
;;;;;;  "fce34930d4b68446aded6f4052b5c276")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-ring.el

(defvar helm-push-mark-mode nil "\
Non-nil if Helm-Push-Mark mode is enabled.
See the command `helm-push-mark-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-push-mark-mode'.")

(custom-autoload 'helm-push-mark-mode "helm-ring" nil)

(autoload 'helm-push-mark-mode "helm-ring" "\
Provide an improved version of `push-mark'.
Modify the behavior of `push-mark' to update
the `global-mark-ring' after each new visit.

\(fn &optional ARG)" t nil)

(autoload 'helm-mark-ring "helm-ring" "\
Preconfigured `helm' for `helm-source-mark-ring'.

\(fn)" t nil)

(autoload 'helm-global-mark-ring "helm-ring" "\
Preconfigured `helm' for `helm-source-global-mark-ring'.

\(fn)" t nil)

(autoload 'helm-all-mark-rings "helm-ring" "\
Preconfigured `helm' for `helm-source-global-mark-ring' and `helm-source-mark-ring'.

\(fn)" t nil)

(autoload 'helm-register "helm-ring" "\
Preconfigured `helm' for Emacs registers.

\(fn)" t nil)

(autoload 'helm-show-kill-ring "helm-ring" "\
Preconfigured `helm' for `kill-ring'.
It is drop-in replacement of `yank-pop'.

First call open the kill-ring browser, next calls move to next line.

\(fn)" t nil)

(autoload 'helm-execute-kmacro "helm-ring" "\
Preconfigured helm for keyboard macros.
Define your macros with `f3' and `f4'.
See (info \"(emacs) Keyboard Macros\") for detailed infos.
This command is useful when used with persistent action.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-semantic" "../../../../../.emacs.d/lisp/helm/helm-semantic.el"
;;;;;;  "aa072fab4a6ce32fc966fb56d4eee27f")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-semantic.el

(autoload 'helm-semantic "helm-semantic" "\
Preconfigured `helm' for `semantic'.
If ARG is supplied, pre-select symbol at point instead of current

\(fn ARG)" t nil)

(autoload 'helm-semantic-or-imenu "helm-semantic" "\
Preconfigured helm for `semantic' or `imenu'.
If ARG is supplied, pre-select symbol at point instead of current
semantic tag in scope.

If `semantic-mode' is active in the current buffer, then use
semantic for generating tags, otherwise fall back to `imenu'.
Fill in the symbol at point by default.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "helm-sys" "../../../../../.emacs.d/lisp/helm/helm-sys.el"
;;;;;;  "66227dcf779b46417649baa665a8ab1c")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-sys.el

(autoload 'helm-top "helm-sys" "\
Preconfigured `helm' for top command.

\(fn)" t nil)

(autoload 'helm-list-emacs-process "helm-sys" "\
Preconfigured `helm' for emacs process.

\(fn)" t nil)

(autoload 'helm-xrandr-set "helm-sys" "\
Preconfigured helm for xrandr.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "helm-tags" "../../../../../.emacs.d/lisp/helm/helm-tags.el"
;;;;;;  "0c27a7492e5d0bf84b1a82443f447cce")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-tags.el

(autoload 'helm-etags-select "helm-tags" "\
Preconfigured helm for etags.
If called with a prefix argument REINIT
or if any of the tag files have been modified, reinitialize cache.

This function aggregates three sources of tag files:

  1) An automatically located file in the parent directories,
     by `helm-etags-get-tag-file'.
  2) `tags-file-name', which is commonly set by `find-tag' command.
  3) `tags-table-list' which is commonly set by `visit-tags-table' command.

\(fn REINIT)" t nil)

;;;***

;;;### (autoloads nil "helm-utils" "../../../../../.emacs.d/lisp/helm/helm-utils.el"
;;;;;;  "24afe49f5cf8cb0eb33abeb717ef29eb")
;;; Generated autoloads from ../../../../../.emacs.d/lisp/helm/helm-utils.el

(defvar helm-popup-tip-mode nil "\
Non-nil if Helm-Popup-Tip mode is enabled.
See the command `helm-popup-tip-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-popup-tip-mode'.")

(custom-autoload 'helm-popup-tip-mode "helm-utils" nil)

(autoload 'helm-popup-tip-mode "helm-utils" "\
Show help-echo informations in a popup tip at end of line.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/lisp/helm/helm-adaptive.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-aliases.el" "../../../../../.emacs.d/lisp/helm/helm-apt.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-autoloads.el" "../../../../../.emacs.d/lisp/helm/helm-bookmark.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-buffers.el" "../../../../../.emacs.d/lisp/helm/helm-color.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-command.el" "../../../../../.emacs.d/lisp/helm/helm-config.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-core-pkg.el" "../../../../../.emacs.d/lisp/helm/helm-dabbrev.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-easymenu.el" "../../../../../.emacs.d/lisp/helm/helm-elisp-package.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-elisp.el" "../../../../../.emacs.d/lisp/helm/helm-elscreen.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-eshell.el" "../../../../../.emacs.d/lisp/helm/helm-eval.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-external.el" "../../../../../.emacs.d/lisp/helm/helm-files.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-font.el" "../../../../../.emacs.d/lisp/helm/helm-grep.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-gtags.el" "../../../../../.emacs.d/lisp/helm/helm-help.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-id-utils.el" "../../../../../.emacs.d/lisp/helm/helm-imenu.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-info.el" "../../../../../.emacs.d/lisp/helm/helm-lib.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-locate.el" "../../../../../.emacs.d/lisp/helm/helm-man.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-match-plugin.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-misc.el" "../../../../../.emacs.d/lisp/helm/helm-mode.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-multi-match.el" "../../../../../.emacs.d/lisp/helm/helm-net.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-org.el" "../../../../../.emacs.d/lisp/helm/helm-pkg.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-plugin.el" "../../../../../.emacs.d/lisp/helm/helm-regexp.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-ring.el" "../../../../../.emacs.d/lisp/helm/helm-semantic.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-source.el" "../../../../../.emacs.d/lisp/helm/helm-sys.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-tags.el" "../../../../../.emacs.d/lisp/helm/helm-types.el"
;;;;;;  "../../../../../.emacs.d/lisp/helm/helm-utils.el" "../../../../../.emacs.d/lisp/helm/helm.el"
;;;;;;  "helm-adaptive.el" "helm-apt.el" "helm-bookmark.el" "helm-buffers.el"
;;;;;;  "helm-color.el" "helm-command.el" "helm-config.el" "helm-dabbrev.el"
;;;;;;  "helm-elisp-package.el" "helm-elisp.el" "helm-elscreen.el"
;;;;;;  "helm-eshell.el" "helm-eval.el" "helm-external.el" "helm-files.el"
;;;;;;  "helm-font.el" "helm-grep.el" "helm-help.el" "helm-id-utils.el"
;;;;;;  "helm-imenu.el" "helm-info.el" "helm-locate.el" "helm-man.el"
;;;;;;  "helm-misc.el" "helm-mode.el" "helm-net.el" "helm-org.el"
;;;;;;  "helm-regexp.el" "helm-ring.el" "helm-semantic.el" "helm-sys.el"
;;;;;;  "helm-tags.el" "helm-utils.el" "helm.el") (22307 30083 993145
;;;;;;  158000))

;;;***

(provide 'helm-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-autoloads.el ends here
