dotfiles
========

Emacs config. Install by replacing ~/.emacs.d with
"ln -s dotfiles-path/.emacs.d ~/.emacs.d"

Keybindings:

 * ESC g : magit [2]
 * ESC p : proced
 * ESC c : flycheck
   * ESC C : flycheck with configure
   * F5/F6 : next/prev-error
 * ESC e : eshell
 * ESC E : ansi-term
 * ESC 1 : workspace w1
 * ESC 2 : workspace w2
 * ESC 5 : open file at cursor
 * ESC 6 : find elisp func at cursor
 * ESC 7 : find func at cursor using gtags
 * ESC F10 : open menu-bar in console mode
 * F9    : compile, search for "compile.txt" as make command
 * F10   : gdb, search for "gdb.txt" as gdb init-command script
   * F1 : restore layout
   * F5 : step into
   * F6 : step over
   * F7 : finish
   * F8 : continue
   * F12: set breakpoint
 * Win-ALT-Shift up/down/left/right : resize window
 * Win up/down/left/right : jump window
 * Win-Shift up/down/left/right : move window
 * Org-mode:
   * F1 (ESC a, Ctrl-c a) : org-agenda
   * F2 : org-todo change
   * F4 : column view (r: recreate, q: quit column view) see [4]
   * F3/S-F3 : clock-in/clock-out
   * F5/S-F5 : org item narrow/widen
   * F7 : export slides as pdf
   * shift-F7 : export reveal slides
   * F8 : export html
   * shift-F8 : export twbs html
   * shift-F9 : diplay inline images
   * C-c C-c: evaluate source block
     (platuml #assistive_technologies=org.GNOME.Accessibility.AtkWrapper in /etc/...)
   * Agenda:
     * enter time: C-c . : enter timestamp
     * edit timestamps : cursor in timestamp or in calendar "C-c ."
       * use: S-arrow, "<", ">" 
 * F7 : org-capture (ctrl-c c), i.e "F7 n"
 * c-mode/c++-mode:
   * ESC . : gtags find ([1]), "M-x ggtags-create-tags" to create tags
   * ESC , : pop markbuffer ([1])
   * ESC : : helm dash query (sqlite3 needed, use helm-dash-update-docset)
   * ESC h : toggle hs-org/minor-mode (toggle blocks and ifdef blocks, disabled default)
   * ESC H : toggle orgstruct-mode (with '* #' org in comment)
   * F9    : switch between org-mode and c-mode
   * M-G   : magit hist
   * M-i   : start irony mode
   * M-M   : helm query man db
   * M-SPC : disable remove space on write
   * M-( + M-) : toggle block hide/show
   * F9    : mix org and c++ mode
 * ESC q : grep in current dir ([1])
 * ESC Q : recursive grep from current dir ([1])
 * dired:
  * TAB   : toggle subtree
  * ESC f : recursive grep in current dir
  * C-fh  : recursive grep on dir under cursor
  * C-fg  : find-gre-dired
  * C-fn  : find-gre-name

HELM:
 * C-x b : helm buffer select
 * M-X   : helm version of M-x

Misc:
 * ESC d : toggle debug on elisp error
 * ESC z : repeat last command (default:M-x z)(continue z to continue repeat)
 * ESC i : start irc
 * ESC j : goto-line

Defaults:

 * M > : End of buffer
 * M < : Top of buffer
 * Ctrl-x 4 a: add changlog entry
 * Ctrl-[+/-] : bigger/smaller font (terminal)
 * Ctrl-l : recenter

[1] helm is only loaded if ggtags loads. Therefore a fairly recent global (www.gnu.org/software/global/)
has to be present
[2] Magit cheetsheet: https://github.com/magit/magit/wiki/Cheatsheet

Org-mode timeentry:
[3] http://orgmode.org/manual/The-date_002ftime-prompt.html#The-date_002ftime-prompt
[4] http://orgmode.org/manual/Effort-estimates.html#Effort-estimates, https://writequit.org/denver-emacs/presentations/2017-04-11-time-clocking-with-org.html
