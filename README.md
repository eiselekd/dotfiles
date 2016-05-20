dotfiles
========

Emacs config. Install by replacing ~/.emacs.d with
"ln -s dotfiles-path/.emacs.d ~/.emacs.d"

Keybindings:

 * ESC g : magit
 * ESC p : proced
 * ESC c : flycheck
   * F5/F6 : next/prev-error
 * ESC e : eshell
 * ESC E : ansi-term
 * ESC 1 : workspace w1
 * ESC 2 : workspace w2
 * ESC 5 : open file at cursor
 * ESC 6 : find elisp func at cursor
 * ESC 7 : find func at cursor using gtags
 * ESC F10 : open menu-bar in console mode
 * F10   : compile, search for "compile.txt" as make command
 * F11   : gdb, search for "gdb.txt" as gdb init-command script
   * F1 : restore layout
   * F5 : step into
   * F6 : step over
   * F7 : finish
   * F8 : continue
   * F12: set breakpoint
 * ALT-Shift up/down/left/right : resize window
 * Shift up/down/left/right : jump window
 * Org-mode:
   * F1 (ESC a, Ctrl-c a) : org-agenda
   * F2  : org-todo change
   * F3/F4 : clock-in/clock-out
   * F5/F6 : org item narrow/widen
 * F7 : org-capture (ctrl-c c), i.e "F7 n"
 * c-mode/c++-mode:
   * ESC . : gtags find ([1]), "M-x ggtags-create-tags" to create tags
   * ESC , : pop markbuffer ([1])
   * ESC h : toggle hs-org/minor-mode (enabled default)
   * ESC H : toggle orgstruct-mode (with '* #' org in comment)
   * M-G   : magit hist
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
 * Ctrl-x Ctrl-[+/-] : bigger/smaller font 

[1] helm is only loaded if ggtags loads. Therefore a fairly recent global (www.gnu.org/software/global/) has to be present