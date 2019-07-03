dotfiles
========

Emacs config. Install by replacing ~/.emacs.d with
"ln -s dotfiles-path/.emacs.d ~/.emacs.d"

    if [ -f ~/git/dotfiles/.bashrc_extra ]; then
        source ~/git/dotfiles/.bashrc_extra
    fi


Keybindings:

 * ESC g/G : magit [2] (G: commit --all)
 * ESC p   : proced
 * ESC c   : flycheck
   * ESC C   : flycheck with configure
   * ESC M-c : verbose flycheck mode (ESC ESC c)
   * ESC M-C : verbose flycheck select checker interactively
   * F5/F6 : next/prev-error
 * ESC e : shell
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
 * Xmonad: Win-ALT-Shift up/down/left/right : resize window
 * Win up/down/left/right : jump window
 * M-S up/down/left/right : enlarge/shrink window
 * Xmonad: Win-Shift up/down/left/right : move window
 * Org-mode:
   * F1 (ESC a, Ctrl-c a) : org-agenda
   * F2 : filter items with :plan: tag
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
   * M-F9  : switch between org-mode and c-mode
   * M-G   : magit hist
   * M-i   : start irony mode
   * M-M   : helm query man db
   * M-( + M-) : toggle block hide/show
   * M-F9  : mix org and c++ mode
 * ESC M-h : haskell interactive mode  
 * haskell-mode:
   * F1 | C-c C-l : eval in repl
   * ESC i: intero mode (runs stack init)
   * F2 : typeinfo : c-c c-t
   * F4 : hoogle
   * ESC-;: generate etags file via hasktags, compile hasktags if it not exists
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

Alsa:
 * Win Ctrl-j, Win Ctrl-k : Audio increate vol, decreate vol.
 * Win Ctrl-m             : Audio toggle mute (Master, Speaker, Headphone)
  

Defaults:

 * M > : End of buffer
 * M < : Top of buffer
 * Ctrl-x 4 a: add changlog entry
 * Ctrl-[+/-] : bigger/smaller font (terminal)
 * Ctrl-l : recenter
 * M : | M-x : Eval command
 * Frames:
  * Ctrl-x 5 b : open frame (new x-window) with buffer
  * Ctrl-x 5 0 : close frame (new x-window)
  * Ctrl-x 5 o : switch frame (new x-window)
 * Themes
  * ESC t : Toggle dark/light

KDE konsole keys:
 * CTRL-Shieft-m : hide/show menubar
 * use solarized/solarizedlight to switch colorscheme in konsole
 * disable shift-l/shift-r to disable Konsole TAB switching

Magit cheatsheet:
 * f a : fetch all
 * b s : branch spinoff, move open changes to new branch
 * b c : branch create
 * r i : rebase interactive
 * r m : rebase-edit selected commit only (point in l l logview)
 * r s : rebase lower part onto destination
 * l a : list all branches
   * Select a patch and:
     * A A : cherrypick to current branch, use b b to switch current branch
 * Merge conflicts:
   * e : on _unmerged_ files opens ediff, second emacs window shows ediff ctrl:
     * |   : column view
     * a/b : select left/right
     * n/p : navigate diff sections
     * q   : finish
 * Ediff 3 colum view:
   * xy : i.e. press ab to move current hunk from a to b, ac to move current hunk from a to c
   * rx : i.e. press ra to restore current hunk in column a
   
 * y   : branch viewer (k delet branch)

Links:

 * [1] helm is only loaded if ggtags loads. Therefore a fairly recent global (www.gnu.org/software/global/)
  has to be present
 * [2] Magit cheetsheet: https://github.com/magit/magit/wiki/Cheatsheet
 * [3] https://github.com/magit/magit/wiki/Cheatsheet
 * [4] https://endlessparentheses.com/it-s-magit-and-you-re-the-magician.html

Org-mode timeentry:
 * [1] http://orgmode.org/manual/The-date_002ftime-prompt.html#The-date_002ftime-prompt
 * [2] http://orgmode.org/manual/Effort-estimates.html#Effort-estimates, https://writequit.org/denver-emacs/presentations/2017-04-11-time-clocking-with-org.html
 * [3] http://doc.norang.ca/org-mode.html#ReviewingEstimates
