dotfiles
========

Emacs config. Install by replacing ~/.emacs.d with
"ln -s dotfiles-path/.emacs.d ~/.emacs.d"

    if [ -f ~/git/dotfiles/.bashrc_extra ]; then
	source ~/git/dotfiles/.bashrc_extra
    fi

Keybindings:

 * ESC H : this file

 * ESC g/G : magit [2] (G: commit --all)
 * ESC p   : proced
 * ESC c   : flycheck
   * ESC C   : flycheck with configure
   * ESC M-c : verbose flycheck mode (ESC ESC c)
   * ESC M-C : verbose flycheck select checker interactively
   * (Shift)-F5/F6 : next/prev-error
   * define gtest rule in parent Makefile and specify buffer-gtest-rule
   /*
    * Local Variables:
    * buffer-gtest-rule:"gtest2"
    * End:
    */
 * ESC W : whitespace-cleanup
 * ESC m : menu-bar-mode
 * ESC e : shell
 * ESC E : ansi-term
 * ESC 1 : workspace w1
 * ESC 2 : workspace w2
 * ESC 5 : open file at cursor
 * ESC 6 : find elisp func at cursor
 * ESC & : describe veriable under cursor
 * ESC 7 : cleanup whitespace
 * ESC F10 : open menu-bar in console mode
 * ESC ^ : Submenue
 * F2    : dired default-directory
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
 * M-n|p (Alt) : scroll without moving cursor
 * Xmonad: Win-Shift up/down/left/right : move window
 * Org-mode:
   * F1 (ESC a, Ctrl-c a) : org-agenda
   * F2 : filter items with :plan: tag
   * F4 : column view (r: recreate, q: quit column view) see [4]
   * M-F3/S-F3 : clock-in/clock-out
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
   * ESC . : gtags find ([1]), "M-x ggtags-create-tags" to create tags (ESC ? : xrefs)
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
   * ESC N : cling interpreter and .cling.cpp load
 * rust-mode:
   * TAB : call completion
 * elisp-mode:
   * M-; : show function help under point
 * ESC M-h : haskell interactive mode
 * haskell-mode:
   * F1 | C-c C-l : eval in repl
   * ESC i: intero mode (runs stack init)
   * F2 : typeinfo : c-c c-t
   * F4 : hoogle
   * ESC-;: generate etags file via hasktags, compile hasktags if it not exists
   * Haskel and hie:
     * ESC . : goto definition
	 ' ESC ; : show references
	 * ESC - : toggle lsp-ui-doc-mode, doc mode
 * raku-move:
   * F1 : raku repl
     * C-c C-c : Send the current line to the REPL
     * C-c C-r : Send the selected region to the REPL
     * C-c C-h : Send the whole buffer to the REPL

 * ESC q : grep in current dir ([1])
 * ESC Q : recursive grep from current dir ([1])
 * dired:
  * TAB   : toggle subtree
  * ESC f : recursive grep in current dir
  * C-fh  : recursive grep on dir under cursor
  * C-fg  : find-gre-dired
  * C-fn  : find-gre-name
 * Occur
  * ESC s : Occure inside buffers (CTRL-o to open location occur window)

HELM:
 * C-x b : helm buffer select
 * M-X   : helm version of M-x

Misc:
 * ESC d : toggle debug on elisp error
 * ESC : : eval elisp
 * ESC z : repeat last command (default:M-x z)(continue z to continue repeat)
 * ESC I : start irc
   * /LIST : list channels
   * C-c C-j : join
   * C-c C-p : part
   * C-c C-q : quit server
 * ESC j : goto-line
 * ESC P : enable powerline statusbar

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
  * ESC t : Cycle themes
  * ESC T : Toggle hide-node-line (hide modeline in single frame)
  * ESC M-t : Toggle xterm-window-mode (click on modeline bar)
 * VC mode
  * Ctrl-x v g : git blame
 * Debug
  * Ctrl-h v : show variable value and description
  * Ctrl-h b : show current keymap
  * Ctrl-h k : show keybinding
 * Spell
  * ESC i : ispell cycle language
 * yank
  * Alt-w : copy 

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
 * P r : push gerrit refspec (enter target and refspec)
 * Merge conflicts:
   * e : on _unmerged_ files opens ediff, second emacs window shows ediff ctrl:
     * |   : column view
     * a/b : select left/right
     * n/p : navigate diff sections
     * q   : finish
 * Ediff 3 colum view:
   * xy : i.e. press ab to move current hunk from a to b, ac to move current hunk from a to c
   * rx : i.e. press ra to restore current hunk in column a
 * SMerge:
   * Ctrl-c v n/p: navigate
   * Ctrl-c v ret: use version on cursor 
 * y   : branch viewer (k delet branch)
  * rebase:
  * r r : continue rebase
  * r s : skip and contine rebase

Ocaml debug (M-x ocamldebug):
 * Ctrl-x Ctrl-a:
   * Ctrl-b : set break
   * Ctrl-t : backtrace


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

* TMUX Ctrl-b:
 Alt-Left|Right|Up|Down : navigate tmux panes
 q: show pane-numbers
 X: show/hide status pane
 x: kill pane
 |: split horizontally
 -: split vertically
 d: detach

 C-b c : new window
 C-b & :kill window
 C-b , : rename window
 C-b $ : rename session
 C-b :new<CR> :new session

 Alt-q : detach
 Alt-Shift-q : kill-server
 Alt-Ctrl-Shift up|down|left|right: resize pane
 alt-Shift left|right : navigate windows
 alt-Shift up|down : navigate session
 Alt-c : add pane

 alt-pageup : scroll/copy-mode : http://hyperpolyglot.org/multiplexers
              q : quite mode
 
 logging:
  Ctrl-b alt-c   : clear  pane history
  Ctrl-b shift-p : start logging
  Ctrl-b alt-p   : screen capture logging
 resurrect:
  Ctrl-b Ctrl-s : save env
  Ctrl-b Ctrl-r : restore env
 copycat:
  ctrl-b /      : serach regex
  ctrl-b Ctrl-u : serach url
  ctrl-b Ctrl-f : serach file
  ctrl-b Ctrl-d : serach digit
   (n/N : sel up/down)
  

 copy-mode:
   Alt-f|b : navigate words
   C-w     : copy
   y       : yank to clipboard (xsel -b)
