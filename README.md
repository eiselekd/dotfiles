dotfiles
========

Emacs config. Install by replacing ~/.emacs.d with
"ln -s dotfiles-path/.emacs.d ~/.emacs.d"

    if [ -f ~/git/dotfiles/.bashrc_extra ]; then
	source ~/git/dotfiles/.bashrc_extra
    fi

Keybindings:

XMonad:
 * Xmonad: Win-Shift up/down/left/right : move window

Emacs:
 * ESC g : magit [2] 
 * ESC l : toggle mode-line
 * ESC e : shell
 * ESC E : ansi-term
 * ESC 5 : open file at cursor
 * Win up/down/left/right : jump window
 * M-S up/down/left/right : enlarge/shrink window
 * M-n/p : scrol with cursor fixed
 * c-mode/c++-mode:
   * ESC . : gtags find ([1]), "M-x ggtags-create-tags" to create tags (ESC ? : xrefs)
   * ESC , : pop markbuffer ([1])
 * C-c, C-v: cua-mode, copy/paste
 
 * ESC j : goto-line
 * ESC P : enable powerline statusbar
 * C-L : Zoom/Unzoom current pane
 * ESC W : whitespace-cleanup

 * M-J : japanese input, hiragana, katakana
 * C-\ : toggle eng/jap
 * C-x RET l Japanese RET : switch IME. , M-x set-input-method RET japanese-katakana RET
 * C-h L                  : IME help

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
  * ESC l : Toggle hide-node-line (statusbar) (hide modeline in single frame), no-fringes
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

* Links:

 * [1] helm is only loaded if ggtags loads. Therefore a fairly recent global (www.gnu.org/software/global/)
  has to be present
 * [2] Magit cheetsheet: https://github.com/magit/magit/wiki/Cheatsheet
 * [3] https://github.com/magit/magit/wiki/Cheatsheet
 * [4] https://endlessparentheses.com/it-s-magit-and-you-re-the-magician.html

* Org-mode timeentry:
 * [1] http://orgmode.org/manual/The-date_002ftime-prompt.html#The-date_002ftime-prompt
 * [2] http://orgmode.org/manual/Effort-estimates.html#Effort-estimates, https://writequit.org/denver-emacs/presentations/2017-04-11-time-clocking-with-org.html
 * [3] http://doc.norang.ca/org-mode.html#ReviewingEstimates

* TMUX Ctrl-b:

 * Alt-Left|Right|Up|Down : navigate tmux panes
 * q: show pane-numbers
 * X: show/hide status pane
 * x: kill pane
 * |: split horizontally
 * -: split vertically
 * d: detach
 * z: maximize/restore current pane

 * C-b c : new window
 * C-b & :kill window
 * C-b , : rename window
 * C-b $ : rename session
 * C-b :new<CR> :new session

 * Alt-q : detach
 * Alt-Shift-q : kill-server
 * Alt-Ctrl-Shift up|down|left|right: resize pane
 * alt-Shift left|right : navigate windows
 * alt-Shift up|down : navigate session
 * Alt-c : add pane

 * alt-pageup : scroll/copy-mode : http://hyperpolyglot.org/multiplexers
              q : quite mode
 
 * logging:
  * Ctrl-b alt-c   : clear  pane history
  * Ctrl-b shift-p : start logging
  * Ctrl-b alt-(shift-)p   : screen capture logging (with hist)
 * resurrect:
  * Ctrl-b Ctrl-s : save env
  * Ctrl-b Ctrl-r : restore env
 * copycat:
  * ctrl-b /      : serach regex
  * ctrl-b Ctrl-u : serach url
  * ctrl-b Ctrl-f : serach file
  * ctrl-b Ctrl-d : serach digit
    (n/N : sel up/down)
 * yank:
  * ctrl-b y : copy clipboard
  * y/Y : copy mode
 * copy-mode:
  * Alt-f|b : navigate next/prev words
  * C-w     : copy
  * y       : yank to clipboard (xsel -b)
