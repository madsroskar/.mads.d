# Table of Contents

1.  [About this repo](#orga55b4a0)
    1.  [What is this?](#org0a2e874)
    2.  [Goals](#org1906574)
    3.  [Prerequisites](#orgf25fd1a)
    4.  [Usage](#org8a99bfa)
2.  [Setup](#org05db708)
    1.  [Utilities](#org796fc5e)
        1.  [Evaluate needed utilities](#orgb282a70)
        2.  [Conditional tangle per OS](#orgec60a10)
        3.  [Prepend a common directory to tangle files to](#orgc0ac0da)
    2.  [Complete list of custom keybinds](#org536efef)
    3.  [DOOM Emacs](#org187e1e4)
        1.  [Config](#orgcdbc0b3)
        2.  [init.el](#org1de2281)
        3.  [custom.el](#org7d69eb1)
        4.  [package.el](#orgbeb5c68)

<a id="orga55b4a0"></a>

# About this repo

<a id="org0a2e874"></a>

## What is this?

This repository keeps my personal configuration so set up my environment. Since being introduced
to emacs by a colleague<sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup>, I&rsquo;ve really started enjoying it, and especially `org-mode` and `magit`.

This setup is most likely just super redundant/overcomplicated, but it seemed like a fun time sink for a Saturday
to mess around a bit with `org-mode`, and some [simple emacs lisp functions](#org796fc5e).

Who I am:

    (setq user-full-name "Mads Røskar"
          user-mail-address "madshvero@gmail.com")

<a id="org1906574"></a>

## Goals

- All config/dotfiles in one place
- Work for each environment i&rsquo;d use
  - macOS
  - (TBD) Windows
  - (TBD) Linux
- Organized in a way that makes sense

<a id="orgf25fd1a"></a>

## Prerequisites

- git
- patience in case things break

<a id="org8a99bfa"></a>

## Usage

- `git clonei git@github.com:madsroskar/.mads.d.git`
- Execute the buffer
  `C-c C-v b`
- # Tangle everything

<a id="org05db708"></a>

# Setup

<a id="org796fc5e"></a>

## Utilities

<a id="orgb282a70"></a>

### Evaluate needed utilities

This just exists as a convenience so I can have the utilities evaluated at the same time, rather than one at a time.
At a later stage I should find a better way to go about this, but evaluating it as a file-local-variable is considered
a security risk, and I&rsquo;ll not spend too much time on that right now.

    (save-excursion
      (org-babel-goto-named-src-block "tangle-util")
      (org-babel-execute-src-block)
      (org-babel-goto-named-src-block "tangle-base-dir")
      (org-babel-execute-src-block)
      )

1.  TODO Find a way to not have to evaluate this manually, and preferably before the rest of the config is set up

    - Could possibly make it into a minor mode for myself? 🤔

<a id="orgec60a10"></a>

### Conditional tangle per OS

To be able to be selective about which OS a file should be tangled for, I have
a simple function to pass to the tangle header argument for simplicity.

Checking the system type is based on [this stackexchange answer](https://emacs.stackexchange.com/a/14034).

    (defun tos (os file-name)
      "tos (tangle OS) returns the passed file-name if the current system is the passed OS, with shortened names for convenience"
      (let ((st (cond
                 ((equal os "mac") 'darwin)
                 ((equal os "linux") 'gnu/linux))
                ))
        (when (eq system-type st) file-name)
        ))

If ran on macOS, it will look like the following:

    (tos "mac" "right/here")

    #+RESULTS:
    : right/here

<a id="orgc0ac0da"></a>

### Prepend a common directory to tangle files to

To avoid having to change the base directory I tangle files to, I have a simple helper
function to prepend a common base directory where I want to tangle all configuration to.

That directory will (for now at least) be my source for `stow`.

> But Mads, why don&rsquo;t you just do this inline on code blocks?

Idk, just seemed easier, and I can change it around later if I want, don&rsquo;t judge I have minimal elisp experience so far

    (setq base-tangle-directory "./.files/")

    (defun tdir (file-name)
      "tdir (tangle directory) takes a file name, and prepends it with a common base directory"
        (concat base-tangle-directory file-name)
      )

The usage will be:

    (tdir ".config/mads/.sleep-schedule")

    #+RESULTS:
    : ./files/.config/mads/.sleep-schedule

<a id="org536efef"></a>

## Complete list of custom keybinds

    (map! :map org-mode-map
          :after org
          :n "M-l" #'org-shiftmetaright)

    (setq mac-command-modifier 'meta)
    (setq mac-pass-command-to-system nil)

<a id="org187e1e4"></a>

## DOOM Emacs

<a id="orgcdbc0b3"></a>

### Config

This is my custom configuration for doom emacs, and all the underlying code blocks will tangle to `.doom.d/config.el`.
I have disabled the literate config module in doom emacs since I will keep the literate version of it here pre tangle
anyways.

1.  Theme / looks

    I used to use the `doom-dracula` theme, and have the same theme set up for several other applications, but
    for some novelty I switch the themes around a bit at times. For now, swapping between these two themes seems
    to be more than good enough:

        ;; (setq doom-theme 'doom-dracula)
        (setq doom-theme 'doom-tomorrow-night)

    I tried having a transparent background for my frame, but it seemed
    like macOS wasn&rsquo;t all that much into it and turned into jet engine mode. I legit thought it was going to lift off
    from my desk.

        ;;(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
        ;;(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

    Don&rsquo;t need to set the title for xterm

        (after! xterm
          (setq xterm-set-window-title nil))

    Show the time and battery status in the modeline.
    Currently commented out as it seemed like it was producing an error,
    will have to check that out later

        ;; (display-time-mode 1)
        ;; (unless (string-match-p "^Power N/A" (battery))
        ;;   (display-battery-mode 1))

    Set frame title depending on whether the file in the active buffer has been
    modified since the last write or not

        (setq frame-title-format
              '(""
                (:eval
                 (let ((project-name (projectile-project-name)))
                   (unless (string= "-" project-name)
                     (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

    1.  TODO Font (Shell script? elisp? whoknows)

        I kind of arbitrarily decided to try out the Hack Nerd Font, and somehow can&rsquo;t imagine using anything else right now,
        so I&rsquo;ve set that up for myself. However, this naturally needs to be installed before use.

        Steps to get this working:

        1.  Install the font on your system
            1.  macOS (Baaaah, can&rsquo;t figure out how to make it only write the file, poopoooooo)
            2.  Linux
        2.  Have doom use the Hack Nerd Font

2.  Org-mode

    I currently just use the default setting for the org mode directory:

        (setq org-directory "~/org/")

    I also have the agenda files in the same directory. As for right now I haven&rsquo;t spend the
    time or effort needed to get into using the agenda views at all, so this might have to change
    for all I know.

        (setq org-agenda-files '("~/org"))

    Custom org headline bullets so I&rsquo;ll look cool if somebody sees me in org-mode:

        (setq
            org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")
        )

    The original mapping for shiftmetaright was a bit wonky, `M-l` works better for me

        (map! :map org-mode-map
              :after org
              :n "M-l" #'org-shiftmetaright)

3.  Keybinds

    Most keybinds are set up by doom and evil-mode, but a few have been added or changed to work
    better for me. See a [complete list of custom keybinds for an overview](#org536efef)

    Use command in macos as Meta, and don&rsquo;t pass it to the system:

        (setq mac-command-modifier 'meta)
        (setq mac-pass-command-to-system nil)

<a id="org1de2281"></a>

### init.el

This is the entry point to the DOOM configuration, which sets up modules to be used with the setup.

    ;;; init.el -*- lexical-binding: t; -*-

    ;; This file controls what Doom modules are enabled and what order they load
    ;; in. Remember to run 'doom sync' after modifying it!

    ;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
    ;;      documentation. There you'll find a "Module Index" link where you'll find
    ;;      a comprehensive list of Doom's modules and what flags they support.

    ;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
    ;;      'C-c c k' for non-vim users) to view its documentation. This works on
    ;;      flags as well (those symbols that start with a plus).
    ;;
    ;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
    ;;      directory (for easy access to its source code).

    (doom! :input
           ;;chinese
           ;;japanese
           ;;layout            ; auie,ctsrnm is the superior home row

           :completion
           company           ; the ultimate code completion backend
           ;;helm              ; the *other* search engine for love and life
           ;;ido               ; the other *other* search engine...
           ivy               ; a search engine for love and life

           :ui
           deft              ; notational velocity for Emacs
           doom              ; what makes DOOM look the way it does
           doom-dashboard    ; a nifty splash screen for Emacs
           doom-quit         ; DOOM quit-message prompts when you quit Emacs
           (emoji +unicode)  ; 🙂
           ;;fill-column       ; a `fill-column' indicator
           hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
           ;;hydra
           ;;indent-guides     ; highlighted indent columns
           ligatures         ; ligatures and symbols to make your code pretty again
           ;;minimap           ; show a map of the code on the side
           modeline          ; snazzy, Atom-inspired modeline, plus API
           nav-flash         ; blink cursor line after big motions
           ;;neotree           ; a project drawer, like NERDTree for vim
           ophints           ; highlight the region an operation acts on
           (popup +defaults)   ; tame sudden yet inevitable temporary windows
           ;;tabs              ; a tab bar for Emacs
           ;;treemacs          ; a project drawer, like neotree but cooler
           unicode           ; extended unicode support for various languages
           vc-gutter         ; vcs diff in the fringe
           vi-tilde-fringe   ; fringe tildes to mark beyond EOB
           ;;window-select     ; visually switch windows
           workspaces        ; tab emulation, persistence & separate workspaces
           zen               ; distraction-free coding or writing

           :editor
           (evil +everywhere); come to the dark side, we have cookies
           file-templates    ; auto-snippets for empty files
           fold              ; (nigh) universal code folding
           (format +onsave)  ; automated prettiness
           ;;god               ; run Emacs commands without modifier keys
           ;;lispy             ; vim for lisp, for people who don't like vim
           ;;multiple-cursors  ; editing in many places at once
           ;;objed             ; text object editing for the innocent
           ;;parinfer          ; turn lisp into python, sort of
           ;;rotate-text       ; cycle region at point between text candidates
           snippets          ; my elves. They type so I don't have to
           word-wrap         ; soft wrapping with language-aware indent

           :emacs
           dired             ; making dired pretty [functional]
           electric          ; smarter, keyword-based electric-indent
           ;;ibuffer         ; interactive buffer management
           undo              ; persistent, smarter undo for your inevitable mistakes
           vc                ; version-control and Emacs, sitting in a tree

           :term
           ;;eshell            ; the elisp shell that works everywhere
           ;;shell             ; simple shell REPL for Emacs
           ;;term              ; basic terminal emulator for Emacs
           vterm             ; the best terminal emulation in Emacs

           :checkers
           syntax              ; tasing you for every semicolon you forget
           ;;(spell +flyspell) ; tasing you for misspelling mispelling
           ;;grammar           ; tasing grammar mistake every you make

           :tools
           ;;ansible
           ;;debugger          ; FIXME stepping through code, to help you add bugs
           direnv
           docker
           editorconfig      ; let someone else argue about tabs vs spaces
           ;;ein               ; tame Jupyter notebooks with emacs
           (eval +overlay)     ; run code, run (also, repls)
           gist              ; interacting with github gists
           lookup              ; navigate your code and its documentation
           lsp
           magit             ; a git pgrcelain for Emacs
           ;;make              ; run make tasks from Emacs
           ;;pass              ; password manager for nerds
           ;;pdf               ; pdf enhancements
           ;;prodigy           ; FIXME managing external services & code builders
           rgb               ; creating color strings
           taskrunner        ; taskrunner for all your projects
           ;;terraform         ; infrastructure as code
           ;;tmux              ; an API for interacting with tmux
           ;;upload            ; map local to remote projects via ssh/ftp

           :os
           (:if IS-MAC macos)  ; improve compatibility with macOS
           ;;tty               ; improve the terminal Emacs experience

           :lang
           ;;agda              ; types of types of types of types...
           ;;beancount         ; mind the GAAP
           ;;cc                ; C > C++ == 1
           ;;clojure           ; java with a lisp
           ;;common-lisp       ; if you've seen one lisp, you've seen them all
           ;;coq               ; proofs-as-programs
           ;;crystal           ; ruby at the speed of c
           ;;csharp            ; unity, .NET, and mono shenanigans
           ;;data              ; config/data formats
           ;;(dart +flutter)   ; paint ui and not much else
           ;;elixir            ; erlang done right
           ;;elm               ; care for a cup of TEA?
           emacs-lisp        ; drown in parentheses
           ;;erlang            ; an elegant language for a more civilized age
           ;;ess               ; emacs speaks statistics
           ;;factor
           ;;faust             ; dsp, but you get to keep your soul
           ;;fsharp            ; ML stands for Microsoft's Language
           ;;fstar             ; (dependent) types and (monadic) effects and Z3
           ;;gdscript          ; the language you waited for
           (go +lsp)         ; the hipster dialect
           (haskell +dante)  ; a language that's lazier than I am
           ;;hy                ; readability of scheme w/ speed of python
           ;;idris             ; a language you can depend on
           json              ; At least it ain't XML
           (java +lsp) ; the poster child for carpal tunnel syndrome
           (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
           ;;julia             ; a better, faster MATLAB
           ;;kotlin            ; a better, slicker Java(Script)
           ;;latex             ; writing papers in Emacs has never been so fun
           ;;lean              ; for folks with too much to prove
           ;;ledger            ; be audit you can be
           ;;lua               ; one-based indices? one-based indices
           markdown          ; writing docs for people to ignore
           ;;nim               ; python + lisp at the speed of c
           ;;nix               ; I hereby declare "nix geht mehr!"
           ;;ocaml             ; an objective camel
           (org +pretty)               ; organize your plain life in plain text
           ;;php               ; perl's insecure younger brother
           ;;plantuml          ; diagrams for confusing people more
           ;;purescript        ; javascript, but functional
           ;;python            ; beautiful is better than ugly
           ;;qt                ; the 'cutest' gui framework ever
           ;;racket            ; a DSL for DSLs
           ;;raku              ; the artist formerly known as perl6
           rest              ; Emacs as a REST client
           ;;rst               ; ReST in peace
           ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
           ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
           ;;scala             ; java, but good
           ;;(scheme +guile)   ; a fully conniving family of lisps
           sh                ; she sells {ba,z,fi}sh shells on the C xor
           ;;sml
           ;;solidity          ; do you need a blockchain? No.
           (swift +lsp)             ; who asked for emoji variables?
           ;;terra             ; Earth and Moon in alignment for performance.
           web              ; the tubes
           typescript-language-server
           yaml              ; JSON, but readable
           ;;zig               ; C, but simpler

           :email
           (mu4e +gmail)
           ;;notmuch
           ;;(wanderlust +gmail)

           :app
           (calendar)
           ;;emms
           ;;everywhere        ; *leave* Emacs!? You must be joking
           irc               ; how neckbeards socialize
           ;;(rss +org)        ; emacs as an RSS reader
           twitter           ; twitter client https://twitter.com/vnought

           :config
           ;; literate
           (default +bindings +smartparens))

<a id="org7d69eb1"></a>

### custom.el

This file captures configuration set by different commands, such as `M-x package-install RET`, and adding
files for the org agenda. Optimally these values should be set manually, as they won&rsquo;t automatically (for now at least)
be synced back here. Yay manual work woo 🎉

    (custom-set-variables
     ;; custom-set-variables was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(org-agenda-files
       '("/Users/madshvero/org/flow.org" "/Users/madshvero/org/README.org" "/Users/madshvero/org/jar-rewrite.org" "/Users/madshvero/org/journal.org" "/Users/madshvero/org/learning-elisp.org" "/Users/madshvero/org/notes.org" "/Users/madshvero/org/projects.org" "/Users/madshvero/org/todo.org"))
     '(package-selected-packages
       '(define-word ox-gfm mvn exec-path-from-shell magit-gh-pulls lsp-mssql org-plus-contrib spotify ediprolog yarn-mode web-mode typescript-mode)))
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     )

1.  TODO extract the packages and config from this file into the proper config

<a id="orgbeb5c68"></a>

### package.el

This file should define all packages I want to have available, on top of what doom gives me.

    ;; -*- no-byte-compile: t; -*-
    ;;; $DOOMDIR/packages.el

    ;; To install a package with Doom you must declare them here and run 'doom sync'
    ;; on the command line, then restart Emacs for the changes to take effect -- or
    ;; use 'M-x doom/reload'.


    ;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
    ;(package! some-package)

    ;; To install a package directly from a remote git repo, you must specify a
    ;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
    ;; https://github.com/raxod502/straight.el#the-recipe-format
    ;(package! another-package
    ;  :recipe (:host github :repo "username/repo"))

    ;; If the package you are trying to install does not contain a PACKAGENAME.el
    ;; file, or is located in a subdirectory of the repo, you'll need to specify
    ;; `:files' in the `:recipe':
    ;(package! this-package
    ;  :recipe (:host github :repo "username/repo"
    ;           :files ("some-file.el" "src/lisp/*.el")))

    ;; If you'd like to disable a package included with Doom, you can do so here
    ;; with the `:disable' property:
    ;(package! builtin-package :disable t)

    ;; You can override the recipe of a built in package without having to specify
    ;; all the properties for `:recipe'. These will inherit the rest of its recipe
    ;; from Doom or MELPA/ELPA/Emacsmirror:
    ;(package! builtin-package :recipe (:nonrecursive t))
    ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

    ;; Specify a `:branch' to install a package from a particular branch or tag.
    ;; This is required for some packages whose default branch isn't 'master' (which
    ;; our package manager can't deal with; see raxod502/straight.el#279)
    ;(package! builtin-package :recipe (:branch "develop"))

    ;; Use `:pin' to specify a particular commit to install.
    ;(package! builtin-package :pin "1a2b3c4d5e")


    ;; Doom's packages are pinned to a specific commit and updated from release to
    ;; release. The `unpin!' macro allows you to unpin single packages...
    ;(unpin! pinned-package)
    ;; ...or multiple packages
    ;(unpin! pinned-package another-pinned-package)
    ;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
    ;(unpin! t)
    ;
    (package! graphql-mode)
    (package! format-all)

# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> I will wait with adding a link here till I&rsquo;ve asked the person whether it&rsquo;s alright to link them here or not. Not a big deal in this case I&rsquo;d think, but rather safe than sorry 😊
