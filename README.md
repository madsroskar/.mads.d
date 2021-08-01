# Table of Contents

1.  [About this repo](#orgd939782)
    1.  [What is this?](#orge80f29b)
    2.  [Goals](#org284f967)
    3.  [Prerequisites](#orgec8874c)
    4.  [Usage](#org6810ece)
2.  [Setup](#org103f523)
    1.  [Utilities](#org8811550)
        1.  [Evaluate needed utilities](#org30102da)
        2.  [Conditional tangle per OS](#orgcaa7e65)
        3.  [Prepend a common directory to tangle files to](#org080dc19)
        4.  [Automatic export on save](#org6b0adb2)
    2.  [Installed software](#org67d9ea5)
        1.  [Homebrew](#org5cbbec8)
    3.  [Complete list of custom keybinds](#orge852d7f)
    4.  [DOOM Emacs](#org3fd0dea)
        1.  [Config](#orgaafbc91)
        2.  [init.el](#org030ea69)
        3.  [custom.el](#orgd76a6dd)
        4.  [package.el](#org29a3070)
    5.  [Git](#org028d99b)
        1.  [Gitconfig](#org7bf126f)
        2.  [Gitignore](#org45c924a)
    6.  [zsh](#org283d813)
        1.  [Editor](#org0155edf)
        2.  [Path](#org46de3e0)
        3.  [Program specific configuration](#org49425c6)
        4.  [XDG Base Directories](#orgb39fc8a)
        5.  [oh-my-zsh](#orga4f60ac)
        6.  [.fzf.zsh](#org027982a)
    7.  [Alacritty](#orga05aad5)
        1.  [Environment variables](#org2e6010c)
        2.  [Window (frame)](#orgb8af27c)
        3.  [UI/&ldquo;Themeing&rdquo;](#orgff2e16e)
        4.  [Mouse](#orgf577841)
        5.  [Key bindings](#org07c3fbb)
        6.  [Misc](#org600403e)
    8.  [SKHD](#org268ba1f)
        1.  [Built-in modifiers](#org4d43716)
        2.  [Yabai (Window management)](#org3815281)
    9.  [Yabai](#org0fdf6a9)
        1.  [Startup](#orgb5dd909)
        2.  [Global settings](#orge391637)
        3.  [General space settings](#org161d381)
        4.  [Ignored programs](#orgaabde49)

<a id="orgd939782"></a>

# About this repo

<a id="orge80f29b"></a>

## What is this?

This repository keeps my personal configuration so set up my environment. Since being introduced
to emacs by a colleague<sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup>, I&rsquo;ve really started enjoying it, and especially `org-mode` and `magit`.

This setup is most likely just super redundant/overcomplicated, but it seemed like a fun time sink for a Saturday
to mess around a bit with `org-mode`, and some [simple emacs lisp functions](#org8811550).

Who I am:

    (setq user-full-name "Mads Røskar"
          user-mail-address "madshvero@gmail.com")

<a id="org284f967"></a>

## Goals

- All config/dotfiles in one place
- Work for each environment i&rsquo;d use
  - macOS
  - (TBD) Windows
  - (TBD) Linux
- Organized in a way that makes sense

<a id="orgec8874c"></a>

## Prerequisites

- git
- patience in case things break

<a id="org6810ece"></a>

## Usage

- `git clonei git@github.com:madsroskar/.mads.d.git`
- Execute the buffer
  `C-c C-v b`
- # Tangle everything

<a id="org103f523"></a>

# Setup

<a id="org8811550"></a>

## Utilities

<a id="org30102da"></a>

### Evaluate needed utilities

This just exists as a convenience so I can have the utilities evaluated at the same time, rather than one at a time.
At a later stage I should find a better way to go about this, but evaluating it as a file-local-variable is considered
a security risk, and I&rsquo;ll not spend too much time on that right now.

    (save-excursion
      (org-babel-goto-named-src-block "tangle-util")
      (org-babel-execute-src-block)
      (org-babel-goto-named-src-block "tangle-base-dir")
      (org-babel-execute-src-block)
      (org-babel-goto-named-src-block "save-and-export")
      (org-babel-execute-src-block)
      )

1.  TODO Find a way to not have to evaluate this manually, and preferably before the rest of the config is set up

    - Could possibly make it into a minor mode for myself? 🤔

2.  TODO Make these not expand their headings

<a id="orgcaa7e65"></a>

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

<a id="org080dc19"></a>

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

<a id="org6b0adb2"></a>

### Automatic export on save

I don&rsquo;t want to do `C-c C-e m M` all the time, so this is just a convenience to have that happen automatically.

    (defun save-and-export ()
      (interactive)
      (if (eq major-mode 'org-mode)
        (org-md-export-to-markdown)))
    (add-hook 'after-save-hook 'save-and-export nil t)

<a id="org67d9ea5"></a>

## Installed software

<a id="org5cbbec8"></a>

### Homebrew

1.  CLI

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">

    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <tbody>
    <tr>
    <td class="org-left">Program</td>
    <td class="org-left">Description</td>
    </tr>

    <tr>
    <td class="org-left">alpine</td>
    <td class="org-left">News and email agent</td>
    </tr>

    <tr>
    <td class="org-left">cabal-install</td>
    <td class="org-left">Command-line interface for Cabal and Hackage</td>
    </tr>

    <tr>
    <td class="org-left">cask</td>
    <td class="org-left">Emacs dependency management</td>
    </tr>

    <tr>
    <td class="org-left">cloc</td>
    <td class="org-left">Statistics utility to count lines of code</td>
    </tr>

    <tr>
    <td class="org-left">cmake</td>
    <td class="org-left">Cross-platform make</td>
    </tr>

    <tr>
    <td class="org-left">cmatrix</td>
    <td class="org-left">Console Matrix</td>
    </tr>

    <tr>
    <td class="org-left">composer</td>
    <td class="org-left">Dependency Manager for PHP</td>
    </tr>

    <tr>
    <td class="org-left">direnv</td>
    <td class="org-left">Load/unload environment variables based on $PWD</td>
    </tr>

    <tr>
    <td class="org-left">dnsmasq</td>
    <td class="org-left">Lightweight DNS forwarder and DHCP server</td>
    </tr>

    <tr>
    <td class="org-left">docker-compose</td>
    <td class="org-left">Isolated development environments using Docker</td>
    </tr>

    <tr>
    <td class="org-left">elixir</td>
    <td class="org-left">Functional metaprogramming aware language built on Erlang VM</td>
    </tr>

    <tr>
    <td class="org-left">figlet</td>
    <td class="org-left">Banner-like program prints strings as ASCII art</td>
    </tr>

    <tr>
    <td class="org-left">findutils</td>
    <td class="org-left">Collection of GNU find, xargs, and locate</td>
    </tr>

    <tr>
    <td class="org-left">fswatch</td>
    <td class="org-left">Monitor a directory for changes and run a shell command</td>
    </tr>

    <tr>
    <td class="org-left">fzf</td>
    <td class="org-left">Command-line fuzzy finder written in Go</td>
    </tr>

    <tr>
    <td class="org-left">gnu-sed</td>
    <td class="org-left">GNU implementation of the famous stream editor</td>
    </tr>

    <tr>
    <td class="org-left">go</td>
    <td class="org-left">Open source programming language to build simple/reliable/efficient software</td>
    </tr>

    <tr>
    <td class="org-left">hstr</td>
    <td class="org-left">Bash and zsh history suggest box</td>
    </tr>

    <tr>
    <td class="org-left">hugo</td>
    <td class="org-left">Configurable static site generator</td>
    </tr>

    <tr>
    <td class="org-left">irssi</td>
    <td class="org-left">Modular IRC client</td>
    </tr>

    <tr>
    <td class="org-left">isync</td>
    <td class="org-left">Synchronize a maildir with an IMAP server</td>
    </tr>

    <tr>
    <td class="org-left">skhd</td>
    <td class="org-left">Simple hotkey-daemon for macOS.</td>
    </tr>

    <tr>
    <td class="org-left">yabai</td>
    <td class="org-left">A tiling window manager for macOS based on binary space partitioning.</td>
    </tr>

    <tr>
    <td class="org-left">kubernetes-cli</td>
    <td class="org-left">Kubernetes command-line interface</td>
    </tr>

    <tr>
    <td class="org-left">librsvg</td>
    <td class="org-left">Library to render SVG files using Cairo</td>
    </tr>

    <tr>
    <td class="org-left">luarocks</td>
    <td class="org-left">Package manager for the Lua programming language</td>
    </tr>

    <tr>
    <td class="org-left">mariadb</td>
    <td class="org-left">Drop-in replacement for MySQL</td>
    </tr>

    <tr>
    <td class="org-left">mas</td>
    <td class="org-left">Mac App Store command-line interface</td>
    </tr>

    <tr>
    <td class="org-left">maven</td>
    <td class="org-left">Java-based project management</td>
    </tr>

    <tr>
    <td class="org-left">mkcert</td>
    <td class="org-left">Simple tool to make locally trusted development certificates</td>
    </tr>

    <tr>
    <td class="org-left">mu</td>
    <td class="org-left">Tool for searching e-mail messages stored in the maildir-format</td>
    </tr>

    <tr>
    <td class="org-left">neofetch</td>
    <td class="org-left">Fast, highly customisable system info script</td>
    </tr>

    <tr>
    <td class="org-left">neovim</td>
    <td class="org-left">Ambitious Vim-fork focused on extensibility and agility</td>
    </tr>

    <tr>
    <td class="org-left">ninja</td>
    <td class="org-left">Small build system for use with gyp or CMake</td>
    </tr>

    <tr>
    <td class="org-left">node</td>
    <td class="org-left">Platform built on V8 to build network applications</td>
    </tr>

    <tr>
    <td class="org-left">nss</td>
    <td class="org-left">Libraries for security-enabled client and server applications</td>
    </tr>

    <tr>
    <td class="org-left">nvm</td>
    <td class="org-left">Manage multiple Node.js versions</td>
    </tr>

    <tr>
    <td class="org-left">pgformatter</td>
    <td class="org-left">PostgreSQL syntax beautifier</td>
    </tr>

    <tr>
    <td class="org-left">php</td>
    <td class="org-left">General-purpose scripting language</td>
    </tr>

    <tr>
    <td class="org-left">python@3.8</td>
    <td class="org-left">Interpreted, interactive, object-oriented programming language</td>
    </tr>

    <tr>
    <td class="org-left">emacs-mac</td>
    <td class="org-left">YAMAMOTO Mitsuharu&rsquo;s Mac port of GNU Emacs</td>
    </tr>

    <tr>
    <td class="org-left">ripgrep</td>
    <td class="org-left">Search tool like grep and The Silver Searcher</td>
    </tr>

    <tr>
    <td class="org-left">powerlevel9k</td>
    <td class="org-left">A badass zsh theme with more power than a normal earthling</td>
    </tr>

    <tr>
    <td class="org-left">shellcheck</td>
    <td class="org-left">Static analysis and lint tool, for (ba)sh scripts</td>
    </tr>

    <tr>
    <td class="org-left">stow</td>
    <td class="org-left">Organize software neatly under a single directory tree (e.g. /usr/local)</td>
    </tr>

    <tr>
    <td class="org-left">swi-prolog</td>
    <td class="org-left">ISO/Edinburgh-style Prolog interpreter</td>
    </tr>

    <tr>
    <td class="org-left">speedtest</td>
    <td class="org-left">Ookla Speedtest</td>
    </tr>

    <tr>
    <td class="org-left">the<sub>silver</sub><sub>searcher</sub></td>
    <td class="org-left">Code-search similar to ack</td>
    </tr>

    <tr>
    <td class="org-left">tmux</td>
    <td class="org-left">Terminal multiplexer</td>
    </tr>

    <tr>
    <td class="org-left">tree</td>
    <td class="org-left">Display directories as trees (with optional color/HTML output)</td>
    </tr>

    <tr>
    <td class="org-left">vim</td>
    <td class="org-left">Vi &rsquo;workalike&rsquo; with many additional features</td>
    </tr>

    <tr>
    <td class="org-left">vlang</td>
    <td class="org-left">V programming language</td>
    </tr>

    <tr>
    <td class="org-left">watch</td>
    <td class="org-left">Executes a program periodically, showing output fullscreen</td>
    </tr>

    <tr>
    <td class="org-left">watchman</td>
    <td class="org-left">Watch files and take action when they change</td>
    </tr>

    <tr>
    <td class="org-left">wget</td>
    <td class="org-left">Internet file retriever</td>
    </tr>

    <tr>
    <td class="org-left">zlib</td>
    <td class="org-left">General-purpose lossless data-compression library</td>
    </tr>
    </tbody>
    </table>

<a id="orge852d7f"></a>

## Complete list of custom keybinds

    (map! :map org-mode-map
          :after org
          :n "M-l" #'org-shiftmetaright)

    (setq mac-command-modifier 'meta)
    (setq mac-pass-command-to-system nil)

    alt - h : yabai -m window --focus west
    alt - j : yabai -m window --focus south
    alt - k : yabai -m window --focus north
    alt - l : yabai -m window --focus east


    ctrl + shift - h  : yabai -m space --focus prev
    ctrl + shift - l  : yabai -m space --focus next
    ctrl + alt - 1  : yabai -m display --focus 1
    ctrl + alt - 2  : yabai -m display --focus 2
    ctrl + alt - 3  : yabai -m display --focus 3
    ctrl + alt - h  : yabai -m display --focus west
    ctrl + alt - l  : yabai -m display --focus east
    shift + alt - h : yabai -m window --warp west
    shift + alt - j : yabai -m window --warp south
    shift + alt - k : yabai -m window --warp north
    shift + alt - l : yabai -m window --warp east
    # Float/unfloat window
    shift + alt - space : \
        yabai -m window --toggle float; \
        yabai -m window --toggle border

    # make floating window fill screen
    shift + alt - up     : yabai -m window --grid 1:1:0:0:1:1
    # balance size of windows
    shift + alt - 0 : yabai -m space --balance
    # Restart Yabai
    shift + lctrl + alt - r : \
        /usr/bin/env osascript <<< \
            "display notification \"Restarting Yabai\" with title \"Yabai\""; \
        launchctl kickstart -k "gui/${UID}/homebrew.mxcl.yabai"

<a id="org3fd0dea"></a>

## DOOM Emacs

<a id="orgaafbc91"></a>

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
    better for me. See a [complete list of custom keybinds for an overview](#orge852d7f)

    Use command in macos as Meta, and don&rsquo;t pass it to the system:

        (setq mac-command-modifier 'meta)
        (setq mac-pass-command-to-system nil)

<a id="org030ea69"></a>

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

<a id="orgd76a6dd"></a>

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

<a id="org29a3070"></a>

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

<a id="org028d99b"></a>

## Git

<a id="org7bf126f"></a>

### Gitconfig

This file is pretty self explanatory. I have a few simple aliases here, but they&rsquo;ve been rendered unnecessary thanks to magit which is my new, and one true, love.

    [user]
    	name = Mads Røskar
    	email = madshvero@gmail.com
    [core]
    	excludesfile = ~/.gitignore
    [rerere]
    	enabled = 1
    [alias]
      c = commit
      s = status
      a = add
      sts = stash
      diffc = diff --color-words -U0
      llog = log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%aN>%Creset' --abbrev-commit --date=relative
    [github]
    	user = madsroskar
    [help]
      defaultBranch = main
    	# oauth-token = {{ Insert here, omitted causee public. :) }}

<a id="org45c924a"></a>

### Gitignore

Many of these are taken from [the gitignore repo by Github](https://github.com/github/gitignore).

1.  macOS

    1.  General

            .DS_Store
            .AppleDouble
            .LSOverride

    2.  Icon must end with two `\r`

            Icon

    3.  Thumbnails

            ._*

    4.  Files that might appear in the root of a volume

            .DocumentRevisions-V100
            .fseventsd
            .Spotlight-V100
            .TemporaryItems
            .Trashes
            .VolumeIcon.icns
            .com.apple.timemachine.donotpresent

    5.  Directories potentially created on remote AFP share

            .AppleDB
            .AppleDesktop
            Network Trash Folder
            Temporary Items
            .apdisk

<a id="org283d813"></a>

## zsh

<a id="org0155edf"></a>

### Editor

I&rsquo;ve been converted. I can no longer use anything else than emacs. This is far too good to have as a tool for me to be able to even try something else. I don&rsquo;t think it&rsquo;s stockholm syndrome, but I guess it&rsquo;s tough to know for sure if it&rsquo;s yourself.

    export EDITOR='emacsclient -t -c'

<a id="org46de3e0"></a>

### Path

I wasn&rsquo;t aware of the lowercase `path` array, but that makes dealing with the path environment variable so much better. Stumbled across it in a [StackOverflow answer](https://stackoverflow.com/a/18077919/2246084).

1.  Yarn

        path+=("$HOME/.yarn/bin")
        path+=("$HOME/.config/yarn/global/node_modules/.bin")

2.  Cabal

        path+=("$HOME/.cabal/bin")

3.  Golang

        path+=("$HOME/go/bin")

4.  Flutter

        path+=("$HOME/code/oss/flutter/bin")

5.  Make the path available to subprocesses

        export PATH

<a id="org49425c6"></a>

### Program specific configuration

1.  nvm

    nvm needs to be loaded for zsh to be able to use it.

        export NVM_DIR="$HOME/.nvm"
        [ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"
        [ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"

2.  direnv

    direnv needs to hook into zsh to set up environment variables.

        eval "$(direnv hook zsh)"

<a id="orgb39fc8a"></a>

### XDG Base Directories

I want to migrate all, or as many as possible, of my configuration and cache files to follow the [XDG Base Directory](https://wiki.archlinux.org/title/XDG_Base_Directory) [specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) to have less clutter in my home directory.

These environment variables should be used to configure where to look for these files.

    export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
    export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"

1.  Configurations to convert

    1.  [ ] Emacs

    2.  [ ] Alacritty

    3.  [ ] Karabiner

    4.  [ ] rvm

    5.  [ ] swi-prolog

    6.  [ ] yarn

    7.  [ ] skhd

    8.  [ ] Cargo

    9.  [ ] Docker

    10. Dart <code>[0/2]</code>

        1.  [ ] .dart

        2.  [ ] .dartServer

    11. [ ] Doom Emacs

    12. [ ] Expo

    13. [ ] .gem

    14. [ ] .ghc

    15. [ ] .gnupg

    16. [ ] .npm

    17. [ ] .nvm

    18. [ ] .oh-my-zsh

    19. [ ] .ssh

    20. [ ] .zsh<sub>sessions</sub>

2.  Cache

    Moving ZSH cache files into `$XDG_CACHE_HOME`

        export ZSH_COMPDUMP="$XDG_CACHE_HOME/zsh/.zcompdump"
        export HISTFILE="$XDG_CACHE_HOME/zsh/.zsh_history"

<a id="orga4f60ac"></a>

### oh-my-zsh

I should really spend time to dive deeper in what I actually need for oh-my-zsh, I&rsquo;ve just always used it as-is without thinking more about it 🤔

1.  TODO Install

    Haven&rsquo;t figured out what to do about installing stuff just yet..

2.  Add oh-my-zsh

        export ZSH="/Users/madshvero/.oh-my-zsh"

3.  Set a theme

    The only theme I really like of the ones I&rsquo;ve tried is `af-magic`, as it&rsquo;s very simple while still displaying the information I want for it to.

        ZSH_THEME="af-magic"

4.  Load

        source $ZSH/oh-my-zsh.sh

<a id="org027982a"></a>

### .fzf.zsh

I use the fuzzy finder by junegunn.

1.  Add fzf to path

        if [[ ! "$PATH" == */usr/local/opt/fzf/bin* ]]; then
          export PATH="${PATH:+${PATH}:}/usr/local/opt/fzf/bin"
        fi

2.  Add autocompletion

        [[ $- == *i* ]] && source "/usr/local/opt/fzf/shell/completion.zsh" 2> /dev/null

3.  Add keybindings

        source "/usr/local/opt/fzf/shell/key-bindings.zsh"

4.  `fd` is `cd` but ✨fuzzy✨

        fd() {
          local dir
          dir=$(find ${1:-.} -path '*/\.*' -prune \
                          -o -type d -print 2> /dev/null | fzf +m) &&
          cd "$dir"
        }

5.  Load

        [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

<a id="orga05aad5"></a>

## Alacritty

I currently use Alacritty as my terminal emulator, even though I very rarely need the terminal emulator anymore since I do just about everything in emacs.

Alacritty does feel pretty nice in use though, and is pretty simple to configure as it&rsquo;s a matter of dealing with one single yaml file - now in org-mode 😎

<a id="org2e6010c"></a>

### Environment variables

    env:
      TERM: xterm-256color

1.  List of used variables

    <!-- This HTML table template is generated by emacs 27.2 -->
    <table border="1">
      <tr>
        <td align="left" valign="top">
          &nbsp;TERM&nbsp;<br />
          &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br />
          &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br />
          &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br />
          &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        </td>
        <td align="left" valign="top">
          &nbsp;This&nbsp;value&nbsp;is&nbsp;used&nbsp;to&nbsp;set&nbsp;the&nbsp;`$TERM`&nbsp;environment&nbsp;&nbsp;&nbsp;&nbsp;<br />
          variable&nbsp;for&nbsp;each&nbsp;instance&nbsp;of&nbsp;Alacritty.&nbsp;If&nbsp;it&nbsp;is&nbsp;not&nbsp;<br />
          present,&nbsp;alacritty&nbsp;will&nbsp;check&nbsp;the&nbsp;local&nbsp;terminfo&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br />
          database&nbsp;and&nbsp;use&nbsp;`alacritty`&nbsp;if&nbsp;it&nbsp;is&nbsp;available,&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<br />
          otherwise&nbsp;`xterm-256color`&nbsp;is&nbsp;used.&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        </td>
      </tr>
    </table>

<a id="orgb8af27c"></a>

### Window (frame)

    window:
      padding:
        x: 6
        y: 6
      dynamic_padding: false
      decorations: buttonless
      title: Alacritty
      # Window class (Linux/BSD only):
      class:
        # Application instance name
        instance: Alacritty
        # General application class
        general: Alacritty
      # GTK theme variant (Linux/BSD only)
      #
      # Override the variant of the GTK theme. Commonly supported values are `dark` and `light`.
      # Set this to `None` to use the default theme variant.
      #gtk_theme_variant: None

<a id="orgff2e16e"></a>

### UI/&ldquo;Themeing&rdquo;

1.  Font

        # Font configuration
        font:
          # Normal (roman) font face
          normal:
            family: Hack Nerd Font
            style: Regular
          # Bold font face
          bold:
            family: Hack Nerd Font
            style: Bold
          # Italic font face
          italic:
            family: Hack Nerd Font
            style: Italic
          bold_italic:
            family: Hack Nerd Font
            style: Bold Italic
          # Point size
          size: 16.0

          # Offset is the extra space around each character. `offset.y` can be thought of
          # as modifying the line spacing, and `offset.x` as modifying the letter spacing.
          offset:
            x: 0
            y: 1

          # Thin stroke font rendering (macOS only)
          #
          # Thin strokes are suitable for retina displays, but for non-retina screens
          # it is recommended to set `use_thin_strokes` to `false`
          #
          # macOS >= 10.14.x:
          #
          # If the font quality on non-retina display looks bad then set
          # `use_thin_strokes` to `true` and enable font smoothing by running the
          # following command:
          #   `defaults write -g CGFontRenderingFontSmoothingDisabled -bool NO`
          #
          # This is a global setting and will require a log out or restart to take
          # effect.
          use_thin_strokes: true

        # If `true`, bold text is drawn using the bright color variants.
        draw_bold_text_with_bright_colors: false

2.  Colors

        colors:
          primary:
            background: '#1d1f21'
            foreground: '#c5c8c6'
          selection:
            text: '0x4a5568'
          # Normal colors
          normal:
            black:   '0x1c1f24'
            red:     '0xff6c6b'
            green:   '0x98be65'
            yellow:  '0xda8548'
            blue:    '0x51afef'
            magenta: '0xc678dd'
            cyan:    '0x5699af'
            white:   '0x202328'

          # Bright colors
          bright:
            black:   '0x5b6268'
            red:     '0xda8548'
            green:   '0x4db5bd'
            yellow:  '0xecbe7b'
            blue:    '0x3071db'   # This is 2257a0 in Doom Emacs but I lightened it.
            magenta: '0xa9a1e1'
            cyan:    '0x46d9ff'
            white:   '0xdfdfdf'
        background_opacity: 0.93

<a id="orgf577841"></a>

### Mouse

    mouse:
      hints:
        launcher:
          program: open
          args: []

<a id="org07c3fbb"></a>

### Key bindings

    key_bindings:
        # (Windows, Linux, and BSD only)
      - { key: V,         mods: Control|Shift, action: Paste                       }
      - { key: C,         mods: Control|Shift, action: Copy                        }
      - { key: Insert,    mods: Shift,         action: PasteSelection              }
      - { key: Key0,      mods: Control,       action: ResetFontSize               }
      - { key: Equals,    mods: Control,       action: IncreaseFontSize            }
      - { key: Plus,      mods: Control,       action: IncreaseFontSize            }
      - { key: Minus,     mods: Control,       action: DecreaseFontSize            }
      - { key: Minus,     mods: Control,       action: DecreaseFontSize            }
      - { key: F11,       mods: None,          action: ToggleFullscreen            }
      - { key: Paste,     mods: None,          action: Paste                       }
      - { key: Copy,      mods: None,          action: Copy                        }
      - { key: L,         mods: Control,       action: ClearLogNotice              }
      - { key: L,         mods: Control,       chars: "\x0c"                       }
      - { key: PageUp,    mods: None,          action: ScrollPageUp,   mode: ~Alt  }
      - { key: PageDown,  mods: None,          action: ScrollPageDown, mode: ~Alt  }
      - { key: Home,      mods: Shift,         action: ScrollToTop,    mode: ~Alt  }
      - { key: End,       mods: Shift,         action: ScrollToBottom, mode: ~Alt  }
        # Use option as Meta in macOs
      - { key: J,         mods: Alt,           chars: "\x1bj"                      }
      - { key: K,         mods: Alt,           chars: "\x1bk"                      }
      - { key: H,         mods: Alt,           chars: "\x1bh"                      }
      - { key: L,         mods: Alt,           chars: "\x1bl"                      }

<a id="org600403e"></a>

### Misc

    dynamic_title: true
    live_config_reload: true
    shell:
      program: /usr/local/bin/zsh
      args:
          - --login
    scrolling:
      # Maximum number of lines in the scrollback buffer.
      # Specifying '0' will disable scrolling.
      history: 5000

      # Number of lines the viewport will move for every line scrolled when
      # scrollback is enabled (history > 0).
      #multiplier: 3

      # Scroll to the bottom when new text is written to the terminal.
      #auto_scroll: false

    # Spaces per Tab (changes require restart)
    #
    # This setting defines the width of a tab in cells.
    #
    # Some applications, like Emacs, rely on knowing about the width of a tab.
    # To prevent unexpected behavior in these applications, it's also required to
    # change the `it` value in terminfo when altering this setting.
    #tabspaces: 8

<a id="org268ba1f"></a>

## SKHD

<a id="org4d43716"></a>

### Built-in modifiers

This is the list of built-in modifiers as described in the start of the default skhd configuration.

    # A list of all built-in modifier and literal keywords can
    # be found at https://github.com/koekeishiya/skhd/issues/1
    #
    # A hotkey is written according to the following rules:
    #
    #   hotkey       = <mode> '<' <action> | <action>
    #
    #   mode         = 'name of mode' | <mode> ',' <mode>
    #
    #   action       = <keysym> '[' <proc_map_lst> ']' | <keysym> '->' '[' <proc_map_lst> ']'
    #                  <keysym> ':' <command>          | <keysym> '->' ':' <command>
    #                  <keysym> ';' <mode>             | <keysym> '->' ';' <mode>
    #
    #   keysym       = <mod> '-' <key> | <key>
    #
    #   mod          = 'modifier keyword' | <mod> '+' <mod>
    #
    #   key          = <literal> | <keycode>
    #
    #   literal      = 'single letter or built-in keyword'
    #
    #   keycode      = 'apple keyboard kVK_<Key> values (0x3C)'
    #
    #   proc_map_lst = * <proc_map>
    #
    #   proc_map     = <string> ':' <command> | <string>     '~' |
    #                  '*'      ':' <command> | '*'          '~'
    #
    #   string       = '"' 'sequence of characters' '"'
    #
    #   command      = command is executed through '$SHELL -c' and
    #                  follows valid shell syntax. if the $SHELL environment
    #                  variable is not set, it will default to '/bin/bash'.
    #                  when bash is used, the ';' delimeter can be specified
    #                  to chain commands.
    #
    #                  to allow a command to extend into multiple lines,
    #                  prepend '\' at the end of the previous line.
    #
    #                  an EOL character signifies the end of the bind.
    #
    #   ->           = keypress is not consumed by skhd
    #
    #   *            = matches every application not specified in <proc_map_lst>
    #
    #   ~            = application is unbound and keypress is forwarded per usual, when specified in a <proc_map>
    #
    # A mode is declared according to the following rules:
    #
    #   mode_decl = '::' <name> '@' ':' <command> | '::' <name> ':' <command> |
    #               '::' <name> '@'               | '::' <name>
    #
    #   name      = desired name for this mode,
    #
    #   @         = capture keypresses regardless of being bound to an action
    #
    #   command   = command is executed through '$SHELL -c' and
    #               follows valid shell syntax. if the $SHELL environment
    #               variable is not set, it will default to '/bin/bash'.
    #               when bash is used, the ';' delimeter can be specified
    #               to chain commands.
    #
    #               to allow a command to extend into multiple lines,
    #               prepend '\' at the end of the previous line.
    #
    #               an EOL character signifies the end of the bind.

<a id="org3815281"></a>

### Yabai (Window management)

1.  Focus window

        alt - h : yabai -m window --focus west
        alt - j : yabai -m window --focus south
        alt - k : yabai -m window --focus north
        alt - l : yabai -m window --focus east


        ctrl + shift - h  : yabai -m space --focus prev
        ctrl + shift - l  : yabai -m space --focus next

2.  Fous monitor

        ctrl + alt - 1  : yabai -m display --focus 1
        ctrl + alt - 2  : yabai -m display --focus 2
        ctrl + alt - 3  : yabai -m display --focus 3
        ctrl + alt - h  : yabai -m display --focus west
        ctrl + alt - l  : yabai -m display --focus east

3.  Moving windows

        shift + alt - h : yabai -m window --warp west
        shift + alt - j : yabai -m window --warp south
        shift + alt - k : yabai -m window --warp north
        shift + alt - l : yabai -m window --warp east

4.  Floating windows

        # Float/unfloat window
        shift + alt - space : \
            yabai -m window --toggle float; \
            yabai -m window --toggle border

        # make floating window fill screen
        shift + alt - up     : yabai -m window --grid 1:1:0:0:1:1

5.  Window sizing

        # balance size of windows
        shift + alt - 0 : yabai -m space --balance

6.  Miscellaneous

        # Restart Yabai
        shift + lctrl + alt - r : \
            /usr/bin/env osascript <<< \
                "display notification \"Restarting Yabai\" with title \"Yabai\""; \
            launchctl kickstart -k "gui/${UID}/homebrew.mxcl.yabai"

<a id="org0fdf6a9"></a>

## Yabai

<a id="orgb5dd909"></a>

### Startup

    #!/usr/bin/env sh
    sudo yabai --load-sa
    yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"

<a id="orge391637"></a>

### Global settings

    yabai -m config mouse_follows_focus          off
    yabai -m config focus_follows_mouse          off
    yabai -m config window_placement             second_child
    yabai -m config window_topmost               off
    yabai -m config window_shadow                off
    yabai -m config window_opacity               off
    yabai -m config window_opacity_duration      0.5
    yabai -m config active_window_opacity        1.0
    yabai -m config normal_window_opacity        0.95
    yabai -m config window_border                on
    yabai -m config window_border_width          2
    yabai -m config active_window_border_color   0xff775759
    yabai -m config normal_window_border_color   0x00555555
    yabai -m config insert_feedback_color        0xffd75f5f
    yabai -m config split_ratio                  0.50
    yabai -m config auto_balance                 off
    yabai -m config mouse_modifier               fn
    yabai -m config mouse_action1                move
    yabai -m config mouse_action2                resize
    yabai -m config mouse_drop_action            swap

<a id="org161d381"></a>

### General space settings

    yabai -m config layout                       bsp
    yabai -m config top_padding                  0
    yabai -m config bottom_padding               0
    yabai -m config left_padding                 0
    yabai -m config right_padding                0
    yabai -m config window_gap                   0

<a id="orgaabde49"></a>

### Ignored programs

- &ldquo;System Preferences&rdquo;
- &ldquo;Finder&rdquo;
- &ldquo;Simulator&rdquo;

  for p in $programs; do
        yabai -m rule --add app="^$p$" manage=off
  done

  /usr/bin/env osascript <<< \
   "display notification \"Restarting Yabai\" with title \"Yabai\""; \
  launchctl kickstart -k "gui/${UID}/homebrew.mxcl.yabai"

# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> I will wait with adding a link here till I&rsquo;ve asked the person whether it&rsquo;s alright to link them here or not. Not a big deal in this case I&rsquo;d think, but rather safe than sorry 😊
