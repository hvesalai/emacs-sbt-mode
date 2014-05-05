sbt-mode
========

An emacs mode for interacting with [sbt](http://www.scala-sbt.org/),
scala console (aka *REPL*) and sbt projects.

Supported and tested versions include:
- sbt 0.12 and 0.13
- scala 2.9 and 2.10
- emacs 24

The mode provides basic functionality required for successfully
interacting with sbt from emacs. The core functionality includes:
- interacting with sbt shell and scala console
- compiling code and navigating to errors
- finding things in code

## Setting the mode up for use

1. Make sure you have the lastest version of **Gnu Emacs**
installed. The mode has been developed on 24.2.

    For best performance of scala console completions, also install
    the latest version of
    [scala-mode2](https://github.com/hvesalai/scala-mode2).

2. Tab-completion is available natively for sbt 0.13.2. For older
versions of sbt, install the [completions](completions.md) plugin
separately.

3. There are two mechanisms that can be used for the installation of
the mode into Emacs. The preferred manner is to use the built-in
package manager of Emacs 24 (i.e. `package.el`) and
the other is to manually clone the git repository, add the path to the mode
to the load-path and then to require it. For more information regarding
`package.el` please refer to the [EmacsWiki](http://emacswiki.org/emacs/ELPA).

    1. Package.el:
        Using the package.el within Emacs installation is the recommended
        manner to install sbt-mode as it allows for continuous, easy
        updating from within Emacs itself. Adding the MELPA
        repository to your emacs initialization will be required to locate
        the packages.

        Add the following to your emacs config (.emacs, init.el, etc).
        If such a definition already exists, ensure that it contains
        the MELPA declaration.

        ```lisp
        (require 'package)
        (add-to-list 'package-archives
                     '("melpa" . "http://melpa.milkbox.net/packages/") t)
        (package-initialize)
        (unless (package-installed-p 'sbt-mode)
          (package-refresh-contents) (package-install 'sbt-mode))
        ```

    2. Manual:
        Download the files to a local directory. You can use the *git clone*
        command, this will create a new directory called sbt-mode.

        ```
        git clone git://github.com/hvesalai/sbt-mode.git
        ```

        Include the following into your Emacs config file.

        ```lisp
        (add-to-list 'load-path "/path/to/sbt-mode/")
        (require 'sbt-mode)
        ```

4. That's it. Next you can start emacs in your project directory and
run sbt-start (use **M-x** *start-sbt*). You might also want to change
some keybindings to make better use of sbt-mode. If you have
customized your sbt project layout, you might also need to customize
some sbt-mode variables (run **M-x** *customize-mode* **RET**
*sbt-mode*).

## Important customization variables and other customizations

To work efficiently with sbt-mode, you should customize these
variables.

- *sbt:program-name* - the name of the sbt executable, defaults to `sbt`.
    Note: this variable is best configured throught the emacs customization
    menu (`M-x customize-variable` RET `sbt:program-name`) or set globally.
    You can not set it with the mode hook.
- *grep-find-ignored-directories* - directories not to include in searches.
    You should add the `target` directory and maybe remove many of the
    directories related to arcane version control tools that you will not
    have anyway.
- *grep-find-ignored-files* - a list of file patterns to ignore in searches.

You may also want to add a mode-hook to you `.emacs` file that alters
key-bindings and some settings.

```lisp
(add-hook 'sbt-mode-hook '(lambda ()
  ;; compilation-skip-threshold tells the compilation minor-mode
  ;; which type of compiler output can be skipped. 1 = skip info
  ;; 2 = skip info and warnings.
  (setq compilation-skip-threshold 1)

  ;; Bind C-a to 'comint-bol when in sbt-mode. This will move the
  ;; cursor to just after prompt.
  (local-set-key (kbd "C-a") 'comint-bol)

  ;; Bind M-RET to 'comint-accumulate. This will allow you to add
  ;; more than one line to scala console prompt before sending it
  ;; for interpretation. It will keep your command history cleaner.
  (local-set-key (kbd "M-RET") 'comint-accumulate)
))
```

Besides customizing sbt-mode, you might also want to add some
customizations to your
[scala-mode2](https://github.com/hvesalai/scala-mode2)
key-bindings. The following two commands are good to have in some
easily accessible key position.

```lisp
(add-hook 'scala-mode-hook '(lambda ()
   ;; sbt-find-definitions is a command that tries to find (with grep)
   ;; the definition of the thing at point.
   (local-set-key (kbd "M-.") 'sbt-find-definitions)

   ;; use sbt-run-previous-command to re-compile your code after changes
   (local-set-key (kbd "C-x '") 'sbt-run-previous-command)
))
```

Also check that your global binding for the *next-error* function is
satisfactory. It is by default bound to **M-\`** which might be hard to
access on some keyboard layouts where **`** is a dead key.
A good alternative is **M-'**.

```lisp
(global-set-key (kbd "M-'") 'next-error)
```

To alter the colors use by the sbt-mode, customize the following faces
using the **M-x** *customize-face* command.

- *sbt:warning*
- *sbt:error*
- *sbt:info*
- *compilation-error*
- *compilation-warning*

Alternatively you may globally set their respective *-face* variables
(e.g. *sbt:warning-face*) to point to your pre-defined face names.

## Commands and key-map

As sbt-mode is based on comint-mode and compilation-mode, all the
commands of those modes are available.

To see what commands sbt-mode adds, just type
**C-h f** *sbt-* **TAB** and choose a command to get its description.

To see the default key-map, see the help page for the sbt-mode:
**C-h f** *sbt-mode*.

## Tasks

### Starting sbt

Run **M-x** *sbt-start*. This will run the *sbt:program-name* executable,
which defaults to `sbt`.

Your sbt command history will be loaded and it is available through
the **M-x** *comint-previous-input* command which is bound by default
to the **M-p** key.

You may also run previous commands by going to the line in the buffer
that contains the command and pressing **RET**. To jump back to the
last line use **M->**.

### Compilation

After starting the sbt shell as described above, use the following
commands in emacs.

- **M-x** *sbt-command*
- **M-x** *sbt-run-previous-command*

The difference in using these compared to typing the command directly
to the sbt buffer, is that using these commands can be run from any
buffer with a file from the sbt project directory tree and that these
commands will reset the sbt buffer, making it easier to read the
compiler output and to navigate to any errors. Having
*sbt-run-previous-command* bound to some key-mapping is advisable.

After using either of these commands, if any buffers need saving, Emacs
will ask if you'd like to save them. You can set
`compilation-ask-about-save` to `nil` to skip the question and have
buffers saved automatically instead.

After running the compilation, you can navigate to the errors by
selecting the error line and pressing **RET** or using mouse. You may
also use the **M-x** *next-error* emacs command, which is by default
bound to **M-`**.

Tab completion is also available when using the described commands.

### Grepping code

A special version of the *rgrep* command is available with sbt-mode. Use
**M-x** *sbt-grep* to grep files in the sbt project. The command
mimics rgrep, so see **C-h f** *rgrep* for help.

### Finding things

Besides *sbt-grep* you have also two other commands available for
finding things at point using grep. These commands work more reliably
if you have [scala-mode2](https://github.com/hvesalai/scala-mode2)
installed.

- **M-x** *sbt-find-definitions* will search for the definition of the
    thing at point from the project `.scala` and `.java` files.
- **M-x** *sbt-find-usages* will search for occurances of the id at
    point from the project `.scala` and `.java` files.

Use the `next-error` and `previous-error` command (or key-binding) to
jump to the locus of each search hit.

### Scala console

You can start the scala console from within sbt as normally (use
*console* or *console-quick*). Your scala-console history will be
loaded on start-up.

When typing in multi-line code snippets, instead of using **RET** to
separate lines, use **M-x** *comint-accumulate* (or the respective
key-binding as adviced above in the customization section). This way,
if you need to modify the code, you can use **M-p** to recall the
whole snippet for reworking.

You can also send a region of code from an other buffer in the same
project. First set the mark to the other end of the region to send and
the point (cursor) the other. Then run the **M-x** *sbt-send-region*
command. The command will skip any whitespace or comments at the
beginning and end of the region.

You may prefer **M-x** *sbt-paste-region* to *sbt-send-region*.
*sbt-paste-region* will enter `:paste` mode of Scala REPL, so that
pasting multiline statement/expression will no longer confuse the
REPL.

## Credits

Mode development: Heikki Vesalainen

While a complete rewrite, the mode was inspired by the
sbt-support provided by the old scala-mode.
