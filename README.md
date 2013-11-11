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

1. Make sure you have the lastest version of **Gnu Emacs** installed. The mode has been developed on 24.2. 

    For best performance of scala console completions, also install the latest version of [scala-mode2](https://github.com/hvesalai/scala-mode2).

2. Add the CompletionPlugin to your sbt environment. This will allow you to tab-complete sbt commands from emacs. It is advisable to make the plugin available to all your projects at once by adding it to your `~/.sbt` directory.

    The directory depends on the sbt version:

    - sbt 0.12: `~/.sbt/plugins/`
    - sbt 0.13: `~/.sbt/0.13/plugins/`

    You need these two files in the plugins directory:

    `CompletionsPlugin.scala`:

    ```scala
    import sbt._
    import Keys._
    import sbt.complete._

    object CompletionsPlugin extends Plugin {
      override lazy val settings = Seq(commands += completions)

      lazy val completions = Command.single("completions") { (state, input) =>
        val str = if (input == "\"\"") "" else input

        Parser.completions(state.combinedParser, str, 1).get map {
          c => if (c.isEmpty) str else str + c.append
        } foreach { c =>
          println("[completions] " + c.replaceAll("\n", " "))
        }

        state
      }
    }

    ```

    `build.sbt`:


    ```
    sbtPlugins := true
    ```

2. Install the mode into emacs. Currently the mode is not available through any package manager (that will change soon), so the only way is to do the old-style manual install:

    1. Manual:
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

3. That's it. Next you can start emacs in your project directory and
run sbt-start (use **M-x** *start-sbt*). You might also want to change
some keybindings to make better use of sbt-mode. If you have
customized your sbt project layout, you might also need to customize
some sbt-mode variables (run **M-x** *customize-mode* **RET**
*sbt-mode*).

## Important customization variables and other customizations

To work efficiently with sbt-mode, you should customize these
variables.

- *sbt:program-name* - the name of the sbt executable, defaults to `sbt`
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

## Commands and key-map

As sbt-mode is based on comint-mode and compilation-mode, all the commands of those modes are available.

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

### Compilation

After starting the sbt shell as described above, use the following
commands in emacs.

- **M-x** *sbt-command*
- **M-x** *sbt-run-previous-command* commands. 

The difference in using these compared to typing the command directly
to the sbt buffer, is that using these commands can be run from any
buffer with a file from the sbt project directory tree and that these
commands will reset the sbt buffer, making it easier to read the
compiler output and to navigate to any errors. Having
*sbt-run-previous-command* bound to some key-mapping is advisable.

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

### Scala console

You can start the scala console from within sbt as normally (use
*console* or *console-quick*). Your scala-console history will be
loaded on start-up.

When typing in multi-line code snippets, instead of using **RET** to
separate lines, use **M-x** *comint-accumulate* (or the respective
key-binding as adviced above in the customization section). This way,
if you need to modify the code, you can use **M-p** to recall the
whole snippet for reworking.

## Credits

Mode development: Heikki Vesalainen

While a complete rewrite, the mode was inspired by the 
sbt-support provided by the old scala-mode.
