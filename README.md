Control Mode
============

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/control-mode-badge.svg)](http://melpa.org/#/control-mode)

* [Installation](#installation)
  * [MELPA](#melpa)
  * [Manual Installation](#manual-installation)
  * [Setup](#setup)
* [What It Does](#what-it-does)
  * [Examples](#examples)
  * [Regenerating Key Bindings](#regenerating-key-bindings)
* [Tips](#tips)
* [Customization](#customization)
* [License](#license)

Control Mode is a minor mode for Emacs that provides a “control” mode,
similar in purpose to Vim's “normal” mode. Unlike the various Vim emulation
modes, the key bindings in Control Mode are derived from the key bindings
already setup, usually by making the control key unnecessary,
e.g. <kbd>C-f</kbd> becomes <kbd>f</kbd>. This provides the power of a mode
dedicated to controlling the editor without needing to learn or maintain new
key bindings.

Installation
------------

### MELPA

If you haven't already, add the following lines to your `.emacs.d/init.el`
and restart Emacs so that you can install packages from MELPA:

```emacs-lisp
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
```

Then do <kbd>M-x package-install RET control-mode RET</kbd>.

### Manual Installation

Download the `control-mode.el` file and put it in your `.emacs.d`
directory. Add the following lines to your `.emacs.d/init.el`:

```emacs-lisp
(add-to-list 'load-path "~/.emacs.d/")
(require 'control-mode)
```

### Setup

Once you have the control-mode package installed, you can add the following
line to your `.emacs`:

```emacs-lisp
(control-mode-default-setup)
```

This will setup <kbd>C-z</kbd> to turn on Control Mode globally, and
<kbd>C-z</kbd> and <kbd>z</kbd> to turn it off globally. If you prefer to
use it on a buffer by buffer basis, use `control-mode-localized-setup`. If
you need the usual binding for `Ctrl-z` to suspend Emacs, you can use
`Ctrl-x Ctrl-z` instead (`x Ctrl-z` in Control Mode). It also binds `x f` to
`find-file` (or whatever you had bound to `Ctrl-x Ctrl-f`) in Control Mode
if it would otherwise be bound to `set-fill-column`.

If you want to run `control-mode` globally as above but want to disable it for
some specific modes, add the mode to the `global-control-mode-exceptions` custom
variable.

What It Does
------------

Control Mode looks at every key binding you already have defined. For each
binding that includes <kbd>⎈ Ctrl</kbd>, it tries to rebind it without
<kbd>⎈ Ctrl</kbd>. It will only do this if the key binding it is replacing
is unbound or bound to `self-insert-command` or `org-self-insert-command`,
the Emacs commands for keys that simply enter themselves. It will also look
at all bindings with <kbd>◆ Meta</kbd> (<kbd>⎇ Alt</kbd> on most keyboards),
and try to rebind those without the <kbd>◆ Meta</kbd>. <kbd>⎈ Ctrl</kbd>
bindings take precedence over <kbd>◆ Meta</kbd> bindings.

An exception is made for <kbd>C-m</kbd> and <kbd>C-i</kbd>, which are
usually synonyms for <kbd>↵ Enter</kbd> and <kbd>↹ Tab</kbd> in Emacs. They
will be ignored, allowing <kbd>M-m</kbd> and <kbd>M-i</kbd> to be bound to
<kbd>m</kbd> and <kbd>i</kbd>.

<kbd>C-M</kbd> combinations also get rebound. <kbd>C-M</kbd> will get bound
to <kbd>⎈ Ctrl</kbd> if <kbd>⎈ Ctrl</kbd> unbound or rebound, and to <kbd>◆
Meta</kbd> if <kbd>◆ Meta</kbd> was unbound or rebound. If you set the
variable `control-mode-rebind-to-shift` to `t` Control Mode will also try to
rebind to <kbd>⇧ Shift</kbd> if that binding wouldn't already be taken over
by a <kbd>⎈ Ctrl</kbd> + <kbd>⇧ Shift</kbd> or <kbd>◆ Meta</kbd> + <kbd>⇧
Shift</kbd> binding. This may interfere with the use of <kbd>⇧ Shift</kbd>
with movement commands to select a region however, and so is off by default.

Control Mode does the right thing when a key binding includes modifiers
other than <kbd>⎈ Ctrl</kbd> and <kbd>◆ Meta</kbd>. For example, it will
rebind <kbd>⎈ Ctrl</kbd> + <kbd>⇧ Shift</kbd> + <kbd>⌫ Backspace</kbd> to
<kbd>⇧ Shift</kbd> + <kbd>⌫ Backspace</kbd> if <kbd>⇧ Shift</kbd> + <kbd>⌫
Backspace</kbd> has a key binding it is allowed to replace, and it will try
to rebind <kbd>⎈ Ctrl</kbd> + <kbd>◆ Meta</kbd> + <kbd>Super</kbd> +
<kbd>Hyper</kbd> to <kbd>⎈ Ctrl</kbd> + <kbd>Super</kbd> +
<kbd>Hyper</kbd> + <kbd>x</kbd>.

Control mode will recurse into prefix keys' keymaps, for example <kbd>C-x
C-x</kbd> becomes available as <kbd>x C-x</kbd> and <kbd>x x</kbd>.

### Examples

Suppose <kbd>C-f</kbd>, <kbd>M-f</kbd>, and <kbd>C-M-f</kbd> are all bound
to commands but <kbd>f</kbd> is either unbound or just types “f”. Control
mode would create key bindings like so:

<table>
<tr><th>Original binding</th><th>Available in Control Mode as</th></tr>
<tr><td><kbd>C-f</kbd></td><td><kbd>f</kbd></td></tr>
<tr><td><kbd>M-f</kbd></td><td><kbd>M-f</kbd></td></tr>
<tr><td><kbd>C-M-f</kbd></td><td><kbd>C-f</kbd>, <kbd>C-M-f</kbd></td></tr>
</table>

If <kbd>C-%</kbd> isn't bound, but <kbd>M-%</kbd> and <kbd>C-M-%</kbd> are:

<table>
<tr><th>Original binding</th><th>Available in Control Mode as</th></tr>
<tr><td><kbd>M-%</kbd></td><td><kbd>%</kbd></td></tr>
<tr>
  <td><kbd>C-M-%</kbd></td>
  <td><kbd>C-%</kbd>, <kbd>M-%</kbd>, <kbd>C-M-%</kbd></td>
</tr>
</table>

If <kbd>M-n</kbd> isn't bound, but <kbd>C-n</kbd> and <kbd>C-M-n</kbd> are:

<table>
<tr><th>Original binding</th><th>Available in Control Mode as</th></tr>
<tr><td><kbd>C-n</kbd></td><td><kbd>n</kbd></td></tr>
<tr>
  <td><kbd>C-M-n</kbd></td>
  <td><kbd>C-n</kbd>, <kbd>M-n</kbd>, <kbd>C-M-n</kbd></td>
</tr>
</table>

### Regenerating Key Bindings

Control mode generates bindings separately for every combination of major
mode and minor modes, and so will setup different bindings in each buffer as
necessary. It is able to detect when the major mode changes and adapt to
that, but there is no way for Control Mode to know if you have turned on a
new minor mode. If this causes a problem, turn Control Mode off and back on
again.

If you change the key bindings in any of the modes or in your global keymap,
you may have to tell Control Mode to regenerate its key bindings. This can
be done with the `control-mode-reload-bindings` command.

Tips
----

<kbd>C-[</kbd> in Emacs acts like pushing <kbd>⎋ Esc</kbd> or holding down
<kbd>◆ Meta</kbd>. In Control mode <kbd>[</kbd> has this behavior. So for
example, <kbd>[ f</kbd> will do `forward-word`. Also, all the number keys,
<kbd>-</kbd>, and <kbd>u</kbd> are rebound in Control Mode to set arguments
for following commands. So <kbd>3 k</kbd> deletes the last three lines and
<kbd>u [ a</kbd> jumps back four sentences.

<kbd>C-q</kbd>, `quoted-insert`, gives you a way to insert text while in
Control Mode. This becomes <kbd>q</kbd>, so <kbd>q t q e q x q t</kbd> will
enter “text”.

Keyboard macros in Emacs record the actual key presses used while creating
them, and so a keyboard macro created in Control Mode may not work outside
of Control Mode, and vice versa. You can start a keyboard macro with
<kbd>C-0 C-z</kbd> to force it to turn Control Mode off, or <kbd>C-1
C-z</kbd> to force it to turn Control Mode on, to prevent problems with
keyboard macros being executed in the wrong mode.

Customization
-------------

Besides the `control-mode-rebind-to-shift` variable mentioned above, Control
Mode provides a keymap and a hook you can use for customization. You can
create key bindings in `control-mode-keymap` and have them available in
Control Mode. These override any automatically generated key bindings.

You can also use `add-hook` with `control-mode-keymap-generation-functions`
to hook into the keymap generation system. Functions attached to this hook
will be passed a single parameter, a keymap they can define bindings in to
make them available in Control Mode. These functions will be called once for
each combination of major mode and minor modes, and so let you customize
Control Mode based on the other modes or key bindings that are present.

You can use Emacs' customization interface to customize Control Mode:
<kbd>M-x customize-group control-mode RET</kbd>.

License
-------

Copyright © 2013–2015 Stephen Marsh

Distributed under GNU GPL, version 3.
