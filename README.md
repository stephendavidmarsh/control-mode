Control mode (control-mode)
============

Control mode is a minor mode for Emacs that provides a "control" mode, similar in purpose to vim's "normal" mode. Unlike the various vim emulation modes, the key bindings in Control mode are derived from the key bindings already setup, usually by making the control key unnecessary, e.g. `Ctrl-f` becomes `f`. This provides the power of a mode dedicated to controlling the editor without needing to learn or maintain new key bindings.

Installation
------------

TODO

Once you have the control-mode package installed, you can add the following line to your `.emacs`:

    (control-mode-default-setup)

This will setup `Ctrl-z` to turn on Control mode globally, and `Ctrl-z` and `z` to turn it off globally. If you prefer to use it on a buffer by buffer basis, use `control-mode-localized-setup`. If you need the usual binding for `Ctrl-z` to suspend Emacs, you can use `Ctrl-x Ctrl-z` instead (`x Ctrl-z` in Control mode). It also binds `x f` to `find-file` (or whatever you had bound to `Ctrl-x Ctrl-f`) in Control mode if it would otherwise be bound to `set-fill-column`.

What it does
------------

Control mode looks at every key binding you already have defined. For each binding that includes `Ctrl-`, it tries to rebind it without `Ctrl-`. It will only do this if the key binding it is replacing is unbound or bound to `self-insert-command`, the Emacs command for keys that simply enter themselves. It will also look at all bindings with `Meta-` (`Alt` on most keyboards), and try to rebind those without the `Meta-`. `Ctrl-` bindings take precedence over `Meta-` bindings.

An exception is that it will ignore the binding for `Ctrl-m`, allowing the binding for `Meta-m` to be bound to `m`.

`Ctrl-Meta-` combinations also get rebound. `Ctrl-Meta-` will get bound to `Ctrl-` if `Ctrl-` was unbound or rebound, and to `Meta-` if `Meta-` was unbound or rebound. If you set the variable `control-mode-rebind-to-shift` to `t` Control mode will also try to rebind to `Shift-` if that binding wouldn't already be taken over by a `Ctrl-Shift-` or `Meta-Shift-` binding. This may interfere with the use of `Shift` with movement commands to select a region however, and so is off by default.

Control mode does the right thing when a key binding includes modifiers other than `Ctrl` and `Meta`. For example, it will rebind `Ctrl-Shift-Backspace` to `Shift-Backspace` if `Shift-Backspace` has a key binding it is allowed to replace, and it will try to rebind `Ctrl-Meta-Super-Hyper-x` to `Ctrl-Super-Hyper-x` and `Meta-Super-Hyper-x`.

Control mode will recurse into prefix keys' keymaps, for example `Ctrl-x Ctrl-x` becomes available as `x Ctrl-x` and `x x`.

###Examples

Suppose `Ctrl-f`, `Meta-f`, and `Ctrl-Meta-f` are all bound to commands but `f` is either unbound or just types `f`. Control mode would create key bindings like so:

<table>
<tr><th>Original binding</th><th>Available in Control mode as</th></tr>
<tr><td>Ctrl-f</td><td>f</td></tr>
<tr><td>Meta-f</td><td>Meta-f</td></tr>
<tr><td>Ctrl-Meta-f</td><td>Ctrl-f, Ctrl-Meta-f</td></tr>
</table>

If `Ctrl-%` isn't bound, but `Meta-%` and `Ctrl-Meta-%` are:

<table>
<tr><th>Original binding</th><th>Available in Control mode as</th></tr>
<tr><td>Meta-%</td><td>%</td></tr>
<tr><td>Ctrl-Meta-%</td><td>Ctrl-%, Meta-%, Ctrl-Meta-%</td></tr>
</table>

If `Meta-n` isn't bound, but `Ctrl-n` and `Ctrl-Meta-n` are:

<table>
<tr><th>Original binding</th><th>Available in Control mode as</th></tr>
<tr><td>Ctrl-n</td><td>n</td></tr>
<tr><td>Ctrl-Meta-n</td><td>Ctrl-n, Meta-n, Ctrl-Meta-n</td></tr>
</table>

###Regenerating Key Bindings

Control mode generates bindings separately for every combination of major mode and minor modes, and so will setup different bindings in each buffer as necessary. It is able to detect when the major mode changes and adapt to that, but there is no way for Control mode to know if you have turned on a new minor mode. If this causes a problem, turn Control mode off and back on again.

If you change the key bindings in any of the modes or in your global keymap, you may have to tell Control mode to regenerate its key bindings. This can be done with the `control-mode-reload-bindings` command.

Tips
----

`Ctrl-[` in Emacs acts like pushing `Escape` or holding down `Meta`. In Control mode `[` has this behavior. So for example, `[f` will do forward-word. Also, all the number keys, `-`, and `u` are rebound in Control mode to set arguments for following commands. So `3-k` deletes the last three lines and `u[a` jumps back four sentences.

`Ctrl-q`, `quoted-insert`, gives you a way to insert text while in Control mode. This becomes `q`, so `qtqeqxqt` will enter `text`.

Keyboard macros in Emacs record the actual key presses used while creating them, and so a keyboard macro created in command mode may not work outside of command mode, and vice versa. You can start a keyboard macro with `Ctrl-0 Ctrl-z` to force it to turn command mode off, or `Ctrl-1 Ctrl-z` to force it to turn command mode on, to prevent problems with keyboard macros being executed in the wrong mode.

Customization
-------------

Besides the `control-mode-rebind-to-shift` variable mentioned above, Control mode provides a keymap and a hook you can use for customization. You can create key bindings in `control-mode-keymap` and have them available in Control mode. These override any automatically generated key bindings. You can also use `add-hook` with `control-mode-keymap-generation-functions` to hook into the keymap generation system. Functions attached to this hook will be passed a single parameter, a keymap they can define bindings in to make them available in Control mode. These functions will be called once for each combination of major mode and minor modes, and so let you customize Control mode based on the other modes or key bindings that are present.
