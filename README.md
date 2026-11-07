# emacs-bliss

An Emacs porcelain for [bliss](../bliss), which sends the current directory, its
environment and its Guix or Nix shell to podman, a remote tmux or screen session, or
Kubernetes.

`M-x bliss` lists your workloads with their live state. `M-x bliss-dispatch` offers
everything from any buffer.

## Install

Needs Emacs 28.1 or newer, transient, and `bliss` on `PATH` (or set `bliss-command`).

Install it as a package (for Guix, `emacs-bliss` in the noonker channel builds from this
checkout), or put this directory on `load-path`. Then:

```elisp
(use-package bliss
  :commands (bliss bliss-dispatch)
  :bind ("C-c b" . bliss-dispatch))
```

## Running here

Choose the target `here` (`-t` in the run and AI menus, or `t` to make it the default) to
get the same list and keys on this machine, in place. Your real directory and environment
are used, nothing is copied, and `p` opens the directory. See the bliss README.

## Keys in the list

| Key | Does | Key | Does |
|---|---|---|---|
| `RET`, `a` | attach in a terminal | `r` | run a command or shell... |
| `l` | logs (`C-u` or `L` to follow) | `A` | start an AI agent... |
| `p` | pull files back, then open them | `g` | refresh |
| `k` | stop the marked workloads, or this one | `t` | set the default target |
| `m` `u` `U` | mark, unmark, unmark all | `G` | collect garbage, after a preview |
| `D` | open its directory | `$` | output of background commands |
| `w` | copy its name | `?`, `h` | all of the above |

## How it behaves

- **Nothing blocks.** Pack builds can take minutes, so bliss runs in the background and
  its output collects in `*bliss-process*`, which opens if a command fails. Name and
  target prompts are the exception: they wait briefly for the list to complete from.
- **The buffer you start from counts.** Workloads get its `default-directory` and
  `process-environment`, including an envrc-style local one. All of that environment
  travels, secrets included, unless `[env] exclude` in the bliss settings drops it. Use
  **Plan only** (`n`) in the run or AI menu to see the names that would be sent.
- **Terminals.** Attaching, shells and interactive agents use eat, vterm or term,
  whichever is installed first; set `bliss-terminal` to choose. Detaching ends the
  terminal, not the workload: `C-b d` in tmux, `C-a d` in screen, `C-p C-q` in podman.
  `kubectl attach` has no detach keys, so kill the terminal buffer instead.
- **Stopping** asks first, since it removes the workload's files on the target.
- **Remote buffers** are not supported: bliss runs on this machine, so start it from a
  local directory.

## Tests

```sh
make test
make compile
```

Both need transient on the load path. If it needs libraries from your own setup, add them:
`make test LOAD_PATH="-L /path/to/transient -L /path/to/cond-let -L /path/to/llama"`.
The tests use a fake bliss, so they touch no real target.
