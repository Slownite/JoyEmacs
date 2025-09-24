# JoyEmacs

[![Built with Nix](https://img.shields.io/badge/built%20with-Nix-5277C3?logo=nixos\&logoColor=white)](https://nixos.org)
[![Emacs](https://img.shields.io/badge/Emacs-29+-7F5AB6?logo=gnu-emacs\&logoColor=white)](https://www.gnu.org/software/emacs/)
[![License: MIT](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)

🚀 A reproducible, standalone Emacs distribution built with **Nix**, configured in **Elisp**.
Think *nixvim* for Emacs: install with one command, edit `~/.config/joyemacs/init.el`, and you’re good to go.

---

## ✨ Features

* **Nix-built Emacs** → reproducible, portable, always the same environment.
* **Zero setup** → `nix run` just works.
* **Declarative config in Elisp** → you own `~/.config/joyemacs/init.el`.
* Prebundled with useful packages:

  * [evil](https://github.com/emacs-evil/evil) (Vim keybindings)
  * [vertico](https://github.com/minad/vertico), [orderless](https://github.com/oantolin/orderless), [marginalia](https://github.com/minad/marginalia), [consult](https://github.com/minad/consult), [embark](https://github.com/oantolin/embark)
  * [which-key](https://github.com/justbur/emacs-which-key)
  * [doom-themes](https://github.com/hlissner/emacs-doom-themes) (default: `doom-nord`)
  * [magit](https://magit.vc/)

---

## 🛠 Installation

### Quick run

```bash
nix run github:Slownite/JoyEmacs
```

If it’s your first time, you’ll see:

```
JoyEmacs: No ~/.config/joyemacs/init.el found.
Run: nix run github:Slownite/JoyEmacs#install
```

---

### Install starter config

```bash
nix run github:Slownite/JoyEmacs#install
nix run github:Slownite/JoyEmacs
```

This creates `~/.config/joyemacs/init.el` with a starter setup you can extend.

---

### Add to your profile (persistent command)

```bash
nix profile install github:Slownite/JoyEmacs#joyemacs
joyemacs
```

---

### Multiple JoyEmacs configs

Set `JOYEMACS_HOME` to use a different config location:

```bash
JOYEMACS_HOME=~/work/emacs-test nix run github:Slownite/JoyEmacs#install
JOYEMACS_HOME=~/work/emacs-test nix run github:Slownite/JoyEmacs
```

---

## 📂 Config layout

By default, JoyEmacs looks in `~/.config/joyemacs/`:

```
~/.config/joyemacs/
├── init.el    # your main Emacs config
└── lisp/      # optional directory for extra .el files
```

---

## 🧩 Example Elisp config

```elisp
;; ~/.config/joyemacs/init.el

(setq inhibit-startup-message t)

(use-package doom-themes
  :config (load-theme 'doom-nord t))

(use-package evil
  :config (evil-mode 1))

(use-package magit)
```

---

## 🔧 Development

Enter a dev shell with Emacs and tools:

```bash
nix develop
```

---

## 📜 License

MIT — do whatever brings you Joy ✨

