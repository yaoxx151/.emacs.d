# Emacs configuration

`myinit.org` is the source of truth. On startup, Emacs retangles it only when
it is newer than the ignored generated file, `myinit.el`.

The configuration targets Emacs 30 and expects `rg`, `hunspell`, `python3`,
`black`, and `isort` on `PATH`. An arm64 build with native compilation is
recommended on Apple Silicon.

```sh
mv ~/.emacs.d ~/.emacs.d.bak
git clone https://github.com/yaoxx151/.emacs.d.git ~/.emacs.d
```

The first startup installs missing packages from GNU ELPA, NonGNU ELPA, and
MELPA. After changing packages, run `M-x package-quickstart-refresh`.

Machine-local configuration belongs in ignored `work.el`. Store API keys in
`auth-source` (for example, encrypted `~/.authinfo.gpg`) rather than in the
repository. The legacy `keys/claude.txt` fallback remains supported.
