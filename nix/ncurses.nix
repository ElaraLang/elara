# ncurses includes paths for where to look for `terminfo`, which by default includes a nix store path.
# this doesn't break anything, but is undesirable when static linking, because we're now searching the nix store unnecessarily
# in particular this means that when doing `nix copy` or `nix profile install` etc, we'd end up also bundling the nix-packaged
# terminfo despite it never being used. This ends up adding about 9MB to the closure size: quite big!
# so instead we override the terminfo directories ncurses looks in for the usual ones, which hopefully won't have any repercussions :clueless:
{ pkgs }:
(pkgs.ncurses.override { enableStatic = true; }).overrideAttrs (old: {
  configureFlags = old.configureFlags ++ [
    "--with-terminfo-dirs=/etc/terminfo:/lib/terminfo:/usr/share/terminfo"
    "--with-default-terminfo-dir=/usr/share/terminfo"
    "--disable-db-install"
  ];
})
