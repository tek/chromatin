{
  description = "Neovim Package Manager for Haskell Plugins";

  inputs = {
    ribosome.url = github:tek/ribosome;
    crm-test.url = github:tek/crm-test;
  };

  outputs = { ribosome, crm-test, ... }:
  let
    inherit (ribosome.inputs) chiasma hix;
    overrides = { hackage, source, minimal, configure, pkgs, ... }: {
      cornea = hackage "0.4.0.0" "1w9rkf6f861kknkskywb8fczlk7az8m56i3hvmg6a5inpvqf6p7i";
      chiasma = source.package chiasma "chiasma";
      chromatin-test = drv: drv.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [pkgs.neovim pkgs.tmux pkgs.git pkgs.rxvt-unicode];
      });
      ribosome = configure "--extra-prog-path=${pkgs.neovim}/bin" (
        minimal (source.package ribosome "ribosome"));
      ribosome-test = minimal (source.package ribosome "ribosome-test");
    };

  in hix.flake {
    base = ./.;
    inherit overrides;
    compat = false;
    packages = {
      chromatin = ./packages/chromatin;
      chromatin-test = ./packages/chromatin-test;
    };
    main = "chromatin";
    versionFile = "ops/hpack/packages/chromatin.yaml";
    runConfig = p: { extraShellInputs = [p.pkgs.neovim]; };
    modify = _: outputs: rec {
      apps.chromatin = {
        type = "app";
        program = "${outputs.packages.chromatin}/bin/chromatin";
      };
      apps.default = apps.chromatin;
    };
  };
}
