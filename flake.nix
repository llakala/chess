{
  description = "My Gleam development setup for the chess competition";

  nixConfig =
  {
    # I don't like it either - but gleam-nix requires it.
    allow-import-from-derivation = true;

    extra-trusted-public-keys = [
      "gleam-nix.cachix.org-1:JFm9l4KxdKyBNjQFxo/SF5SVjBTGvib/D877Zwf8C0s="
    ];
    extra-substituters = [ "https://gleam-nix.cachix.org" ];
  };

  inputs =
  {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    llakaLib =
    {
      url = "github:llakala/llakaLib";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # There may be some flake.lock duplication from duplicate versions of nixpkgs -
    # but this has a cachix, so its inputs need to be the same.
    gleam-nix =
    {
      url = "github:vic/gleam-nix";
    };
  };

  outputs = { self, nixpkgs, ... } @ inputs:
  let
    lib = nixpkgs.lib;

    # This is really only meant for my usage, so I'm not designing it to
    # support other systems
    supportedSystems = [ "x86_64-linux" "aarch64-darwin" ];

    forAllSystems = function: lib.genAttrs
      supportedSystems
      (system: function nixpkgs.legacyPackages.${system});
  in
  {
    legacyPackages = forAllSystems
    (
      # llakaLib has my custom lib functions, specifically `writeFishApplication`
      # for writing scripts
      pkgs: let llakaLib = inputs.llakaLib.fullLib.${pkgs.system};
      in llakaLib.collectDirectoryPackages
      {
        inherit pkgs;
        directory = ./nixPackages;

        # Lets the packages rely on custom functions and newest gleam version
        extras =
        {
          inherit llakaLib;
          gleamPackages = inputs.gleam-nix.packages.${pkgs.system};
        };
      }
    );

    devShells = forAllSystems
    (
      pkgs:
      {
        default = import ./shell.nix
        {
          inherit pkgs;

          # Gleam version built from source - but not actually, thanks to Cachix
          gleam = inputs.gleam-nix.packages.${pkgs.system}.gleam;

          # attrValues turns a list into an attrset
          localPackages = builtins.attrValues self.legacyPackages.${pkgs.system};
        };
      }
    );
  };
}
