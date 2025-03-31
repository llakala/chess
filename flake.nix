{
  description = "My Gleam development setup for the chess competition";

  inputs =
  {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    llakaLib =
    {
      url = "github:llakala/llakaLib";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... } @ inputs:
  let
    lib = nixpkgs.lib;

    # This is really only meant for my usage, so I'm not designing it to
    # support other systems
    supportedSystems = lib.singleton "x86_64-linux";

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

        # Lets the packages rely on custom functions and packages
        extras =
        {
          inherit llakaLib;
          llakaPackages = inputs.llakaLib.packages.${pkgs.system};
        };
      }
    );

    devShells = forAllSystems
    (
      pkgs: let llakaPackages = inputs.llakaLib.packages.${pkgs.system}; in
      {
        default = import ./shell.nix
        {
          # Rely on packages from nixpkgs and my custom ones
          inherit pkgs llakaPackages;

          # attrValues turns a list into an attrset
          extraPackages = builtins.attrValues self.legacyPackages.${pkgs.system};
        };
      }
    );
  };
}
