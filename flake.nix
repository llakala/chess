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

        # Lets the packages rely on llakaLib functions
        extras = { inherit llakaLib; };
      }
    );

    devShells = forAllSystems
    (
      pkgs:
      {
        default = import ./shell.nix
        {
          inherit pkgs;

          # attrValues turns a list into an attrset
          extraPackages = builtins.attrValues self.legacyPackages.${pkgs.system};
        };
      }
    );
  };
}
