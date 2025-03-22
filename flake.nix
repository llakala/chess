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

    # Basic gleam development stuff. In an ideal world I would have hex
    # packages here, but Nix doesn't have much gleam stuff right now, so
    # we leave those to be installed on the Gleam side.
    gleamPackages = forAllSystems
    (
      pkgs:
      {
        inherit (pkgs)
          gleam
          erlang_27
          rebar3;
      }
    );

    devShells = forAllSystems
    (
      pkgs:
      {
        default = pkgs.mkShellNoCC
        {
          # makes an attrset into a list
          # // isn't associative, so be careful to not clobber packages with the same name!
          packages = builtins.attrValues
          (
            self.gleamPackages.${pkgs.system}
            // self.legacyPackages.${pkgs.system}
          );
        };
      }
    );
  };
}
