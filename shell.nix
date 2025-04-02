{ gleam, pkgs, localPackages ? [] }:

let
  # From nixpkgs
  upstreamPackages = with pkgs;
  [
    erlang_27
    rebar3
  ];

in pkgs.mkShellNoCC
{
  # Use the passed version of Gleam: since I build it from unstable
  packages = [ gleam ] ++ localPackages ++ upstreamPackages;
}
