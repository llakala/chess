{ pkgs, llakaPackages ? {}, extraPackages ? [] }:

let
  myPackages = with llakaPackages;
  [
    gleam
  ];

  # From nixpkgs
  upstreamPackages = with pkgs;
  [
    erlang_27
    rebar3
  ];

in pkgs.mkShellNoCC
{
  packages = myPackages ++ extraPackages ++ upstreamPackages;
}
