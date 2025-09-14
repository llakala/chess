{ pkgs, localPackages ? [] }:

let
  # From nixpkgs
  upstreamPackages = with pkgs;
  [
    erlang_27
    rebar3
    elixir
    beamPackages.hex
    gleam
  ];

in pkgs.mkShellNoCC
{
  packages = localPackages ++ upstreamPackages;
}
