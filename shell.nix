{ pkgs, extraPackages ? [] }:

# Basic gleam development stuff. In an ideal world I would have hex
# packages here, but Nix doesn't have much gleam stuff right now, so
# we leave those to be installed on the Gleam side.
let gleamPackages = with pkgs;
[
  gleam
  erlang_27
  rebar3
];

in pkgs.mkShellNoCC
{
  packages = extraPackages ++ gleamPackages;
}
