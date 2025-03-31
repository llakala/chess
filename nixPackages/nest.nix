{ llakaLib, llakaPackages }:

llakaLib.writeFishApplication
{
  # Gleam testing with Birdie integration, so if the tests fail, we
  # go straight into Birdie to approve any new snapshot tests
  name = "nest";

  # Rely  on the unstable version of Gleam. I'd like to eventually have Gleam stuff
  # packaged in Nix, but I don't think the ecosystem is there yet
  runtimeInputs = with llakaPackages;
  [
    gleam
  ];

  text =
  /* gleam */
  ''
    gleam test || gleam run -m birdie
  '';
}
