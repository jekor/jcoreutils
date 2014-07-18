{ buildLocalCabal ? (import <nixpkgs> {}).haskellPackages.buildLocalCabal
, src ? ./.
}:

{
  build = buildLocalCabal src "jcoreutils";
}
