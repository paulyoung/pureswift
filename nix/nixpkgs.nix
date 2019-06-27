let rev = "ae02678a9d93b1d8bc1ed6ce357af9b71bb89e2a"; in
let sha256 = "0xcy1fwbzr8rpwrpp79dlpms3bw43d251r553fvkvfqli7skppl4"; in

let src = builtins.fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}; in

{
  inherit rev src;
  nixpkgs = import src;
}
