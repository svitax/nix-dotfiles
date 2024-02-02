{ lib, stdenv, fetchFromGitHub, bash, }:
stdenv.mkDerivation {
  name = "zellij-smart-sessionizer";
  version = "unstable-2023-11-15";
  src = fetchFromGitHub {
    owner = "demestoss";
    repo = "zellij-smart-sessionizer";
    rev = "32f476e882d3becc59a799da400fa6d111cc612f";
    hash = "sha256-0GmCyhQ9eBJuRPau76r1prYWmY5piKStOLteMaCv40w=";
  };
  buildInputs = [ bash ];
  installPhase = ''
    # Make the output directory
    mkdir -p $out/bin

    # Copy the script there and make it executable
    cp zellij-smart-sessionizer $out/bin/zellij-smart-sessionizer
    chmod +x $out/bin/zellij-smart-sessionizer
  '';
}
