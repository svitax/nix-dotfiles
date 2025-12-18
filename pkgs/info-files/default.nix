{ pkgs, ... }:
pkgs.stdenv.mkDerivation {
  name = "info-files";
  src = ./.;

  nativeBuildInputs = [ pkgs.texinfo ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/info
    cp *.info $out/share/info

    cd $out/share/info
    for file in *.info; do
        install-info "$file" dir
    done

    runHook postInstall
  '';
}
