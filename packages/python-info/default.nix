{
  lib,
  stdenvNoCC,
  texinfo,
  writeText,
  pythonInterpreters,
}:
let
  # docutils 0.22 breaks CPython's docs build (cpython#139257); pin 0.21.2.
  withFixedDocutils =
    python:
    python.override {
      packageOverrides = _: prev: {
        docutils = prev.docutils.overridePythonAttrs (_: rec {
          version = "0.21.2";
          src = prev.fetchPypi {
            pname = "docutils";
            inherit version;
            hash = "sha256-OmsYcy7fGC2qPNEndbuzOM9WkUaPke7rEJ3v9uv6mG8=";
          };
          doCheck = false;
        });
        sphinx = prev.sphinx.overridePythonAttrs (_: { doCheck = false; });
        sphinxext-opengraph = prev.sphinxext-opengraph.overridePythonAttrs (_: { doCheck = false; });
        sphinx-notfound-page = prev.sphinx-notfound-page.overridePythonAttrs (_: { doCheck = false; });
        python-docs-theme = prev.python-docs-theme.overridePythonAttrs (_: { doCheck = false; });
      };
    };

  isStableCPython =
    name: py:
    (builtins.match "python3[0-9]+" name != null)
    && ((py.implementation or "") == "cpython")
    && (py ? src)
    && (builtins.match "[0-9]+\\.[0-9]+\\.[0-9]+" (py.version or "") != null);

  candidates = lib.filterAttrs (
    name: py:
    let
      r = builtins.tryEval (isStableCPython name py);
    in
    r.success && r.value
  ) pythonInterpreters;

  mkVersionInfo =
    basePy:
    let
      py = withFixedDocutils basePy;
      mm = lib.versions.majorMinor py.version;
      docEnv = py.withPackages (ps: [
        ps.sphinx
        ps.sphinxext-opengraph
        ps.sphinx-notfound-page
        ps.python-docs-theme
      ]);
    in
    stdenvNoCC.mkDerivation {
      pname = "python-info-${mm}";
      version = py.version;
      src = py.src;

      nativeBuildInputs = [
        docEnv
        texinfo
      ];

      dontConfigure = true;

      buildPhase = ''
        runHook preBuild
        export HOME="$TMPDIR"
        cd Doc
        sphinx-build -b texinfo -d build/doctrees . build/texinfo
        substituteInPlace build/texinfo/python.texi \
          --replace-fail '@setfilename python.info' '@setfilename python-${mm}.info'
        makeinfo --no-split build/texinfo/python.texi -o build/texinfo/python-${mm}.info
        runHook postBuild
      '';

      installPhase = ''
        runHook preInstall
        mkdir -p "$out/share/info"
        cp build/texinfo/python-${mm}.info "$out/share/info/"
        runHook postInstall
      '';

      meta = {
        description = "Python ${mm} standard-library documentation (GNU Info)";
        homepage = "https://docs.python.org/${mm}/";
        license = lib.licenses.psfl;
        platforms = lib.platforms.all;
      };
    };

  discovered = lib.mapAttrsToList (_: py: {
    inherit py;
    mm = lib.versions.majorMinor py.version;
  }) candidates;

  buildable = builtins.filter (v: (builtins.tryEval (mkVersionInfo v.py).drvPath).success) discovered;
  versions = lib.sort (a: b: lib.versionOlder a.mm b.mm) buildable;

  skippedMMs = lib.subtractLists (map (v: v.mm) versions) (map (v: v.mm) discovered);

  umbrellaTexi = writeText "python-umbrella.texi" (
    lib.concatStringsSep "\n" (
      [
        "\\input texinfo"
        "@setfilename python.info"
        "@settitle Python Documentation"
        "@dircategory Python"
        "@direntry"
        "* Python: (python).  Python standard-library docs (all installed versions)."
        "@end direntry"
        "@node Top"
        "@top Python Documentation"
        ""
        "Select a Python version:"
        ""
        "@menu"
      ]
      ++ (map (v: "* ${v.mm}: (python-${v.mm})Top.  Python ${v.mm} standard library") versions)
      ++ [
        "@end menu"
        "@bye"
        ""
      ]
    )
  );
in
lib.warnIf (skippedMMs != [ ])
  "python-info: skipping CPython ${lib.concatStringsSep ", " skippedMMs} (docs toolchain unavailable in this nixpkgs)"
  (stdenvNoCC.mkDerivation {
    pname = "python-info";
    version = if versions == [ ] then "0" else (lib.last versions).mm;

    dontUnpack = true;
    dontConfigure = true;
    dontBuild = true;

    nativeBuildInputs = [ texinfo ];

    installPhase = ''
      runHook preInstall
      mkdir -p "$out/share/info"
      ${lib.concatMapStringsSep "\n" (
        v: ''cp ${mkVersionInfo v.py}/share/info/python-${v.mm}.info "$out/share/info/"''
      ) versions}
      makeinfo --no-split ${umbrellaTexi} -o "$out/share/info/python.info"
      install-info "$out/share/info/python.info" "$out/share/info/dir"
      runHook postInstall
    '';

    passthru.perVersion = builtins.listToAttrs (
      map (v: {
        name = v.mm;
        value = mkVersionInfo v.py;
      }) versions
    );

    meta = {
      description = "Python standard-library docs (GNU Info), one manual per non-EOL CPython minor, under a single `info python` menu";
      homepage = "https://docs.python.org/3/";
      license = lib.licenses.psfl;
      platforms = lib.platforms.all;
    };
  })
