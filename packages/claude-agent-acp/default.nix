{
  lib,
  buildNpmPackage,
  fetchurl,
}:

buildNpmPackage rec {
  pname = "claude-agent-acp";
  version = "0.39.0";

  src = fetchurl {
    url = "https://registry.npmjs.org/@agentclientprotocol/claude-agent-acp/-/claude-agent-acp-${version}.tgz";
    hash = "sha256-L0PO3myte4oBsnyn3uaSVVwnVvyVvorxa1DcYLknX3U=";
  };

  sourceRoot = "package";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-pfQ/8+NxkXcd7WJ25qJPht3wgRLzoLwVCmfQ8OQcenk=";

  dontNpmBuild = true;

  meta = {
    description = "Claude agent adapter for the Agent Client Protocol";
    homepage = "https://github.com/agentclientprotocol/claude-agent-acp";
    license = lib.licenses.mit;
    mainProgram = "claude-agent-acp";
  };
}
