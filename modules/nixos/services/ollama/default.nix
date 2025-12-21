{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.services.ollama;
in
{
  options.modules.services.ollama = {
    enable = mkEnableOption "Ollama";
  };

  config = lib.mkIf cfg.enable {
    services.ollama = {
      enable = true;
      package = pkgs.ollama-rocm;
      environmentVariables = {
        # Override the LLVM target by getting the version from your GPU like so
        # nix-shell -p "rocmPackages.rocminfo" --run "rocminfo" | grep "gfx"
        # NOTE 2025-06-12 My RADEON 6700XT is gfx1031, which is not officially
        # supported by AMD in ROCm. See the following link:
        # <https://rocm.docs.amd.com/projects/install-on-linux/en/latest/reference/system-requirements.html>
        HSA_OVERRIDE_GFX_VERSION = "10.3.0";
        HCC_AMDGPU_TARGET = "gfx1031";
      };
      rocmOverrideGfx = "10.3.0";
      loadModels = [
        "deepseek-r1:1.5b"
        "qwen3:1.7b"
        "llama3.2:1b"
        "gemma3:1b"
        "qwen2.5vl:3b"
      ];
    };
  };
}
