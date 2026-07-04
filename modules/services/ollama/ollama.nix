{
  ...
}:
{
  flake.modules.nixos.ollama =
    { pkgs, ... }:
    {
      services.ollama = {
        enable = true;
        package = pkgs.ollama-rocm;
        environmentVariables = {
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
