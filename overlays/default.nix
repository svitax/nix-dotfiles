{
  inputs,
  outputs,
  ...
}: {
  # This brings our custom packages from the 'packages' directory.
  additions = final: _prev:
    import ../packages {pkgs = final;};
}
