# Nix configuration for testing hr against all supported GHC versions
#
# Usage:
#
#     $ nix-build test-all.nix

{
  hr-ghc-822 = import ./default.nix { compiler = "ghc822"; };
  hr-ghc-844 = import ./default.nix { compiler = "ghc844"; };
  hr-ghc-865 = import ./default.nix { compiler = "ghc865"; };
  hr-ghc-884 = import ./default.nix { compiler = "ghc884"; };
  hr-ghc-8104 = import ./default.nix { compiler = "ghc8104"; };
  hr-ghc-901 = import ./default.nix { compiler = "ghc901"; };
}
