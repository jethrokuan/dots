{ config, pkgs, ... }:

{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  nixpkgs.config.allowUnfree = true;
  home.file.".config/nixpkgs/config.nix".text = ''
    { allowUnfree = true; }
  '';

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "jethro";
  home.homeDirectory = "/home/jethro";

  home.packages = with pkgs; [
    niv

    # Archiving
    unrar
    unzip
    xz
    zip

    # PDF
    evince

    # System Utils
    flameshot
    htop
    tree
    rsync
    ripgrep
    wget
    xclip
    xdg_utils
    direnv
    fd
    file
    fzf
    sshfs

    # Emacs-related tooling
    proselint
    sqlite
    graphviz

    # Apps
    slack
    tdesktop
    bitwarden
    zotero

    # Media
    vlc

    # Screencasting
    simplescreenrecorder
    scrot
    imagemagick

    # Emacs
    ripgrep
    coreutils
    clang
    texlive.combined.scheme-full
    zotero

    (makeDesktopItem {
      name = "org-protocol";
      exec = "emacsclient %u";
      comment = "Org Protocol";
      desktopName = "org-protocol";
      type = "Application";
      mimeType = "x-scheme-handler/org-protocol";
    })
  ];

  programs.firefox.enable = true;
  programs.direnv.enable = true;
  services.lorri.enable = true;
  services.dropbox.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = false;
    enableFishIntegration = false;
  };

  programs.git = {
    enable = true;
    userName = "Jethro Kuan";
    userEmail = "jethrokuan95@gmail.com";
  };

  programs.bash = {
    enable = true;
    shellAliases = {
      "p2x1" = "pdfnup --nup 2x1 --landscape --suffix '2x1' --batch ";
    };
  };
  
  services.emacs.enable = true;

  targets.genericLinux.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}

