About
=====
This is a utility to write Nix expressions for Jenkins plugins.
Jenkins4nix parses https://updates.jenkins-ci.org/download/plugins,
searches for requested plugins and their dependencies,
writes Nix expression to stdout:
```nix
{
  "plugin-name" = {
    version = "1.2.3";
    sha1 = "...";
    depends = [  ];
  };
  ...
}
```

Requirements
============
Jenkins4nix is written in Haskell with [GHC](http://www.haskell.org/ghc/).
All required Haskell libraries are listed in [jenkins4nix.cabal](jenkins4nix.cabal).
Use [cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install)
to fetch and build all pre-requisites automatically.

Installation
============
    git clone https://github.com/zalora/jenkins4nix.git
    cd jenkins4nix
    cabal install


Usage
=====

    # jenkins4nix s3 mercurial > plugins.nix
    fetching https://updates.jenkins-ci.org/download/plugins/s3/0.7/s3.hpi ...
    fetching https://updates.jenkins-ci.org/download/plugins/mercurial/1.52/mercurial.hpi ...
    fetching https://updates.jenkins-ci.org/download/plugins/copyartifact/1.35/copyartifact.hpi ...
    fetching https://updates.jenkins-ci.org/download/plugins/credentials/1.22/credentials.hpi ...
    fetching https://updates.jenkins-ci.org/download/plugins/matrix-project/1.4/matrix-project.hpi ...
    fetching https://updates.jenkins-ci.org/download/plugins/multiple-scms/0.4/multiple-scms.hpi ...
    fetching https://updates.jenkins-ci.org/download/plugins/scm-api/0.2/scm-api.hpi ...
    fetching https://updates.jenkins-ci.org/download/plugins/ssh-credentials/1.11/ssh-credentials.hpi ...

The result's in `plugins.nix`, it's up to you what to do with it:

```nix
{
  "copyartifact" = {
    version = "1.35";
    sha1 = "a39c54502ff9441144ae45da03bffbc36d20bfca";
    depends = [ "matrix-project" ];
  };
  "credentials" = {
    version = "1.22";
    sha1 = "15fe1d3947a7c64b67f653ba0aa48413fdb7304e";
    depends = [  ];
  };
  "matrix-project" = {
    version = "1.4";
    sha1 = "bf14b558cf731e6078dd9d226fe197f13d3d6924";
    depends = [  ];
  };
  "mercurial" = {
    version = "1.52";
    sha1 = "6b87bc879a75e219dd691a1f7f53485871ba6019";
    depends = [ "credentials" "scm-api" "ssh-credentials" "matrix-project" "multiple-scms" ];
  };
  "multiple-scms" = {
    version = "0.4";
    sha1 = "e235b33ea49aee968c69720c9d65ff1d668292c8";
    depends = [  ];
  };
  "s3" = {
    version = "0.7";
    sha1 = "01ddd81e5dba67785351da32564d8a29eacb001d";
    depends = [ "copyartifact" ];
  };
  "scm-api" = {
    version = "0.2";
    sha1 = "cc98487e2daaf7484a2028f62828bf6f9ef986ce";
    depends = [  ];
  };
  "ssh-credentials" = {
    version = "1.11";
    sha1 = "d47e6a2899ee75e48336f6d2637da4e9ba0e3d21";
    depends = [ "credentials" ];
  };
}
```
