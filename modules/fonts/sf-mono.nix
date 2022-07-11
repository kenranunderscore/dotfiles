{ sf-mono, runCommand }:

runCommand "sf-mono" { } ''
  mkdir -p $out/share/fonts
  cp ${sf-mono}/*.otf $out/share/fonts
''
