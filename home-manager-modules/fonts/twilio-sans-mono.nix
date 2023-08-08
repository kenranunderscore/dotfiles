{ twilio-sans-mono, runCommand, unzip }:

runCommand "twilio-sans-mono" { } ''
  dest=$out/share/fonts
  mkdir -p $dest
  ${unzip}/bin/unzip ${twilio-sans-mono}/Twilio-Sans-Mono.zip -d $dest
  mv $dest/Twilio-Sans-Mono/TTF/*.ttf $dest/
  rm -r $dest/Twilio-Sans-Mono
''
