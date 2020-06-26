xrandr --output DVI-D-1 --left-of DVI-I-1 \
& xinput set-prop 13 294 {-1,1,1} \
& xkbcomp ~/dhm.xkb $DISPLAY \
& ~/.fehbg \
& exec i3
