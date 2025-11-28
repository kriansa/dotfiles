# My font setup

- Roboto - Replaces Helvetica, Helvetica Neue
- Roboto Condensed - Used on GTK interface
- Roboto Mono - Replaces Courier, monospace
- Arimo - Replaces Arial, sans-serif
- Tinos - Replaces Times, serif
- Gelasio - Replaces Georgia
- Comic Neue - Replaces Comic Sans
- Iosevka Term - Used on terminal

## Extra

Font hinting and antialiasing is typically provided by the compositor, so these shouldn't be
necessary. In case weirdfont rendering happens, add the following rules to fontconfig as well.

```xml
 <match target="font">
  <edit mode="assign" name="antialias"><bool>true</bool></edit>
 </match>
 <match target="font">
  <edit mode="assign" name="rgba"><const>rgb</const></edit>
 </match>
 <match target="font">
  <edit mode="assign" name="hinting"><bool>true</bool></edit>
 </match>
 <match target="font">
  <edit mode="assign" name="hintstyle"><const>hintslight</const></edit>
 </match>
```
