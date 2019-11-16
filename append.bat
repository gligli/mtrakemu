dd.exe if=mtrak.tpl of=mtrak.bin
dd.exe if=output.sms of=mtrak.bin conv=notrunc bs=1 count=27 skip=564 seek=564
dd.exe if=output.sms of=mtrak.bin conv=notrunc bs=1 count=60 skip=13764 seek=13764
