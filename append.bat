dd.exe if=mtrak.tpl of=mtrak.bin bs=1 count=13764
dd.exe if=output.sms of=mtrak.bin bs=1 count=60 skip=13764 seek=13764
dd.exe if=mtrak.tpl of=mtrak.bin bs=1 count=2560 skip=13824 seek=13824
