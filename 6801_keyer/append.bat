wla-6801 sdump.a01
echo [objects]>linkfile
echo sdump.o>>linkfile
wlalink.exe -r -v linkfile sdump.bin
