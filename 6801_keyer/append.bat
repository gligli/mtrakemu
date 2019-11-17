wla-6801 sdump.a01
echo [objects]>linkfile
echo sdump.o>>linkfile
wlalink.exe -s -r -v linkfile sdump.bin
bin2srec -s -o -0x8000 sdump.bin > sdump.s19
copy /b sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin+sdump.bin sdump512.bin