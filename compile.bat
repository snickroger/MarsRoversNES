@C:\cc65\bin\ca65 marsrovers.asm -g -o marsrovers.o
@C:\cc65\bin\ld65 -C nrom.cfg -o marsrovers.nes marsrovers.o -m marsrovers_map.txt -Ln marsrovers_labels.txt